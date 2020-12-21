{-# LANGUAGE FlexibleContexts #-}
-- We need these to write a ConvertibleStrings instance for
-- ShortByteString
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HaScheme.Eval where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.String
import Data.String.Conversions
import qualified Data.Text as T
import HaScheme.Ast (SchemeError (..), SchemeVal (..))
import LLVM.AST (Operand)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Name
import qualified LLVM.AST.Type as AST
import LLVM.AST.Typed (typeOf)
import qualified LLVM.IRBuilder.Constant as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import LLVM.Prelude (ShortByteString)

data Env = Env
  { variables :: [T.Text],
    functions :: [T.Text] -- Add more data to function table
  }

primitives :: M.Map T.Text Primitive
primitives =
  M.fromList
    [ ("+", Add),
      ("-", Sub),
      ("*", Mult),
      ("/", Div),
      ("&&", And),
      ("||", Or),
      (">", Gt),
      ("<", Lt),
      (">=", Ge),
      ("<=", Le),
      ("=", Eq),
      ("/=", Ne)
    ]

type EvalState = ExceptT SchemeError (State Env)

eval :: SchemeVal -> EvalState EvalAst
-- literals
eval (Number num) = pure (NumConst num)
eval (Bool bool) = pure (BoolConst bool)
eval (String str) = pure (StrConst str)
-- variables
eval (Atom id) = getVar id
eval (List atoms) =
  case atoms of
    [Atom "quote", val] -> eval val
    [Atom "if", cond, ifTrue, ifFalse] -> do
      _cond <- eval cond
      _ifTrue <- eval ifTrue
      _ifFalse <- eval ifFalse
      return $ IfCall _cond _ifTrue _ifFalse
    [Atom "set!", Atom name, value] -> setVar name value
    [Atom "define", Atom name, value] -> defineVar name value
    (Atom "define" : List (Atom name : params) : body) -> setFunction name params body
    (Atom "define" : _) -> throwError $ Default "Illegal define."
    (Atom "lambda" : List params : body) -> makeLambda params body
    (Atom function : args) -> do
      -- first check for primitive function
      let prim = M.lookup function primitives
      args <- evalList args
      case prim of
        Just prim -> return $ PrimitiveCall prim args
        Nothing -> do
          functions <- gets functions
          unless (function `elem` functions) $ throwError $ UnboundFunction function
          return $ FunctionCall function args
    _ -> undefined
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalProgram :: [SchemeVal] -> Either SchemeError [EvalAst]
evalProgram program = evalState (runExceptT (evalList program)) baseEnv
  where
    baseEnv = Env {functions = [], variables = []}

evalList :: [SchemeVal] -> EvalState [EvalAst]
evalList = mapM eval

data EvalAst
  = Var T.Text
  | NumConst Integer
  | BoolConst Bool
  | StrConst T.Text
  | VariableIdentifier T.Text
  | VariableSet T.Text EvalAst
  | VariableDefinition T.Text EvalAst
  | FunctionDefinition T.Text [T.Text] [EvalAst]
  | Lambda [T.Text] [EvalAst]
  | FunctionArg T.Text
  | PrimitiveCall Primitive [EvalAst]
  | FunctionCall T.Text [EvalAst]
  | IfCall EvalAst EvalAst EvalAst
  | EList [EvalAst]
  deriving (Show)

data Primitive
  = -- arithmetic
    Add
  | Sub
  | Mult
  | Div
  | -- Boolean
    And
  | Or
  | Gt
  | Lt
  | Ge
  | Le
  | Eq
  | Ne
  deriving (Show)

makeLambda :: [SchemeVal] -> [SchemeVal] -> EvalState EvalAst
makeLambda args body = do
  variables <- gets variables
  let params = evalFArgs args
  case params of
    Right params -> do
      modify $ \env -> env {variables = variables ++ params}
      body <- mapM eval body
      return $ Lambda params body
    Left err -> throwError err

getFunction :: T.Text -> [EvalAst] -> EvalState EvalAst
getFunction name params = do
  functions <- gets functions
  if name `elem` functions
    then return $ FunctionCall name params
    else throwError $ UnboundFunction name

setFunction :: T.Text -> [SchemeVal] -> [SchemeVal] -> EvalState EvalAst
setFunction name args body = do
  functions <- gets functions
  if name `elem` functions
    then throwError $ FunctionRedefinition name
    else do
      -- add function to symbol table
      let params = evalFArgs args
      case params of
        Right params -> do
          variables <- gets variables
          modify $ \env -> env {functions = name : functions, variables = params ++ variables}
          evaluatedBody <- mapM eval body
          return $ FunctionDefinition name params evaluatedBody
        Left err -> throwError err

evalFArgs :: [SchemeVal] -> Either SchemeError [T.Text]
evalFArgs args = case args of
  [] -> return []
  (Atom arg1 : rest) -> do
    rest <- evalFArgs rest
    return $ arg1 : rest
  (_ : _) -> throwError $ Default "Invalid function call: Invalid arguments"

getVar :: T.Text -> EvalState EvalAst
getVar var = do
  variables <- gets variables
  if var `elem` variables
    then return (VariableIdentifier var)
    else throwError $ UnboundVariable var

setVar :: T.Text -> SchemeVal -> EvalState EvalAst
setVar var val = do
  variables <- gets variables
  if var `elem` variables
    then do
      value <- eval val
      return $ VariableSet var value
    else throwError $ UnboundVariable var

defineVar :: T.Text -> SchemeVal -> EvalState EvalAst
defineVar var val = do
  variables <- gets variables
  if var `elem` variables
    then throwError $ VariableRedefinition var
    else do
      -- add variable to symbol table
      modify $ \env -> env {variables = var : variables}
      value <- eval val
      return $ VariableDefinition var value

-- Code generation

newtype GenState = GenState
  { operands :: M.Map T.Text Operand
  }

registerOperand :: MonadState GenState m => T.Text -> Operand -> m ()
registerOperand name op =
  modify $ \env -> env {operands = M.insert name op (operands env)}

type LLVM = L.ModuleBuilderT (State GenState)

type Codegen = L.IRBuilderT LLVM

-- | Prevents from polluting the global environment with local variable names
locally :: MonadState s m => m a -> m a
locally computation = do
  oldState <- get
  result <- computation
  put oldState
  return result

codegenExpr :: EvalAst -> Codegen Operand
codegenExpr expr = case expr of
  NumConst num -> pure $ L.int32 num
  BoolConst b -> pure $ L.bit (if b then 1 else 0)
  VariableIdentifier id -> do
    var <- gets ((M.! id) . operands)
    L.load var 0
  PrimitiveCall prim (x : xs) -> do
    x <- codegenExpr x
    xs <- mapM codegenExpr xs
    case prim of
      Add -> foldM L.add x xs
      Mult -> foldM L.mul x xs
      Sub -> foldM L.sub x xs
      Div -> foldM L.sdiv x xs
      And -> foldM L.and x xs
      Or -> foldM L.or x xs
      Eq -> foldM (L.icmp IP.EQ) x xs
      Ne -> foldM (L.icmp IP.NE) x xs
      -- TODO: Fix for multiple values
      Lt -> foldM (L.icmp IP.SLT) x xs
      Gt -> foldM (L.icmp IP.SGT) x xs
      Le -> foldM (L.icmp IP.SLE) x xs
      Ge -> foldM (L.icmp IP.SGE) x xs
  VariableSet varName newValue -> do
    var <- gets ((M.! varName) . operands)
    L.store var 0 =<< codegenExpr newValue
    return var
  _ -> undefined

codegenVariableDef :: T.Text -> EvalAst -> LLVM ()
codegenVariableDef name value = do
  let _name = mkName $ cs name
      val = codegenExpr value
  var <- L.global _name AST.i32 (C.Int 32 0)
  -- L.store var 0 val
  registerOperand name var

codegenValue :: EvalAst -> LLVM ()
codegenValue val = case val of
  VariableDefinition name value -> codegenVariableDef name value
  FunctionDefinition fName args body -> do
    let params = map mkParam args
    function <- L.function name params AST.i32 genBody
    registerOperand fName function
    where
      name = mkName (cs fName)
      mkParam :: T.Text -> (AST.Type, L.ParameterName)
      mkParam n = (AST.i32, L.ParameterName (cs n))

      -- Generate the body of the function:
      genBody :: [Operand] -> Codegen ()
      genBody operands = do
        _entry <- L.block `L.named` "entry"
        -- Register parameters, allocate them on the stack,
        -- emit store instructions
        forM_ (zip operands args) $ \(op, n) -> do
          addr <- L.alloca (typeOf op) Nothing 0
          L.store addr 0 op
          registerOperand n addr

        -- emit instructions for function body
        -- emit return
        L.ret . last =<< mapM codegenExpr body
  _ -> return ()

codegenList :: [EvalAst] -> AST.Module
codegenList program =
  flip evalState emptyState $
    L.buildModuleT "Micro Scheme" $ mapM_ codegenValue program
  where
    emptyState = GenState {operands = M.empty}

-- llvm-hs uses ShortByteString for names, but we want
-- easy conversion to Text with cs from Data.String.Conversions
instance ConvertibleStrings T.Text ShortByteString where
  convertString = fromString . T.unpack
