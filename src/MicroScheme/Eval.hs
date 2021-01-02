-- |
-- Module      : MicroScheme.Eval
-- Copyright   : Paweł Rubin
--
-- This module implements semantic analysis of AST of the Micro Scheme language.
module MicroScheme.Eval (evalProgram, EvalAst (..)) where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
    unless,
  )
import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.Map as M
import qualified Data.Text as T
import MicroScheme.Ast (SchemeError (..), SchemeVal (..))
import MicroScheme.Common (withFrozenState)
import MicroScheme.Primitives (Primitive, primitives)

data Env = Env
  { variables :: [T.Text],
    functions :: [T.Text]
  }

type EvalState = ExceptT SchemeError (State Env)

data EvalAst
  = NumConst Integer
  | VariableIdentifier T.Text
  | VariableSet T.Text EvalAst
  | VariableDefinition T.Text EvalAst
  | FunctionDefinition T.Text [T.Text] [EvalAst]
  | PrimitiveCall Primitive [EvalAst]
  | FunctionCall T.Text [EvalAst]
  | IfCall EvalAst EvalAst EvalAst
  deriving (Show)

setFunction :: T.Text -> [SchemeVal] -> [SchemeVal] -> EvalState EvalAst
setFunction name args body = do
  functions <- gets functions
  if name `elem` functions
    then throwError $ FunctionRedefinition name
    else do
      let params = evalFArgs args
      case params of
        Right params -> do
          -- add function to symbol table
          modify $ \env -> env {functions = name : functions}
          withFrozenState $ do
            variables <- gets variables
            modify $ \env -> env {variables = params ++ variables}
            evaluatedBody <- mapM eval body
            return $ FunctionDefinition name params evaluatedBody
        Left err -> throwError err

evalFArgs :: [SchemeVal] -> Either SchemeError [T.Text]
evalFArgs args = case args of
  [] -> return []
  (Atom arg1 : rest) -> do
    rest <- evalFArgs rest
    return $ arg1 : rest
  (_ : _) -> throwError $ Default "Invalid function definition: Invalid arguments"

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

functionCall :: T.Text -> [SchemeVal] -> EvalState EvalAst
functionCall function args = do
  -- first check for primitive function
  let prim = M.lookup function primitives
  args <- evalList args
  case prim of
    Just prim -> return $ PrimitiveCall prim args
    Nothing -> do
      functions <- gets functions
      unless (function `elem` functions) $ throwError $ UnboundFunction function
      return $ FunctionCall function args

ifCall :: SchemeVal -> SchemeVal -> SchemeVal -> EvalState EvalAst
ifCall cond ifTrue ifFalse = do
  _cond <- eval cond
  _ifTrue <- eval ifTrue
  _ifFalse <- eval ifFalse
  return $ IfCall _cond _ifTrue _ifFalse

evalList :: [SchemeVal] -> EvalState [EvalAst]
evalList = mapM eval

eval :: SchemeVal -> EvalState EvalAst
eval (Number num) = pure (NumConst num)
eval (Atom id) = getVar id
eval (List atoms) =
  case atoms of
    [Atom "define", Atom name, value] -> defineVar name value
    (Atom "define" : List (Atom name : params) : body) -> setFunction name params body
    (Atom "define" : _) -> throwError $ Default "Illegal define."
    [Atom "set!", Atom name, value] -> setVar name value
    [Atom "if", cond, ifTrue, ifFalse] -> ifCall cond ifTrue ifFalse
    (Atom function : args) -> functionCall function args
    v -> error $ show v

-- | Traverses the simple parse tree and produces more meaningful AST.
evalProgram :: [SchemeVal] -> Either SchemeError [EvalAst]
evalProgram program = evalState (runExceptT (evalList program)) baseEnv
  where
    baseEnv = Env {functions = [], variables = []}
