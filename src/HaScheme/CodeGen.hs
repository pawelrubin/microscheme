{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- To create ConvertibleStrings instance for ShortByteString
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HaScheme.CodeGen where

import Control.Monad.State
import qualified Data.Map as M
import Data.String (fromString)
import Data.String.Conversions
import qualified Data.Text as T
import HaScheme.Eval
import LLVM.AST (Operand)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Attribute as Atr
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
  VariableIdentifier id -> flip L.load 0 =<< gets ((M.! id) . operands)
  PrimitiveCall prim args -> codegenPrimitiveCall prim args
  VariableSet varName newValue -> codegenVariableSet varName newValue
  VariableDefinition name value -> codegenVariableDef name value
  IfCall cond ifTrue ifFalse -> codegenIfExpr cond ifTrue ifFalse
  FunctionCall name params -> codegenFunCall name params
  x -> error $ show x

codegenFunCall :: T.Text -> [EvalAst] -> Codegen Operand
codegenFunCall fName fParams = do
  f <- gets ((M.! fName) . operands)
  params <- mapM codegenExpr fParams
  L.call f (map (,[]) params)

codegenIfExpr :: EvalAst -> EvalAst -> EvalAst -> Codegen Operand
codegenIfExpr c t f = mdo
  cond <- codegenExpr c
  L.condBr cond thenBlock elseBlock

  thenBlock <- L.block `L.named` "then"
  thenValue <- codegenExpr t
  mkTerminator $ L.br mergeBlock

  elseBlock <- L.block `L.named` "else"
  elseValue <- codegenExpr f
  mkTerminator $ L.br mergeBlock

  mergeBlock <- L.block `L.named` "merge"
  L.phi [(thenValue, thenBlock), (elseValue, elseBlock)]

codegenPrimitiveCall :: Primitive -> [EvalAst] -> Codegen Operand
codegenPrimitiveCall prim (x : xs) = do
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
    Mod -> foldM L.srem x xs
    Lt -> foldM (L.icmp IP.SLT) x xs
    Gt -> foldM (L.icmp IP.SGT) x xs
    Le -> foldM (L.icmp IP.SLE) x xs
    Ge -> foldM (L.icmp IP.SGE) x xs
    Display -> callFunction "display" [(x, [])]
    _ -> error "Not implemented"
codegenPrimitiveCall Newline _ = callFunction "newline" []
codegenPrimitiveCall Read _ = callFunction "read" []
codegenPrimitiveCall _ [] = error "Primitive function called without arguments"

callFunction :: T.Text -> [(Operand, [Atr.ParameterAttribute])] -> Codegen Operand
callFunction f xs = flip L.call xs =<< gets ((M.! f) . operands)

codegenVariableSet :: T.Text -> EvalAst -> Codegen Operand
codegenVariableSet varName newValue = do
  var <- gets ((M.! varName) . operands)
  val <- codegenExpr newValue
  L.store var 0 val
  return var

codegenVariableDef :: T.Text -> EvalAst -> Codegen Operand
codegenVariableDef name value = do
  let _name = mkName $ cs name
  val <- codegenExpr value
  var <- L.global _name AST.i32 (C.Int 32 0)
  L.store var 0 val
  registerOperand name var
  return var

codegenFunction :: T.Text -> [T.Text] -> [EvalAst] -> LLVM ()
codegenFunction fName args body = mdo
  registerOperand fName function
  let params = map mkParam args
  function <- L.function name params AST.i32 genBody
  return ()
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

emitBuiltIns :: LLVM ()
emitBuiltIns = mapM_ emitBuiltIn builtIns
  where
    emitBuiltIn (name, paramsTypes, returnType) = do
      f <- L.extern (mkName $ cs name) paramsTypes returnType
      registerOperand name f
    builtIns =
      [ ("display", [AST.i32], AST.void),
        ("newline", [], AST.void),
        ("read", [], AST.i32)
      ]

codegenList :: [EvalAst] -> AST.Module
codegenList program =
  flip evalState emptyState $
    L.buildModuleT "Micro Scheme" $
      do
        emitBuiltIns
        mapM_ genFunction funDefs
        _ <- L.function (mkName "main") [] AST.i32 genMain
        return ()
  where
    (funDefs, program') = partition (\case FunctionDefinition {} -> True; _ -> False) program
    genMain _ = mapM_ codegenExpr program' >> L.ret (L.int32 0)
    genFunction (FunctionDefinition n xs ys) = codegenFunction n xs ys
    genFunction _ = return ()
    emptyState = GenState {operands = M.empty}

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- llvm-hs uses ShortByteString for names, but we want
-- easy conversion to Text with cs from Data.String.Conversions
instance ConvertibleStrings T.Text ShortByteString where
  convertString = fromString . T.unpack

mkTerminator :: Codegen () -> Codegen ()
mkTerminator instr = do
  check <- L.hasTerminator
  unless check instr
