{-# LANGUAGE FlexibleContexts #-}
-- We need these to write a ConvertibleStrings instance for
-- ShortByteString
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HaScheme.CodeGen (codegenProgram) where

import Control.Monad.State
import Data.List (find)
import qualified Data.Map as M
import Data.String (fromString)
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import HaScheme.Ast (SchemeVal (..))
import HaScheme.SAst (Definition (FunctionDefinition, VariableDefinition), Expr (NumConstant), Program, Type (..))
import LLVM.AST (Operand)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as AST
import qualified LLVM.AST.FloatingPointPredicate as FP
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
  { operands :: M.Map Text Operand,
    strings :: M.Map Text Operand
  }
  deriving (Eq, Show)

registerOperand :: MonadState Env m => Text -> Operand -> m ()
registerOperand name op =
  modify $ \env -> env {operands = M.insert name op (operands env)}

type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM

-- instance ConvertibleStrings Text ShortByteString where
--   convertString = fromString . T.unpack

astTypeToLLVMAstType :: MonadState Env m => Type -> m AST.Type
astTypeToLLVMAstType = \case
  TInt -> pure AST.i32

-- codegenExpr :: Expr -> LLVM Operand
-- codegenExpr expr = case expr of
--   NumConstant num -> do
--     let llvmtype = C.Int 32 num
--         name = mkName $ cs id
--     L.int32 (fromIntegral num)
--   _ -> undefined

codegenDefinition :: Definition -> LLVM ()
codegenDefinition def = case def of
  VariableDefinition id (NumConstant num) -> mdo
    let llvmtype = C.Int 32 num
        name = mkName $ cs id
    -- L.int32 (fromIntegral num)
    variable <- L.global name AST.i32 llvmtype
    registerOperand id variable
  -- L.store variable 0 value
  VariableDefinition _ __ -> undefined
  FunctionDefinition name args body -> undefined

codegenExpression :: Expr -> LLVM ()
codegenExpression = undefined

codegenProgram :: Program -> AST.Module
codegenProgram (definitions, expressions) =
  flip evalState (Env {operands = M.empty, strings = M.empty}) $
    L.buildModuleT "microscheme" $
      do
        mapM_ codegenDefinition definitions
        mapM_ codegenExpression expressions