{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : HaScheme.Eval
-- Copyright   : Paweł Rubin
--
-- This module implements syntactic analysis of AST of the Micro Scheme language.
module HaScheme.Eval where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
    unless,
  )
import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.Map as M
import qualified Data.Text as T
import HaScheme.Ast (SchemeError (..), SchemeVal (..))
import HaScheme.Primitives (Primitive, primitives)

data Env = Env
  { variables :: [T.Text],
    functions :: [T.Text] -- Add more data to function table
  }

type EvalState = ExceptT SchemeError (State Env)

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

evalList :: [SchemeVal] -> EvalState [EvalAst]
evalList = mapM eval

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
