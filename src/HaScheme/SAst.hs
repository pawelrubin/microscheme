module HaScheme.SAst where

import qualified Data.Text as T

-- Program

data Program = Program [Definition] [Expr]

-- Definitions

data Definition
  = VariableDefinition T.Text Expr
  | FunctionDefinition T.Text [T.Text] Body

data Body
  = Body [Definition] [Expr]

-- Expressions

data Expr
  = BoolConstant Bool
  | NumConstant Integer
  | Variable T.Text
  | List [Expr]
  | DottedList [Expr] Expr
  | Application Expr [Expr]
