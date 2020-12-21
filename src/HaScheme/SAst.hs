module HaScheme.SAst where

import qualified Data.Text as T

-- Program

type Program = ([Definition], [Expr])

-- Definitions

data Definition
  = VariableDefinition T.Text Expr
  | FunctionDefinition T.Text [T.Text] Body
  deriving (Show)

data Body
  = Body [Definition] [Expr]
  deriving (Show)

-- Expressions

data Expr
  = BoolConstant Bool
  | NumConstant Integer
  | Variable T.Text
  | -- | List [Expr]/
    DottedList [Expr] Expr
  | Application Expr [Expr]
  deriving (Show)

data Type
  = TInt
