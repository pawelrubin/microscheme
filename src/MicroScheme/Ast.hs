-- |
-- Module      : MicroScheme.Ast
-- Copyright   : Paweł Rubin
--
-- This module introduces the AST data type and the SchemeError data type.
module MicroScheme.Ast where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec (ParseError)

data SchemeVal
  = Atom T.Text
  | List [SchemeVal]
  | Number Integer
  deriving (Show)

--------------------------------------------
-- Errors
--------------------------------------------

-- | Errors that might occur during evaluation.
data SchemeError
  = -- | Error during parsing.
    Parser ParseError
  | -- | Unbound variable.
    UnboundVariable T.Text
  | -- | Indicated variable redefinition
    VariableRedefinition T.Text
  | FunctionRedefinition T.Text
  | UnboundFunction T.Text
  | BadSpecialForm T.Text SchemeVal
  | Default T.Text

type ThrowsError = Either SchemeError

--------------------------------------------
-- Show instances
--------------------------------------------

instance Show SchemeError where
  show = T.unpack . showError

showError :: SchemeError -> T.Text
showError err =
  case err of
    Parser parseError -> T.pack $ show parseError
    UnboundVariable varName -> "Getting an unbound variable: " <> varName
    VariableRedefinition varName -> "Variable redefinition: " <> varName
    FunctionRedefinition fName -> "Function redefinition: " <> fName
    UnboundFunction fName -> "Getting an unbound function: " <> fName
    BadSpecialForm msg val -> "Invalid special form: " <> msg <> T.pack (" of " <> show val)
    Default msg -> "Error: " <> msg
