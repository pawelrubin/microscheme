module HaScheme.Ast where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Text.ParserCombinators.Parsec

data SchemeVal
  = Atom T.Text
  | List [SchemeVal]
  | DottedList [SchemeVal] SchemeVal
  | Number Integer
  | String T.Text
  | Bool Bool
  | Nil

--------------------------------------------
-- Errors
--------------------------------------------

-- | Errors that might occur during evaluation.
data SchemeError
  = -- | Invalid number of arguments passed
    InvalidArgsNum (Maybe Integer) [SchemeVal]
  | -- | Type mismatch error
    TypeMismatch T.Text SchemeVal
  | -- | Error during parsing.
    Parser ParseError
  | -- | Unbound variable.
    UnboundVariable T.Text
  | VariableRedefinition T.Text
  | FunctionRedefinition T.Text
  | UnboundFunction T.Text
  | BadSpecialForm T.Text SchemeVal
  | Default T.Text

type ThrowsError = Either SchemeError

--------------------------------------------
-- Show instances
--------------------------------------------

showSchemeVal :: SchemeVal -> T.Text
showSchemeVal val =
  case val of
    Atom name -> T.pack $ "Atom " <> show name
    String str -> "String \"" <> str <> "\""
    Number num -> T.pack $ "Number " <> show num
    Bool True -> "Bool #t"
    Bool False -> "Bool #f"
    Nil -> "Nil ()"
    List values ->
      "List (" <> T.unwords (map showSchemeVal values) <> ")"
    DottedList head tail ->
      "DottedList (" <> T.unwords (map showSchemeVal head) <> " . " <> showSchemeVal tail <> ")"

instance Show SchemeVal where
  show = T.unpack . showSchemeVal

instance Show SchemeError where
  show = T.unpack . showError

showError :: SchemeError -> T.Text
showError err =
  case err of
    InvalidArgsNum num args ->
      T.pack $ "Invalid Number of arguments. Expected: " <> show num <> ", but found " <> show (length args) <> "."
    TypeMismatch expected value ->
      T.pack "Type mismatch. Expected: " <> expected <> T.pack (", but found" <> show value <> ".")
    Parser parseError -> T.pack $ show parseError
    UnboundVariable varName -> "Getting an unbound variable: " <> varName
    VariableRedefinition varName -> "Variable redefinition: " <> varName
    FunctionRedefinition fName -> "Function redefinition: " <> fName
    UnboundFunction fName -> "Getting an unbound function: " <> fName
    BadSpecialForm msg val -> T.pack $ "Invalid special form: " <> T.unpack msg <> " of " <> show val
    Default msg -> "Error: " <> msg

--------------------------------------------
-- Pretty instances
--------------------------------------------
instance Pretty SchemeVal where
  pretty = \case
    Atom name -> "Atom " <> pretty name
    List values ->
      "List "
        <> lparen
        <> hsep (map (\val -> pretty val <> "") values)
        <> rparen
    DottedList head tail ->
      lparen <> hsep (map (\val -> pretty val <> "") head) <> " . " <> pretty tail <> rparen
    Number num -> pretty num
    String str -> "\"" <> pretty str <> "\""
    Bool True -> "#t"
    Bool False -> "#f"
    Nil -> "()"
