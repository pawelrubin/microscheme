module HaScheme.Ast where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

data SchemeVal
  = Atom T.Text
  | List [SchemeVal]
  | DottedList [SchemeVal] SchemeVal
  | Number Integer
  | String T.Text
  | Bool Bool
  | PrimitiveFunc ([SchemeVal] -> ThrowsError SchemeVal)
  | Nil

data SchemeError
  = InvalidArgsNum (Maybe Integer) [SchemeVal]
  | TypeMismatch T.Text SchemeVal

type ThrowsError = Either SchemeError

--------------------------------------------
-- Utils
--------------------------------------------

unwordsList :: [SchemeVal] -> T.Text
unwordsList list = T.unwords $ showSchemeVal <$> list

--------------------------------------------
-- Show instances
--------------------------------------------

showSchemeVal :: SchemeVal -> T.Text
showSchemeVal val =
  case val of
    Atom name -> T.pack $ show name
    String str -> "\"" <> str <> "\""
    Number num -> T.pack $ show num
    Bool True -> "#t"
    Bool False -> "#f"
    Nil -> "()"
    List values -> 
      "(" <> T.unwords (map showSchemeVal values) <> ")"
    DottedList head tail -> 
      "(" <> T.unwords (map showSchemeVal head) <> " . " <> showSchemeVal tail <> ")"
    PrimitiveFunc _ -> "<primitive>"

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

--------------------------------------------
-- Pretty instances
--------------------------------------------
instance Pretty SchemeVal where
  pretty = \case
    Atom name -> pretty name
    List values -> 
      lparen <> hsep (map (\val -> pretty val <> "") values) <> rparen
    DottedList head tail -> 
      "(" <> hsep (map (\val -> pretty val <> "") head) <> " . " <> pretty tail <> ")"
    Number num -> pretty num
    String str -> "\"" <> pretty str <> "\""
    Bool True -> "#t"
    Bool False -> "#f"
    PrimitiveFunc _ -> "<primitive>"
    Nil -> "()"
