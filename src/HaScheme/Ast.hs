module HaScheme.Ast where

import Data.IORef (IORef)
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
  | PrimitiveFunc ([SchemeVal] -> ThrowsError SchemeVal)
  | Func
      { params :: [T.Text],
        vararg :: Maybe T.Text,
        body :: [SchemeVal],
        closure :: Env
      }
  | Nil

--------------------------------------------
-- Environment
--------------------------------------------

type Env = IORef [(String, IORef SchemeVal)]

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
    Func {params = args, vararg = varargs} ->
      "(lambda (" <> T.unwords args
        <> ( case varargs of
               Nothing -> ""
               Just arg -> " . " <> arg
           )
        <> ") ...)"

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

--------------------------------------------
-- Pretty instances
--------------------------------------------
instance Pretty SchemeVal where
  pretty = \case
    Atom name -> pretty name
    List values ->
      lparen <> hsep (map (\val -> pretty val <> "") values) <> rparen
    DottedList head tail ->
      lparen <> hsep (map (\val -> pretty val <> "") head) <> " . " <> pretty tail <> rparen
    Number num -> pretty num
    String str -> "\"" <> pretty str <> "\""
    Bool True -> "#t"
    Bool False -> "#f"
    PrimitiveFunc _ -> "<primitive>"
    Func {params = args, vararg = varargs} ->
      "(lambda (" <> hsep (map (\val -> pretty val <> "") args)
        <> ( case varargs of
               Nothing -> ""
               Just arg -> " . " <> pretty arg
           )
        <> ") ...)"
    Nil -> "()"
