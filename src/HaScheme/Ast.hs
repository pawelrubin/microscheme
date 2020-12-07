module HaScheme.Ast where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc

data SchemeVal
  = Atom Text
  | List [SchemeVal]
  | Number Integer
  | String Text
  | Bool Bool
  | Nil
  deriving (Show, Eq)

--------------------------------------------
-- Pretty instances
--------------------------------------------
instance Pretty SchemeVal where
  pretty = \case
    Atom name -> pretty name
    List values -> lparen <> hsep (map (\val -> pretty val <> "") values) <> rparen
    Number num -> pretty num
    String str -> pretty str
    Bool True -> "#t"
    Bool False -> "#f"
    Nil -> "()"
