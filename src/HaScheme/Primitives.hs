module HaScheme.Primitives where

import qualified Data.Map as M
import qualified Data.Text as T

primitives :: M.Map T.Text Primitive
primitives =
  M.fromList
    [ -- numeric
      ("+", Add),
      ("-", Sub),
      ("*", Mult),
      ("/", Div),
      ("%", Mod),
      -- boolean
      ("&&", And),
      ("||", Or),
      (">", Gt),
      ("<", Lt),
      (">=", Ge),
      ("<=", Le),
      ("=", Eq),
      ("/=", Ne),
      -- list
      ("cons", Cons),
      -- functional
      ("apply", Apply),
      -- io
      ("display", Display),
      ("newline", Newline),
      ("read", Read)
    ]

data Primitive
  = -- arithmetic
    Add
  | Sub
  | Mult
  | Div
  | Mod
  | -- boolean
    And
  | Or
  | Gt
  | Lt
  | Ge
  | Le
  | Eq
  | Ne
  | -- list
    Cons
  | -- functional
    Apply
  | -- io
    Display
  | Newline
  | Read
  deriving (Show)
