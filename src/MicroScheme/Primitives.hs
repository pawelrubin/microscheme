-- |
-- Module      : MicroScheme.Primitives
-- Copyright   : Paweł Rubin
--
-- This module declarations of primitive and built-in functions.
module MicroScheme.Primitives where

import qualified Data.Map as M
import qualified Data.Text as T

-- | Primitive and built-in functions.
primitives :: M.Map T.Text Primitive
primitives =
  M.fromList
    [ -- numeric
      ("+", Add),
      ("-", Sub),
      ("*", Mult),
      ("/", Div),
      ("mod", Mod),
      -- boolean
      ("&&", And),
      ("||", Or),
      (">", Gt),
      ("<", Lt),
      (">=", Ge),
      ("<=", Le),
      ("=", Eq),
      ("/=", Ne),
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
  | -- io
    Display
  | Newline
  | Read
  deriving (Show)
