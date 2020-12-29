{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : MicroScheme.Parser
-- Copyright   : Paweł Rubin
--
-- This module implements parsing of the Micro Scheme language.
module MicroScheme.Parser (readExprFile) where

import Control.Monad (mzero)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import MicroScheme.Ast (SchemeVal (..))
import Text.Parsec
  ( ParseError,
    SourceName,
    char,
    digit,
    eof,
    letter,
    oneOf,
    parse,
    sepBy,
    try,
    (<?>),
    (<|>),
  )
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Token as Tok

langDef :: Tok.GenLanguageDef T.Text () Identity
langDef =
  Lang.emptyDef
    { Tok.commentStart = "#|",
      Tok.commentEnd = "|#",
      Tok.commentLine = ";",
      Tok.nestedComments = True,
      Tok.caseSensitive = True,
      Tok.opStart = mzero,
      Tok.opLetter = mzero,
      Tok.identStart = letter <|> symbol,
      Tok.identLetter = letter <|> digit <|> symbol
    }

-- | Match a special character
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~."

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

decimal :: Parser Integer
decimal = Tok.decimal lexer

sign :: Parser (Integer -> Integer)
sign =
  (char '-' $> negate)
    <|> return id

atomP :: Parser SchemeVal
atomP = Atom . T.pack <$> Tok.identifier lexer <?> "identifier"

listP :: Parser SchemeVal
listP = List <$> try (parens (schemeVal `sepBy` whitespace)) <?> "list"

numberP :: Parser SchemeVal
numberP = Number <$> try (sign <*> decimal) <?> "number"

schemeVal :: Parser SchemeVal
schemeVal =
  numberP
    <|> atomP
    <|> listP
    <?> "Scheme value"

readExprFile :: SourceName -> T.Text -> Either ParseError [SchemeVal]
readExprFile = parse (whitespace *> (schemeVal `sepBy` whitespace) <* eof)
