{-# LANGUAGE FlexibleContexts #-}

module HaScheme.Parser
  ( readExpr,
    readExprFile,
  )
where

import Control.Monad (mzero)
import Data.Functor
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import HaScheme.Ast
import Text.Parsec
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok

langDef :: Tok.GenLanguageDef T.Text () Identity
langDef =
  Lang.emptyDef
    { Tok.commentStart = "{-",
      Tok.commentEnd = "-}",
      Tok.commentLine = ";",
      Tok.opStart = mzero,
      Tok.opLetter = mzero,
      Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~",
      Tok.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
    }

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

decimal :: Parser Integer
decimal = Tok.decimal lexer

sign :: Parser (Integer -> Integer)
sign =
  (char '-' $> negate)
    <|> (char '+' $> id)
    <|> return id

textLiteral :: Parser T.Text
textLiteral = T.pack <$> Tok.stringLiteral lexer

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
  where
    specialIdentifier :: Parser String
    specialIdentifier =
      lexeme $
        try $
          string "-" <|> string "+" <|> string "..."

nil :: Parser ()
nil = try (char '\'') *> string "()" *> return () <?> "nil"

schemeVal :: Parser SchemeVal
schemeVal =
  Number <$> try (sign <*> decimal)
    <|> String <$> textLiteral
    <|> Atom <$> identifier
    <|> List <$> parens (schemeVal `sepBy` whitespace)
    <|> Nil <$ nil

readExpr :: T.Text -> Either ParseError SchemeVal
readExpr = parse (whitespace *> lexeme schemeVal <* eof) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either ParseError SchemeVal
readExprFile = parse (whitespace *> (List <$> (schemeVal `sepBy` whitespace)) <* eof)
