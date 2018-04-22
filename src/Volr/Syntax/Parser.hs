module Volr.Syntax.Parser where

import Control.Applicative hiding (many, some)
import Control.Monad.State.Lazy

import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Scientific as Scientific

import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Pos as Pos

import Volr.Syntax.AST

type SyntaxError = Megaparsec.ParseError (Megaparsec.Token String) String
type Parser = Megaparsec.Parsec String String

parse :: String -> Either SyntaxError Expr
parse code = Megaparsec.runParser experimentParser "" code

experimentParser :: Parser Expr
experimentParser = pure (ExperimentExpr [])

-- Block
-- Thanks to: https://markkarpov.com/megaparsec/indentation-sensitive-parsing.html
parseBlock :: Parser Expr
parseBlock = Lexer.nonIndented newlineSpace (Lexer.indentBlock newlineSpace innerOpt)
  where
    innerOpt = do
      category <- parseString
      name <- Megaparsec.try (inlineSpace *> Megaparsec.optional parseString)
      eof <- Megaparsec.optional Megaparsec.eof
      let indentType = if isJust eof then Lexer.IndentMany else Lexer.IndentSome
      let fieldParser = (parseList parseScalar) <|> parseScalar
      let nestedParser = Megaparsec.try ((parseField fieldParser) <|> (parseList parseScalar))
      return $ indentType Nothing (return . (BlockExpr category name)) nestedParser

-- Aggregations

parseField :: Parser Expr -> Parser Expr
parseField innerParser = FieldExpr <$> (parseString <* inlineSpace <* (Char.string ":") <* inlineSpace) <*> innerParser

parseList :: Parser Expr -> Parser Expr
parseList inner = do
  let list = (inner `Megaparsec.sepBy` (newlineSpace *> (Char.char ',') <* newlineSpace))
  ListExpr <$> (Char.string "[" *> newlineSpace *> list <* newlineSpace <* (Char.string "]"))

-- Scalars

parseScalar :: Parser Expr
parseScalar = (Megaparsec.try parseQuantity) <|> (Megaparsec.try parseNumber) <|> parseName

parseName :: Parser Expr
parseName = StringExpr <$> parseString

parseNumber :: Parser Expr
parseNumber = do
  number <- Lexer.signed inlineSpace (Megaparsec.try (Lexer.float :: Parser Double) <|> (fromIntegral <$> Lexer.decimal))
  let int = round number
  return $ if number == (fromIntegral int) then IntExpr int else RealExpr number

parseQuantity :: Parser Expr
parseQuantity = QuantityExpr <$> parseNumber <*> (inlineSpace *> parseName)

parseString :: Parser String
parseString = Megaparsec.some (Char.alphaNumChar <|> (Char.char '_') <|> (Char.char '-') <|> (Char.char '.'))

-- Spaces
inlineSpace :: Parser ()
inlineSpace = Lexer.space (Megaparsec.takeWhile1P Nothing f *> pure ()) lineComment empty
  where f x = x == ' ' || x == '\t'

inlineSpaces :: Parser ()
inlineSpaces = Megaparsec.many inlineSpace *> pure ()

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "#"

newlineSpace :: Parser ()
newlineSpace = Lexer.space Char.space1 lineComment empty
