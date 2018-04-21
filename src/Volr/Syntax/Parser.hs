module Volr.Syntax.Parser where

import Control.Applicative hiding (many, some)
import Control.Monad.State.Lazy

import qualified Data.Map as Map
import qualified Data.Scientific as Scientific

import qualified Text.Megaparsec as P
import Text.Megaparsec (customFailure, ParseError, Parsec)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Volr.Syntax.AST

type SyntaxError = ParseError (P.Token String) String
type Parser = Parsec String String

-- Static values
indentationSpaceTwo = "  "
indentationSpaceFour = "    "
indentationTabs = "\t"

parse :: String -> Either SyntaxError Expr
parse code = P.runParser experimentParser "" code

experimentParser :: Parser Expr
experimentParser = pure (ExperimentExpr [])

-- Aggregations

parseField :: Parser Expr -> Parser Expr
parseField innerParser = FieldExpr <$> (P.some alphaNumChar <* space <* (string ":") <* space) <*> innerParser

parseList :: Parser Expr -> Parser Expr
parseList inner = do
  let list = (parseScalar `P.sepBy` (space *> char ',' *> space)) <|> (pure [])
  ListExpr <$> (string "[" *> space *> list <* space <* string "]")

-- Scalars

parseScalar :: Parser Expr
parseScalar = (P.try parseQuantity) <|> (P.try parseReal) <|> (P.try parseInt) <|> parseString

parseInt :: Parser Expr
parseInt = IntExpr <$> (L.signed (void spaceChar) L.decimal)

parseQuantity :: Parser Expr
parseQuantity = QuantityExpr <$> parseReal <*> (space *> parseString)

parseReal :: Parser Expr
parseReal = RealExpr <$> (L.signed space (P.try L.float) <|> (fromIntegral <$> L.decimal))

parseString :: Parser Expr
parseString = StringExpr <$> P.some (alphaNumChar <|> (char '.'))

-- parseIndentation :: Int -> Parser ()
-- parseIndentation 0 = (pure ())
-- parseIndentation n = (P.try )
