module Volr.Syntax.Parser where

import Control.Applicative
import Control.Monad.State.Lazy

import qualified Data.Map as Map
import qualified Data.Scientific as Scientific

import qualified Text.Megaparsec as P
import Text.Megaparsec (customFailure, ParseError,ParsecT)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Volr.Syntax.AST

type SyntaxError = ParseError (P.Token String) String
type ExprState = Map.Map String Expr
type ParserState = State ExprState
type Parser = ParsecT String String ParserState

-- Static values
indentationSpaceTwo = "  "
indentationSpaceFour = "    "
indentationTabs = "\t"

parse :: String -> Either SyntaxError Expr
parse code = evalState (P.runParserT experimentParser "" code) Map.empty

experimentParser :: Parser Expr
experimentParser = pure (ExperimentExpr [])

-- Scalars

parseInt :: Parser Expr
parseInt = IntExpr <$> (L.lexeme space) L.decimal

parseQuantity :: Parser Expr
parseQuantity = do
  quantity <- (Scientific.toRealFloat <$> (P.try (L.scientific))) <|> (P.try L.float) <|> (fromIntegral <$> L.decimal)
  unit <- space *> parseString
  return $ QuantityExpr quantity unit

parseString :: Parser Expr
parseString = StringExpr <$> some (alphaNumChar <|> punctuationChar)

-- parseIndentation :: Int -> Parser ()
-- parseIndentation 0 = (pure ())
-- parseIndentation n = (P.try )
