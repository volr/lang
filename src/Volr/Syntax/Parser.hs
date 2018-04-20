module Volr.Syntax.Parser where

import Control.Applicative
import Control.Monad.State.Lazy

import qualified Data.Map as Map

import qualified Text.Megaparsec as P
import Text.Megaparsec (customFailure, ParseError,ParsecT)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Volr.Syntax.AST

type SyntaxError = ParseError (P.Token String) String
type ExprState = Map.Map String Expr
type ParserState = State ExprState
type Parser = ParsecT String String ParserState

parse :: String -> Either SyntaxError Expr
parse code = evalState (P.runParserT experimentParser "" code) Map.empty

experimentParser :: Parser Expr
experimentParser = pure (ExperimentExpr [])

parseInt :: Parser Expr
parseInt = IntExpr <$> (L.lexeme space) L.decimal

parseReal :: Parser Expr
parseReal = RealExpr <$> ((P.try L.float) <|> (fromIntegral <$> L.decimal))

parseString :: Parser Expr
parseString = StringExpr <$> some (alphaNumChar <|> punctuationChar)
