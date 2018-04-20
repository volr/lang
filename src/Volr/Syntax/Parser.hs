module Volr.Syntax.Parser where

import Control.Applicative
import Control.Monad.State.Lazy

import qualified Data.Map as Map

import qualified Text.Megaparsec as P
import Text.Megaparsec (customFailure, ParseError,ParsecT)

import Volr.Syntax.AST

type SyntaxError = ParseError (P.Token String) String
type ExprState = Map.Map String Expr
type ParserState = State ExprState
type Parser = ParsecT String String ParserState

parse :: String -> Either SyntaxError Expr
parse code = evalState (P.runParserT experimentParser "" code) Map.empty

experimentParser :: Parser Expr
experimentParser = pure (ExperimentExpr [])
