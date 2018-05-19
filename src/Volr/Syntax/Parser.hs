{-# LANGUAGE FlexibleInstances #-}

module Volr.Syntax.Parser where

import Control.Applicative hiding (many, some)
import Control.Monad.State.Lazy

import Data.Functor
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

instance Megaparsec.ShowErrorComponent String where
  showErrorComponent = show

parse :: String -> Either SyntaxError Expr
parse code = Megaparsec.runParser parseExperiment "" code

parseExperiment :: Parser Expr
parseExperiment =
  let rootBlock = Lexer.nonIndented newlineSpace parseBlock
  in  ExperimentExpr <$> Megaparsec.some (newlineSpace *> parseBlock)

-- Block
-- Thanks to: https://markkarpov.com/megaparsec/indentation-sensitive-parsing.html
parseBlock :: Parser Expr
parseBlock =
  Lexer.indentBlock newlineSpace innerParser
  where
    innerParser = do
      -- Parse the category
      category <- Megaparsec.dbg "category" parseString
      -- Parse an optional name
      name <- Megaparsec.dbg "name" $ inlineSpace *> Megaparsec.try (Megaparsec.optional parseString)
      -- Parse a potential early end (end-of-file or double linebreak)
      eof <- Megaparsec.dbg "eof" $ Megaparsec.optional $ Megaparsec.eof
               <|> Megaparsec.try (Char.newline *> Char.newline $> ())

      if isJust eof
        then return $ Lexer.IndentNone $ BlockExpr category name []
        else do
          -- Nested content can be either block, a number of fields or a scalar
          let nestedParser = Megaparsec.try (Megaparsec.dbg "nested block" parseBlock)
                              <|> Megaparsec.try (Megaparsec.dbg "nested field" (parseField parseScalar))
                              <|> Megaparsec.dbg "nested scalar" parseScalar
          return $ Lexer.IndentSome Nothing (return . BlockExpr category name) nestedParser

-- Aggregations

parseField :: Parser Expr -> Parser Expr
parseField innerParser = FieldExpr <$> (parseString <* inlineSpace <* (Char.string ":") <* inlineSpace) <*> innerParser

parseList :: Parser Expr -> Parser Expr
parseList inner = do
  Char.char '['
  newlineSpace
  Megaparsec.choice
    [ do  Char.char ']'
          return $ ListExpr []
    , do  headExpr <- inner
          newlineSpace
          parseListHelp inner [headExpr]
    ]

parseListHelp :: Parser Expr -> [Expr] -> Parser Expr
parseListHelp inner list = do
  Megaparsec.choice
    [ do  Char.char ','
          newlineSpace
          pos <- Megaparsec.getPosition
          next <- inner
          checkSpace pos
          Megaparsec.try newlineSpace
          parseListHelp inner (next:list)
    , do  Char.char ']'
          return $ ListExpr (reverse list)
    ]

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

-- | Verifies that the indentation is above or equal to the expected position
--   Inspired by the Elm compiler: https://github.com/elm-lang/elm-compiler/blob/master/compiler/src/Parse/Primitives.hs
checkSpace :: Pos.SourcePos -> Parser ()
checkSpace (Pos.SourcePos _ _ col) =
  do  indent <- getIndent
      if indent > col && indent > (Pos.mkPos 1)
        then return ()
        else Megaparsec.customFailure $ "Expected indentation of " ++ (show indent) ++ " or more, but saw " ++ (show col)

getIndent :: Parser Pos.Pos
getIndent = do
  Pos.SourcePos _ _ col <- Megaparsec.getPosition
  return col

inlineSpace :: Parser ()
inlineSpace = Lexer.space (Megaparsec.takeWhile1P Nothing f *> pure ()) lineComment empty
  where f x = x == ' ' || x == '\t'

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "#"

newlineSpace :: Parser ()
newlineSpace = Lexer.space Char.space1 lineComment empty
