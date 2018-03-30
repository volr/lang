module Volr.Parser (Error, modelParser, parse, parseResponse, parseStimulus, parseStrategy) where

import Control.Applicative
import Control.Monad.State.Lazy

import qualified Text.Megaparsec as P
import Text.Megaparsec (customFailure, ParseError,ParsecT)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error
import Data.Char
import qualified Data.Map as Map
import Data.Void

import Volr.Ast

type Error = ParseError (P.Token String) String
type ModelState = Map.Map String Stimulatable
type ParserState = State ModelState
type Parser = ParsecT String String ParserState

type IOModule = (Features, Features)

parse :: String -> Either Error Model
parse s =
  evalState (P.runParserT modelParser "" s) Map.empty

modelParser :: Parser Model
modelParser = do
  stimuli <- (space *> P.try (many ( parseStimulus)))
  strategies <- (space *> P.try (many (parseStrategy)))
  Model <$> (space *> parseResponse)

parseResponse :: Parser Response
parseResponse = Response
  <$> (string "response" *> space1 *> parseStimulatableList)
  <*> (space1 *> parseFile)
  <*> (space1 *> parseNamedField "learning_rate" parseNumber)

parseStimulus :: Parser Stimulus
parseStimulus = do
  stimulus <- Stimulus <$> (string "stimulus" *> space1 *> parseName) <*> (space1 *> parseFeatures) <*> (space1 *> parseFile)
  let (Stimulus name _ _) = stimulus
  modify (\m -> Map.insert name (Stimulatable stimulus) m)
  return stimulus

parseStrategy :: Parser Strategy
parseStrategy = do
  strategy <- Strategy <$> ((string "strategy") *> space1 *>
               parseName) <* space1 <*>
               parseStimulatableList <* space1 <*>
               parseNamedField "functions" parseInteger
  let (Strategy name _ _) = strategy
  modify (\m -> Map.insert name (Stimulatable strategy) m)
  return strategy

parseFeatures :: Parser Features
parseFeatures = char '[' *> space *> parseInteger <* space <* char ']'

parseFile :: Parser String
parseFile = parseNamedField "file" parseName

parseNamedField :: String -> Parser a -> Parser a
parseNamedField n p = string n <* space <* char ':' <* space *> p

parseField :: Parser a -> Parser (String, a)
parseField p = (,) <$> parseName <* space <* char ':' <* space <*> p

parseInteger :: Parser Int
parseInteger = (L.lexeme space) L.decimal

parseName :: Parser String
parseName = some (alphaNumChar <|> punctuationChar)

parseStimulatableList :: Parser [Stimulatable]
parseStimulatableList = (:) <$> (string "from" *> space *> parseStimulatable) <*> many (P.try (space *> char ',' *> space *> parseStimulatable))

parseStimulatable :: Parser Stimulatable
parseStimulatable = do
  name <- parseName
  state <- get
  case Map.lookup name state of
    Just s -> gets (\_ -> s)
    Nothing -> customFailure $ "No stimulus or strategy of name '" ++ name ++ "' found"

parseNumber :: Parser Float
parseNumber = (L.lexeme space) L.float
