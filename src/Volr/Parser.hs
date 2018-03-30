module Volr.Parser (Error, modelParser, parse, parseStimuli, parseStrategy) where

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
  let strategies = [] :: [Strategy]
  let response = Response [] "" Random
  return $ Model strategies response

-- parseResponse :: Parser (Response, ModelState)
-- parseResponse =
--   let
--      getFeatures :: Parser Int
--      getFeatures = do
--        names <- parseNameList
--        state <- get
--        sizes <- map length names
--    in Response <$> (string "response" *> space1 *> getFeatures) <*> (space1 *> parseFile) <*> (space *> parseSelection)

parseSelection :: Parser SelectionCriterion
parseSelection =
   string "criterion:" *> space *> ((string "accuracy" *> pure Accuracy) <|> (string "random" *> pure Random))

parseStimuli :: Parser Stimuli
parseStimuli = do
  stimuli <- Stimuli <$> (string "stimuli" *> space1 *> parseName) <*> (space1 *> parseFeatures) <*> (space1 *> parseFile)
  let (Stimuli name _ _) = stimuli
  modify (\m -> Map.insert name (Stimulatable stimuli) m)
  return stimuli

parseStrategy :: Parser Strategy
parseStrategy = do
  strategy <- Strategy <$> ((string "strategy") *> space1 *>
               parseName) <* space1 <* string "from" <* space <*>
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
parseStimulatableList = (:) <$> parseStimulatable <*> many (P.try (space *> char ',' *> space *> parseStimulatable))

parseStimulatable :: Parser Stimulatable
parseStimulatable = do
  name <- parseName
  state <- get
  case Map.lookup name state of
    Just s -> gets (\_ -> s)
    Nothing -> customFailure $ "No stimuli or strategy of name '" ++ name ++ "' found"

parseNumber :: Parser Float
parseNumber = (L.lexeme space) L.float
