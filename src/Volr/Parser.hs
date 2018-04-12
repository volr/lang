module Volr.Parser (Error, modelParser, parse, parseResponse, parseStimulus, parseFunction, parseTarget) where

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

import Myelin.SNN (ExecutionTarget(Spikey, Nest))
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
  functions <- (space *> P.try (many (parseFunction)))
  Model <$> (space *> parseResponse) <*> (space *> parseTarget)

parseResponse :: Parser Response
parseResponse = Response
  <$> (string "response" *> space1 *> parseStimulatableList)

parseStimulus :: Parser Stimulus
parseStimulus = do
  stimulus <- Stimulus <$> (string "stimulus" *> space1 *> parseName) <*> (space1 *> parseFeatures)
  let (Stimulus name _) = stimulus
  modify (\m -> Map.insert name (Stimulatable stimulus) m)
  return stimulus

parseFunction :: Parser Function
parseFunction = do
  function <- Function <$> ((string "function") *> space1 *>
               parseName) <* space1 <*>
               parseStimulatableList <* space1 <*>
               parseNamedField "neurons" parseInteger
  let (Function name _ _) = function
  modify (\m -> Map.insert name (Stimulatable function) m)
  return function

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
    Nothing -> customFailure $ "No stimulus or function of name '" ++ name ++ "' found"

parseTarget :: Parser Target
parseTarget = Target
  <$> ((string "target") *> space1 *> ((P.try (string' "futhark") *> (pure Futhark)) <|> (string' "nest") *> (pure (Myelin (Nest 0 0)))))
  <*> (space *> parseDataSource)
  <*> (space *> parseNamedField "output" parseName)
-- (P.try (string' "spikey") *> (pure (Myelin (Spikey 0)))) <|>

parseDataSource :: Parser DataSource
parseDataSource = (P.try (Array <$> (parseNamedField "input" (parseList parseNumber))) <|> (File <$> (parseNamedField "input" parseName)))

parseNumber :: Parser Float
parseNumber = (L.lexeme space) L.float

parseList :: Parser a -> Parser [a]
parseList inner = string "[" *> space *> ((:) <$> inner <*> (P.many (P.try (space *> string "," *> space *> inner)))) <* space <* string "]"
