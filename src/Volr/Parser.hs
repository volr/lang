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

import Myelin.SNN (ExecutionTarget(BrainScaleS, Spikey, Nest), SynapseEffect(Excitatory, Inhibitory))
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
  <$> (string "response" *> space1 *> parseConnectionList)

parseStimulus :: Parser Stimulus
parseStimulus = do
  stimulus <- Stimulus <$> (string "stimulus" *> space1 *> parseName) <*> (space *> parseDataSource) <*> (pure 2) -- TODO: Make features backend dependent or remove from Futhark
  let (Stimulus name _ _) = stimulus
  modify (\m -> Map.insert name (Stimulatable stimulus) m)
  return stimulus

parseFunction :: Parser Function
parseFunction = do
  function <- Function <$> ((string "function") *> space1 *>
               parseName) <* space1 <*>
               parseConnectionList <* space1 <*>
               parseNamedField "neurons" parseInteger
  let (Function name _ _) = function
  modify (\m -> Map.insert name (Stimulatable function) m)
  return function

parseConnectionList :: Parser [Connection]
parseConnectionList = (:) <$> (string "from" *> space *> parseConnection) <*> many (P.try (space *> char ',' *> space *> parseConnection))

parseConnection :: Parser Connection
parseConnection = Connection <$> parseStimulatable
  <*> (space *> ((P.try ((string' "excitatory") *> (pure Excitatory))) <|> ((string' "inhibitory") *> (pure Inhibitory))))
  <*> ((P.try (space *> (parseNamedField "weight" parseNumber))) <|> (pure 1))

parseStimulatable :: Parser Stimulatable
parseStimulatable = do
  name <- parseName
  state <- get
  case Map.lookup name state of
    Just s -> gets (\_ -> s)
    Nothing -> customFailure $ "No stimulus or function of name '" ++ name ++ "' found"

parseTarget :: Parser Target
parseTarget = Target
  <$> ((string "target") *> space1 *> ((P.try parseFutharkBackend) <|> parseMyelinBackend))

parseFutharkBackend :: Parser Backend
parseFutharkBackend = (string' "futhark") *> (Futhark <$> (space *> (parseNamedField "output" parseName)) <*> (space *> parseFeatures))

parseMyelinBackend :: Parser Backend
parseMyelinBackend = Myelin <$> (space *> parseExecutionTarget) <*> (space *> (parseNamedField "runtime" parseNumber))

parseExecutionTarget :: Parser ExecutionTarget
parseExecutionTarget = (P.try (string' "nest" *> pure (Nest 0 0))) <|> parseBrainScaleSTarget

parseBrainScaleSTarget :: Parser ExecutionTarget
parseBrainScaleSTarget = string' "brainscales" *> (BrainScaleS <$> (space *> (parseNamedField "wafer" parseInteger)) <*> (space *> (parseNamedField "hicann" parseInteger)))

parseDataSource :: Parser DataSource
parseDataSource = (P.try (Array <$> (parseNamedField "input" (parseList parseNumber))) <|> (File <$> (parseNamedField "input" parseName)))

--

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

parseNumber :: (RealFloat a) => Parser a
parseNumber = (P.try L.float) <|> (fromIntegral <$> L.decimal)

parseList :: Parser a -> Parser [a]
parseList inner = string "[" *> space *> ((:) <$> inner <*> (P.many (P.try (space *> string "," *> space *> inner)))) <* space <* string "]"
