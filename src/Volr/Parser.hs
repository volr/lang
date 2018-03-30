module Volr.Parser (Error, modelParser, parse, parseStimuli) where

import Control.Applicative
import Control.Monad.State.Lazy

import qualified Text.Megaparsec as P
import Text.Megaparsec (ParseError,ParsecT)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error
import Data.Char
import Data.Map as Map
import Data.Void

import Volr.Ast

type Error = ParseError (P.Token String) String
type ModelState = Map String Stimulatable
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

-- parseResponse :: ModelState -> Parser (Response, ModelState)
-- parseResponse =
--   let
--     getFeatures :: Parser Int
--     getFeatures =
--   Response <$> (string "response" *> space1 *> parseNameList) <*> (space1 *> parseFile) <*> (space *> parseSelection)
--
-- parseSelection :: Parser SelectionCriterion
-- parseSelection =
--   string "criterion:" *> space *> ((string "accuracy" *> pure Accuracy) <|> (string "random" *> pure Random))

parseStimuli :: Parser Stimuli
parseStimuli = do
  stimuli <- Stimuli <$> (string "stimuli" *> space1 *> parseName) <*> (space1 *> parseFeatures) <*> (space1 *> parseFile)
  let (Stimuli name _ _) = stimuli
  modify (\m -> insert name (Stimulatable stimuli) m)
  return stimuli

-- parseStrategy :: Parser Strategy
-- parseStrategy =
--   Strategy <$> (string "strategy") <* space1 <*
--                 parseName <* space1 <* string "from" <* space <*>
--                 parseNameList <* space1 <*>
--                 parseNamedField "functions" parseInteger

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

parseNameList :: Parser [String]
parseNameList = (:) <$> parseName <*> many (space *> char ',' *> space *> parseName)

parseNumber :: Parser Float
parseNumber = (L.lexeme space) L.float
