module Parser (parse) where

import Control.Applicative

import qualified Text.Megaparsec as P
import Text.Megaparsec (ParseError,Parsec)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error
import Data.Char
import Data.Void

import Ast (Model(Model), Input(Input), As(As), SurfacePhenomena(SurfacePhenomena),
            Output(Output))

type Parser a = Parsec String String a

parse :: String -> Either (ParseError (P.Token String) String) Model
parse = P.parse parseModel ""

parseModel :: Parser Model
parseModel = do
  let input = Input 0
  let output = Output 0
  let as = As 0
  let surface = SurfacePhenomena [as]
  return $ Model input [surface] output

parseField :: Parser a -> Parser (String, a)
parseField p = (,) <$> parseName <* space <* char '=' <* space <*> p

parseInteger :: Parser Int
parseInteger = (L.lexeme space) L.decimal

parseName :: Parser String
parseName = some $ printChar

parseNumber :: Parser Float
parseNumber = (L.lexeme space) L.float
