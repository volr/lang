module Parser (parse) where

import qualified Text.Parsec as P
import Text.Parsec (ParseError, parserFail)
import Text.Parsec.Char
import Data.Char
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

import Ast (Model(Model), Input(Input), As(As), SurfacePhenomena(SurfacePhenomena),
            Output(Output))

parse :: String -> Either ParseError Model
parse = P.parse parseModel ""

parseModel :: Parser Model
parseModel = do
  let input = Input 0
  let output = Output 0
  let as = As 0
  let surface = SurfacePhenomena [as]
  return $ Model input [surface] output 
