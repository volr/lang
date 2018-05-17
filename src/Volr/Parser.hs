module Volr.Parser where

import Data.Either

import Volr.Model.Model
import Volr.Model.Parser as ModelParser
import Volr.Syntax.Parser as SyntaxParser

parse :: String -> Either String Experiment
parse code = either (Left . show) ModelParser.parse $ SyntaxParser.parse code
