module Volr.Parser where

import Data.Either

import Volr.Model
import Volr.Semantic.Parser as SemanticParser
import Volr.Syntax.Parser as SyntaxParser

parse :: String -> Either String Experiment
parse code = either (Left . show) SemanticParser.parse $ SyntaxParser.parse code
