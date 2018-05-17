module Volr.ModelSpec (main, spec) where

import Control.Monad.Error
import Control.Monad.State.Lazy

import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Map.Strict as Map
import System.IO

import Text.Megaparsec (ParseError)
import qualified Text.Megaparsec as P

import Test.Hspec
import qualified Test.QuickCheck

import Myelin.SNN
import Volr.Model.Model
import Volr.Parser

main :: IO ()
main = hspec spec

readExample :: String -> IO String
readExample fileName = readFile ("examples/" ++ fileName ++ ".volr")

parseSuccess :: String -> Experiment -> IO ()
parseSuccess fileName expected = do
  text <- readExample fileName
  parse text `shouldBe` Right expected

spec :: Spec
spec = do
  describe "Volr model parser" $ do
    it "can build a 1layer example" $ do
      parseSuccess "1layer" $ Experiment Graph.empty [Myelin (Nest 0 0) 100]
