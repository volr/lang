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

import qualified Myelin.SNN as SNN
import Volr.Model
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

    it "can build a bare-bone example" $ do
      let expected = Experiment Graph.empty [Myelin (SNN.Nest 0 100) 100]
      parse "target nest\n runtime: 100" `shouldBe` Right expected

    it "can build a 1layer example" $ do
      let nodes =
            [ (1, Stimulus "s" (Array [1.0, 2.0, 3.0]))
            , (2, Population "f" 10)
            , (3, Response)]
      let edges =
            [ (1, 2, Connection 1)
            , (2, 3, Connection 1)]
      let graph = Graph.mkGraph nodes edges
      parseSuccess "1layer" $ Experiment graph [Myelin (SNN.Nest 0 100) 90]
