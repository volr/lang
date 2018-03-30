module Volr.ParserSpec (main, spec) where

import Control.Monad.State.Lazy
import Data.Map as Map

import Text.Megaparsec (ParseError)
import qualified Text.Megaparsec as P

import Test.Hspec
import qualified Test.QuickCheck

import Volr.Parser
import Volr.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The volr parser" $ do

    let model = "stimulus s [1] \
    \  file: x.txt\
    \ \
    \strategy 1 from s\
    \  functions: 10\
    \ \
    \response from 1\
    \  file: y.txt\
    \  learning_rate: 0.5"

    it "can parse a simple model" $ do
      let stimulus = Stimulatable (Stimulus "s" 1 "x.txt")
      let strategy = Stimulatable (Strategy "1" [stimulus] 10)
      parse model `shouldBe` (Right (Model (Response [strategy] "y.txt" 0.5)))

    it "can parse a simple response" $ do
      let stimulus = Stimulus "2" 3 "x.y"
      let (r, s) = runState (P.runParserT parseResponse "" "response from 2\n  file: x.y\n  learning_rate: 0.5") $ Map.fromList [("2", Stimulatable stimulus)]
      let expected = Response [Stimulatable stimulus] "x.y" 0.5
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("2", Stimulatable stimulus)])

    it "can parse a simple stimulus" $ do
      let (r, s) = runState (P.runParserT parseStimulus "" "stimulus 1 [2]\n  file: x.y") Map.empty
      let expected = Stimulus "1" 2 "x.y"
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("1", Stimulatable expected)])

    it "can parse a simple strategy" $ do
      let stimulus = Stimulus "1" 2 "x.y"
      let expected = Strategy "2" [Stimulatable stimulus] 10
      let (r, s) = runState (P.runParserT parseStrategy "" "strategy 2 from 1\n  functions: 10") (Map.fromList [("1", Stimulatable stimulus)])
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("1", Stimulatable stimulus), ("2", Stimulatable expected)])
