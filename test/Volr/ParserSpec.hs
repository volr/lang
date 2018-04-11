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
    \ \
    \function 1 from s\
    \  neurons: 10\
    \ \
    \response from 1\
    \ \
    \target Futhark\
    \  input: x\
    \  output: y"

    it "can parse a target" $ do
      let expected = Target Myelin "x" "y"
      let (r, s) = runState (P.runParserT parseTarget "" "target Myelin\n  input: x\n  output: y") Map.empty
      r `shouldBe` (Right expected)

    it "can parse a simple model" $ do
      let stimulus = Stimulatable (Stimulus "s" 1)
      let function = Stimulatable (Function "1" [stimulus] 10)
      parse model `shouldBe` (Right (Model (Response [function])(Target Futhark "x" "y")))

    it "can parse a simple response" $ do
      let stimulus = Stimulus "2" 3
      let (r, s) = runState (P.runParserT parseResponse "" "response from 2") $ Map.fromList [("2", Stimulatable stimulus)]
      let expected = Response [Stimulatable stimulus]
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("2", Stimulatable stimulus)])

    it "can parse a simple stimulus" $ do
      let (r, s) = runState (P.runParserT parseStimulus "" "stimulus 1 [2]") Map.empty
      let expected = Stimulus "1" 2
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("1", Stimulatable expected)])

    it "can parse a simple function" $ do
      let stimulus = Stimulus "1" 2
      let expected = Function "2" [Stimulatable stimulus] 10
      let (r, s) = runState (P.runParserT parseFunction "" "function 2 from 1\n  neurons: 10") (Map.fromList [("1", Stimulatable stimulus)])
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("1", Stimulatable stimulus), ("2", Stimulatable expected)])
