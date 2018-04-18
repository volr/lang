module Volr.ParserSpec (main, spec) where

import Control.Monad.State.Lazy
import Data.Map as Map

import Text.Megaparsec (ParseError)
import qualified Text.Megaparsec as P

import Test.Hspec
import qualified Test.QuickCheck

import Myelin.SNN
import Volr.Parser
import Volr.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The volr parser" $ do

    let model = "stimulus s\
                \  input: x\
                \ \
                \stimulus t\
                \  input: ø\
                \ \
                \function f\
                \  from s excitatory\
                \    weight: 2\
                \  from t inhibitory\
                \  neurons: 10\
                \ \
                \response from f inhibitory\
                \ \
                \target Futhark\
                \  output: y [3]"

    it "can parse a target" $ do
      let expected = Target (Myelin (Nest 0 0) 101)
      let (r, s) = runState (P.runParserT parseTarget "" "target nest\n  runtime: 101") Map.empty
      r `shouldBe` (Right expected)

    it "can parse a simple model" $ do
      let stimulus1 = Stimulatable (Stimulus "s" (File "x") 2)
      let stimulus2 = Stimulatable (Stimulus "t" (File "ø") 2)
      let function = Stimulatable (Function "f" [Connection stimulus1 Excitatory 2, Connection stimulus2 Inhibitory 1] 10)
      parse model `shouldBe` (Right (Model (Response [Connection function Inhibitory 1]) (Target (Futhark "y" 3))))

    it "can parse a simple response" $ do
      let stimulus = Stimulus "2" (Array []) 3
      let (r, s) = runState (P.runParserT parseResponse "" "response from 2 excitatory") $ Map.fromList [("2", Stimulatable stimulus)]
      let expected = Response [Connection (Stimulatable stimulus) Excitatory 1]
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("2", Stimulatable stimulus)])

    it "can parse a simple stimulus" $ do
      let (r, s) = runState (P.runParserT parseStimulus "" "stimulus 1\n  input: [1, 2, 3]") Map.empty
      let expected = Stimulus "1" (Array [1, 2, 3]) 2
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("1", Stimulatable expected)])

    it "can parse a simple function" $ do
      let stimulus = Stimulus "1" (Array []) 2
      let expected = Function "2" [Connection (Stimulatable stimulus) Inhibitory 1] 10
      let (r, s) = runState (P.runParserT parseFunction "" "function 2 from 1 inhibitory\n  neurons: 10\n") (Map.fromList [("1", Stimulatable stimulus)])
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("1", Stimulatable stimulus), ("2", Stimulatable expected)])

    it "can parse a function with several stimuli" $ do
      let stimulus1 = Stimulus "1" (Array []) 2
      let stimulus2 = Stimulus "2" (Array []) 2
      let expected = Function "3" [Connection (Stimulatable stimulus1) Inhibitory 2, Connection (Stimulatable stimulus2) Excitatory 1] 10
      let (r, s) = runState (P.runParserT parseFunction "" "function 3\n  from 1 inhibitory\n  weight: 2\n  from 2 excitatory\n  neurons: 10\n") (Map.fromList [("1", Stimulatable stimulus1), ("2", Stimulatable stimulus2)])
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("1", Stimulatable stimulus1), ("2", Stimulatable stimulus2), ("3", Stimulatable expected)])

    it "can parse function input weight" $ do
      let stimulus = Stimulus "s" (Array []) 2
      let expected = Function "f" [Connection (Stimulatable stimulus) Excitatory 2] 7
      let (r, _) = runState (P.runParserT parseFunction "" "function f from s excitatory\n  weight: 2\n neurons: 7") (Map.fromList [("s", Stimulatable stimulus)])
      r `shouldBe` (Right expected)
