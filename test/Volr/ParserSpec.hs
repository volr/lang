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

    let model = "stimuli s [1] \
    \  file: x.txt\
    \strategy 1 from s\
    \functions: 10\
    \response [2]\
    \  file: y.txt"

    it "can parse a simple model" $ do
      parse model `shouldBe` (Right (Model [Strategy "1" [] 10] (Response [] "y.txt" Random)) :: Either Error Model)

    -- it "can parse a simple response" $ do
      -- P.parse parseResponse "" "response [2]\n  file: x.y\n  criterion: random" `shouldBe` (Right (Response 2 "x.y" Random))

    it "can parse a simple stimuli" $ do
      let (r, s) = runState (P.runParserT parseStimuli "" "stimuli 1 [2]\n  file: x.y") Map.empty
      let expected = Stimuli "1" 2 "x.y"
      r `shouldBe` (Right expected)
      s `shouldBe` (Map.fromList [("1", Stimulatable expected)])
