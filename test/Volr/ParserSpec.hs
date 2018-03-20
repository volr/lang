module Volr.ParserSpec (main, spec) where

import Text.Megaparsec (ParseError)

import Test.Hspec
import qualified Test.QuickCheck

import Volr.Parser
import Volr.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The volr parser" $ do

    it "can parse a simple model" $ do
      (parse "") `shouldBe` (Right (Model (Input 0) [SurfacePhenomena [As 0]] (Output 0)) :: Either Error Model)
