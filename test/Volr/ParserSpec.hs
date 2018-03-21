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

    let model = """
input { features = 4 } feeding as1
as1 { strategies = [ 100 ] } feeding output
output { features = 1 }
    """

    it "can parse a simple model" $ do
      parse model `shouldBe` (Right (Model (Input 4) [SurfacePhenomena [As 0]] (Output 0)) :: Either Error Model)
