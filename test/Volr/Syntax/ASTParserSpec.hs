module Volr.Syntax.ASTParserSpec (main, spec) where

import Control.Monad.State.Lazy
import Data.Map as Map

import Text.Megaparsec (ParseError)
import qualified Text.Megaparsec as P

import Test.Hspec
import qualified Test.QuickCheck

import Myelin.SNN
import Volr.Syntax.AST
import Volr.Syntax.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The AST parser parser" $ do

    it "can parse an empty experiment" $ do
      parse "" `shouldBe` (Right (ExperimentExpr []))
