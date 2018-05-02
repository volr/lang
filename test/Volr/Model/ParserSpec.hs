module Volr.Model.ParserSpec (main, spec) where

import Control.Monad.Error
import Control.Monad.State.Lazy
import Data.Map.Strict as Map

import Text.Megaparsec (ParseError)
import qualified Text.Megaparsec as P

import Test.Hspec
import qualified Test.QuickCheck

import Myelin.SNN
import Volr.Model.Model
import Volr.Model.Parser
import Volr.Syntax.AST

main :: IO ()
main = hspec spec

parseSuccess :: (Eq a, Show a) => Expr -> (Expr -> ModelState a) -> a -> IO ()
parseSuccess expr parser expected = do
  let r = evalState (runErrorT (parser expr)) $ ExperimentState 0 0 0 Map.empty
  r `shouldBe` (Right expected)

spec :: Spec
spec = do
  describe "The model parser" $ do
    let initialState = ExperimentState 0 0 0 Map.empty

    it "can parse a block to a stimulus" $ do
      parseSuccess (BlockExpr "stimulus" Nothing [FieldExpr "file" (StringExpr "x")]) parseStimulus (Stimulus "stimulus1" (File "x"))
