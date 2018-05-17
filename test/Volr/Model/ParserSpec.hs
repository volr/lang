module Volr.Model.ParserSpec (main, spec) where

import Control.Monad.Error
import Control.Monad.State.Lazy

import Data.Either (isLeft)
import qualified Data.Graph.Inductive.Graph as Grap
import qualified Data.Map.Strict as Map

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

parseFailure :: Expr -> (Expr -> ModelState a) -> IO ()
parseFailure expr parser = do
  let r = evalState (runErrorT (parser expr)) $ ExperimentState [] 0 Map.empty
  isLeft r `shouldBe` True

parseSuccess :: (Eq a, Show a) => Expr -> (Expr -> ModelState a) -> a -> IO ()
parseSuccess expr parser expected = do
  let r = evalState (runErrorT (parser expr)) $ ExperimentState [] 0 Map.empty
  r `shouldBe` (Right expected)

spec :: Spec
spec = do
  describe "The model parser" $ do
    let initialState = ExperimentState [] 0 Map.empty
    let fileField = FieldExpr "file" (StringExpr "x")

    it "can parse a block to a stimulus" $ do
      parseSuccess (BlockExpr "stimulus" Nothing [fileField]) parseNode (Stimulus "stimulus1" (File "x"))

    it "can fail to parse a block to a stimulus without input" $ do
      parseFailure (BlockExpr "stimulus" Nothing []) parseNode

    it "can parse a block to a response" $ do
      parseSuccess (BlockExpr "response" Nothing [BlockExpr "from" (Just "a") []]) parseNode (Response)

    it "can fail to parse a block to a response without connections " $ do
      parseFailure (BlockExpr "response" Nothing []) parseNode

    it "can parse a data source from an array" $ do
      parseSuccess (FieldExpr "array" (ListExpr [(RealExpr 1), (RealExpr 2), (RealExpr 3)])) parseDataSource $ Array [1, 2, 3]

    it "can parse a data source from a file" $ do
      parseSuccess (FieldExpr "file" (StringExpr "x")) parseDataSource $ File "x"
