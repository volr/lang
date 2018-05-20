module Volr.Model.ParserSpec (main, spec) where

import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.Either (isLeft)
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Map.Strict as Map

import Text.Megaparsec (ParseError)
import qualified Text.Megaparsec as P

import Test.Hspec
import qualified Test.QuickCheck

import qualified Myelin.SNN as Myelin
import Volr.Model.Model
import Volr.Model.Parser
import Volr.Syntax.AST

main :: IO ()
main = hspec spec

parseFailure :: Expr -> (Expr -> ModelState a) -> IO ()
parseFailure expr parser = do
  let r = evalState (runExceptT (parser expr)) emptyState
  isLeft r `shouldBe` True

parseFailureEnv :: (Eq a, Show a)
                => Expr
                -> ExperimentState
                -> (Expr -> ModelState a)
                -> IO ()
parseFailureEnv expr env parser = do
  let r = evalState (runExceptT (parser expr)) env
  isLeft r `shouldBe` True

parseSuccess :: (Eq a, Show a) => Expr -> (Expr -> ModelState a) -> a -> IO ()
parseSuccess expr parser expected = do
  let r = evalState (runExceptT (parser expr)) emptyState
  r `shouldBe` (Right expected)

parseSuccessEnv :: (Eq a, Show a)
                => Expr
                -> ExperimentState
                -> (Expr -> ModelState a)
                -> a
                -> ExperimentState
                -> IO ()
parseSuccessEnv expr env parser expected expectedEnv =
  let (result, state) = runState (runExceptT (parser expr)) env
  in  do  result `shouldBe` Right expected
          state `shouldBe` expectedEnv
          return ()

stateWith :: [(String, Vertex)] -> [Edge] -> ExperimentState
stateWith vertices edges =
  let env = emptyState
  in  env { nodes = Map.fromList vertices, edges = edges }

spec :: Spec
spec = do
  describe "The model parser" $ do
    let initialState = emptyState
    let fileField = FieldExpr "file" (StringExpr "x")

    context "when parsing experiments" $ do

      it "can parse an empty experiment" $ do
        let expected = Experiment Graph.empty [Myelin (Myelin.Nest 0 100) 100]
        parseSuccess (ExperimentExpr [BlockExpr "backend" (Just "nest") [FieldExpr "runtime" (RealExpr 100)]]) parseExperiment expected

    context "when parsing blocks" $ do

      it "can parse a block to a stimulus" $ do
        parseSuccess (BlockExpr "stimulus" Nothing [fileField]) parseNode (Stimulus "stimulus1" (File "x"))

      it "can fail to parse a block to a stimulus without input" $ do
        parseFailure (BlockExpr "stimulus" Nothing []) parseNode

      it "can fail to parse a population without specification for neurons" $ do
        let env = stateWith [("a", (0, Stimulus "a" (File "x")))] []
        parseFailureEnv (BlockExpr "population" Nothing [BlockExpr "from" (Just "a") []]) env parseNode

      it "can parse a block to a population" $ do
        let connection = Connection 1
        let env = stateWith [("a", (0, Stimulus "a" (File "x")))] []
        let postEnv = env { edges = [(0, 1, connection)], index = 1}
        let inner = [BlockExpr "from" (Just "a") [], FieldExpr "neurons" (IntExpr 7)]
        let expr = BlockExpr "population" (Just "p") inner
        parseSuccessEnv expr env parseNode (Population "p" 7) postEnv

      it "can parse a block to a response" $ do
        let connection = Connection 1
        let env = stateWith [("a", (0, Stimulus "a" (File "x")))] []
        let postEnv = env { edges = [(0, 1, connection)], index = 1}
        let expr = BlockExpr "response" Nothing [BlockExpr "from" (Just "a") []]
        parseSuccessEnv expr env parseNode Response postEnv

      it "can fail to parse a block to a response without connections " $ do
        parseFailure (BlockExpr "response" Nothing []) parseNode

    context "when parsing backends" $ do

      it "can parse a data source from an array" $ do
        parseSuccess (FieldExpr "array" (ListExpr [(RealExpr 1), (RealExpr 2), (RealExpr 3)])) parseDataSource $ Array [1, 2, 3]

      it "can parse a data source from a file" $ do
        parseSuccess (FieldExpr "file" (StringExpr "x")) parseDataSource $ File "x"
