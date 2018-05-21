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
spec =
  describe "The model parser" $ do
    let initialState = emptyState
    let fileField = FieldExpr "file" (StringExpr "x")

    context "when parsing experiments" $ do

      it "can parse a target-only experiment" $ do
        let expected = Experiment Graph.empty [Myelin (Myelin.Nest 0 100) 100]
        parseSuccess (ExperimentExpr [BlockExpr "target" (Just "nest") [FieldExpr "runtime" (RealExpr 100)]]) parseExperiment expected

      it "can parse a rudimentary experiment" $ do
        let nodes = [(1, Stimulus "a" (File "x")), (2, Response)]
        let edges = [(1, 2, Connection 1)]
        let graph = Graph.mkGraph nodes edges
        let expected = Experiment graph [Myelin (Myelin.Nest 0 100) 100]
        let stimulus = BlockExpr "stimulus" (Just "a") [FieldExpr "file" (StringExpr "x")]
        let response = BlockExpr "response" Nothing [BlockExpr "from" (Just "a") []]
        let target = BlockExpr "target" (Just "nest") [FieldExpr "runtime" (IntExpr 100)]
        let expr = ExperimentExpr [stimulus, response, target]
        parseSuccess expr parseExperiment expected

    context "when parsing blocks" $ do

      it "can parse a block to a stimulus" $
        parseSuccess (BlockExpr "stimulus" Nothing [fileField]) parseNode (Stimulus "stimulus1" (File "x"))

      it "can store a stimulus block as a node" $ do
        let node = Stimulus "a" (File "x")
        let expectedEnv = (stateWith [("a", (1, node))] []) { index = 1 }
        let expr = BlockExpr "stimulus" (Just "a") [FieldExpr "file" (StringExpr "x")]
        parseSuccessEnv expr emptyState parseNode node expectedEnv

      it "can fail to parse a block to a stimulus without input" $
        parseFailure (BlockExpr "stimulus" Nothing []) parseNode

      it "can fail to parse a population without specification for neurons" $ do
        let env = stateWith [("a", (0, Stimulus "a" (File "x")))] []
        parseFailureEnv (BlockExpr "population" Nothing [BlockExpr "from" (Just "a") []]) env parseNode

      it "can parse a block to a population" $ do
        let connection = Connection 1
        let env = stateWith [("a", (0, Stimulus "a" (File "x")))] []
        let postEnv = env { edges = [(0, 1, connection)], index = 1, nodes = Map.insert "p" (1, Population "p" 7) (nodes env)}
        let inner = [BlockExpr "from" (Just "a") [], FieldExpr "neurons" (IntExpr 7)]
        let expr = BlockExpr "population" (Just "p") inner
        parseSuccessEnv expr env parseNode (Population "p" 7) postEnv

      it "can parse a block to a response" $ do
        let connection = Connection 1
        let env = stateWith [("a", (0, Stimulus "a" (File "x")))] []
        let postEnv = env
              { edges = [(0, 1, connection)]
              , index = 1
              , nodes = Map.insert "response1" (1, Response) (nodes env)
              , responseCounter = 1}
        let expr = BlockExpr "response" Nothing [BlockExpr "from" (Just "a") []]
        parseSuccessEnv expr env parseNode Response postEnv

      it "can fail to parse a block to a response without connections " $
        parseFailure (BlockExpr "response" Nothing []) parseNode

    context "when parsing targets" $ do

      it "can parse a data source from an array" $
        parseSuccess (FieldExpr "array" (ListExpr [RealExpr 1, RealExpr 2, RealExpr 3])) parseDataSource $ Array [1, 2, 3]

      it "can parse a data source from a file" $
        parseSuccess (FieldExpr "file" (StringExpr "x")) parseDataSource $ File "x"
