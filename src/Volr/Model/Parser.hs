{-# LANGUAGE RecordWildCards #-}

module Volr.Model.Parser where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State.Lazy

import Data.Functor.Identity
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Map.Strict as Map

import GHC.Float (double2Float)

import qualified Myelin.SNN as Myelin

import Volr.Model.Model
import Volr.Syntax.AST

type ModelState = ErrorT String (State ExperimentState)

data ExperimentState = ExperimentState
  { edges :: [Edge]
  , index :: Int
  , nodes :: Map.Map String Vertex
  }

type Edge = Graph.LEdge Connection
type Vertex = (Int, Node)

parse :: Expr -> Either String Experiment
parse (ExperimentExpr initialExprs) =
  let
    initialState = ExperimentState [] 0 Map.empty
  in evalState (runErrorT parseModel) initialState
  where
    parseModel :: ModelState Experiment
    parseModel = do
      -- Parse all nodes
      (backendExprs, _) <- parseUntilError initialExprs [] parseNode

      -- Parse all backends
      (_, backends) <- parseUntilError backendExprs [] parseBackend

      -- Build the graph
      state <- get
      let graphEdges = edges state
      let graphNodes = Map.elems $ nodes state
      let model = Graph.mkGraph graphNodes graphEdges

      pure $ Experiment model backends
parse s = Left $ "Expected a full experiment model, but got " ++ show s

-- Block parsing

-- | Parses an incomplete node block with the given name and index
parseNode :: Expr -> ModelState Node
parseNode (BlockExpr name label exprs) = do
  case name of
    "population" -> parsePopulationBody exprs
    "stimulus" -> parseStimulusBody name exprs
    "response" -> parseResponseBody exprs
    _ -> throwError $ "Expected 'population', 'stimulus' or 'response' but found " ++ name

-- Backend

parseBackend :: Expr -> ModelState Backend
parseBackend (BlockExpr "backend" (Just target) [FieldExpr "runtime" (RealExpr runtime)]) =
  let
    executionTarget = case target of
      "nest" -> Myelin.Nest 0 100
      "brainscales" -> Myelin.BrainScaleS 33 297
  in return $ Myelin executionTarget runtime
parseBackend s = throwError $ "Expected backend, but got " ++ (show s)

-- Population

parsePopulationBody :: [Expr] -> ModelState Node
--parseBlock (BlockExpr "stimulus" label entries) =
parsePopulationBody expr = throwError ("Unknown block " ++ (show expr))

-- Response

parseResponseBody :: [Expr] -> ModelState Node
parseResponseBody body = do
  return Response

-- Stimulus

parseStimulusBody :: String -> [Expr] -> ModelState Node
parseStimulusBody name [source] = do
  source <- parseDataSource source
  return $ Stimulus name source
parseStimulusBody name [] = throwError $ "Stimulus " ++ name ++ " requires a data source, but none was given"
parseStimulusBody name s = throwError $ "Too many elements for stimulus " ++ name ++ ": " ++ show s

parseDataSource :: Expr -> ModelState DataSource
parseDataSource (FieldExpr "file" (StringExpr fileName)) = return (File fileName)
parseDataSource (FieldExpr "array" l@(ListExpr _)) = fmap Array (parseNumberList l)
parseDataSource _ = throwError "Error"

-- Connection

parseConnection :: Expr -> Vertex -> ModelState Connection
parseConnection (BlockExpr "from" (Just name) exprs) to = do
  connection <- case exprs of
    [RealExpr weight] -> return $ Connection (double2Float weight)
    [] -> return $ Connection 1
    s -> throwError $ "Connection to " ++ name ++ " only supports a weight attribute, but received several: " ++ show s
  addEdge name to connection
  return connection
parseConnection (BlockExpr "from" Nothing _) _ =
  throwError "Connection requires the name of the source population or simuli. None was given"
parseConnection s _ = throwError $ "Expected connection, but got " ++ show s

-- Internal

parseNumberList :: Expr -> ModelState [Float]
parseNumberList (ListExpr list) = return $ listToFloats list
  where
    listToFloats ((RealExpr first):rest) = (realToFrac first) : (listToFloats rest)
    listToFloats [] = []
parseNumberList t = throwError $ "Expected list of numbers, but got " ++ (show t)

getAndIncrementIndex :: ModelState Int
getAndIncrementIndex = do
  state <- get
  let idx = index state
  put $ state {index = idx + 1}
  return idx

addEdge :: String -> Vertex -> Connection -> ModelState Edge
addEdge fromName (toId, toNode) connection = do
  (fromId, fromNode) <- getVertex fromName
  state <- get
  let xs = edges state
  let edge = (fromId, toId, connection) :: Edge
  put $ state { edges = (edge : xs) }
  return edge

addNode :: String -> Node -> ModelState Int
addNode key value = do
  state <- get
  nextId <- getAndIncrementIndex
  put $ state { nodes = Map.insert key (nextId, value) (nodes state)}
  return nextId

addNodeWithDefaultName :: Maybe String -> Node -> ModelState ()
addNodeWithDefaultName maybeName node = do
  name <- case maybeName of
    Just n -> pure n
    Nothing -> show <$> getAndIncrementIndex
  addNode name node
  return ()

getNode :: String -> ModelState Node
getNode = fmap snd . getVertex

getNodeId :: String -> ModelState Int
getNodeId = fmap fst . getVertex

getVertex :: String -> ModelState Vertex
getVertex name = do
  state <- get
  case nodes state Map.!? name of
    Nothing -> throwError $ "Could not find module of name " ++ name
    Just m -> return m

-- | Parses a list of expressions until an error occurs, but instead of relaying
--   the error, the remaining expressions is returned along with the
--   successfully parsed elements.
parseUntilError :: [Expr] -> [a] -> (Expr -> ModelState a) -> ModelState ([Expr], [a])
parseUntilError exprs@(next:remaining) parsed parser = do
    result <- parser next
    return (remaining, (result:parsed))
  `catchError` (\err -> return (exprs, parsed)) -- Discard error for now; TODO: Store for later debugging
