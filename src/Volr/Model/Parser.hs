{-# LANGUAGE RecordWildCards #-}

module Volr.Model.Parser where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State.Lazy

import Data.Functor.Identity
import qualified Data.Graph.Inductive.Graph as Graph
import Data.List (partition)
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
  , populationCounter :: Int
  , stimulusCounter :: Int
  } deriving (Eq, Show)

emptyState = ExperimentState [] 0 Map.empty 0 0

type Edge = Graph.LEdge Connection
type Vertex = (Int, Node)

parse :: Expr -> Either String Experiment
parse (ExperimentExpr initialExprs) =
  evalState (runErrorT parseModel) emptyState
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

-- Generic parsing

-- | Parses an incomplete node block with the given name and index
parseNode :: Expr -> ModelState Node
parseNode (BlockExpr name label exprs) = do
  ((_, node), edges) <- case name of
    "population" -> do
      populationName <- labelToName label PopulationCounter
      parseNodeWithConnections (parsePopulationBody populationName) exprs
    "stimulus" -> do
      label <- labelToName label StimulusCounter
      vertex <- parseStimulusBody label exprs
      return (vertex, [])
    "response" -> parseNodeWithConnections parseResponseBody exprs
    _ -> throwError $ "Expected 'population', 'stimulus' or 'response' but found " ++ name
  return node
  where
    labelToName :: Maybe String -> Counter -> ModelState String
    labelToName (Just name) _ = pure name
    labelToName Nothing counter =
      (++) <$> pure (show counter) <*> fmap show (getAndIncrement counter)

-- | Parses a backend from a 'BlockExpr'
parseBackend :: Expr -> ModelState Backend
parseBackend (BlockExpr "backend" (Just target) [FieldExpr "runtime" (RealExpr runtime)]) =
  let
    executionTarget = case target of
      "nest" -> Myelin.Nest 0 100
      "brainscales" -> Myelin.BrainScaleS 33 297
  in return $ Myelin executionTarget runtime
parseBackend s = throwError $ "Expected backend, but got " ++ (show s)

-- | Parses a 'Response' or a 'Population' from a 'BlockExpr'. Both are required
--   to have one or more 'Connection's
parseNodeWithConnections :: ([Expr] -> ModelState Node) -- ^ A parser for the node body
                         -> [Expr] -- ^ The expressions within the node
                         -> ModelState (Vertex, [Edge]) -- ^ The parsed node and its edges
parseNodeWithConnections parser body =
  let (connectionsExprs, fieldExprs) = partition isConnection body
  in if length connectionsExprs == 0 then throwError "No connections found" else do
         node <- parser fieldExprs
         nodeId <- getAndIncrementIndex
         let vertex = (nodeId, node)
         edges <- mapM (parseConnection vertex) connectionsExprs
         return (vertex, edges)
  where
    isConnection :: Expr -> Bool
    isConnection (BlockExpr "from" (Just _) _) = True
    isConnection _ = False

parsePopulationBody :: String -> [Expr] -> ModelState Node
parsePopulationBody name body = return $ Population name 10

parseResponseBody :: [Expr] -> ModelState Node
parseResponseBody _ = return Response

-- Stimulus

parseStimulusBody :: String -> [Expr] -> ModelState Vertex
parseStimulusBody name [source] = do
  source <- parseDataSource source
  nodeId <- getAndIncrementIndex
  return $ (nodeId, Stimulus name source)
parseStimulusBody name [] = throwError $ "Stimulus " ++ name ++ " requires a data source, but none was given"
parseStimulusBody name s = throwError $ "Too many elements for stimulus " ++ name ++ ": " ++ show s

parseDataSource :: Expr -> ModelState DataSource
parseDataSource (FieldExpr "file" (StringExpr fileName)) = return (File fileName)
parseDataSource (FieldExpr "array" l@(ListExpr _)) = fmap Array (parseNumberList l)
parseDataSource _ = throwError "Error"

-- | Parses a connection
parseConnection :: Vertex -> Expr -> ModelState Edge
parseConnection to (BlockExpr "from" (Just name) exprs) = do
  connection <- case exprs of
    [RealExpr weight] -> return $ Connection (double2Float weight)
    [] -> return $ Connection 1
    s -> throwError $ "Connection to " ++ name ++ " only supports a weight attribute, but received several: " ++ show s
  addEdge name to connection
parseConnection _ (BlockExpr "from" Nothing _) =
  throwError "Connection requires the name of the source population or simuli. None was given"
parseConnection _ s = throwError $ "Expected connection, but got " ++ show s

-- Internal

data Counter = Index | PopulationCounter | StimulusCounter
instance Show Counter where
  show Index = "index"
  show PopulationCounter = "population"
  show StimulusCounter = "stimulus"

parseNumberList :: Expr -> ModelState [Float]
parseNumberList (ListExpr list) = return $ listToFloats list
  where
    listToFloats ((RealExpr first):rest) = (realToFrac first) : (listToFloats rest)
    listToFloats [] = []
parseNumberList t = throwError $ "Expected list of numbers, but got " ++ (show t)

getAndIncrement :: Counter -> ModelState Int
getAndIncrement counter =
  do  state <- get
      let value = valueFromState state + 1
      put $ case counter of
        Index -> state { index = value }
        PopulationCounter -> state { populationCounter = value }
        StimulusCounter -> state { stimulusCounter = value }
      return value
  where
    valueFromState state = case counter of
      Index -> index state
      PopulationCounter -> populationCounter state
      StimulusCounter -> stimulusCounter state

getAndIncrementIndex :: ModelState Int
getAndIncrementIndex = getAndIncrement Index

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
