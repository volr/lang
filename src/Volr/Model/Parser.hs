{-# LANGUAGE RecordWildCards #-}

module Volr.Model.Parser where

import Control.Monad.Error
import Control.Monad.State.Lazy
import Data.Functor.Identity
import qualified Data.Map.Strict as Map

import qualified Myelin.SNN as Myelin

import Volr.Model.Model
import Volr.Syntax.AST

type ModelState = ErrorT String (State ExperimentState)

data ExperimentState = ExperimentState
  { populationIndex :: Int
  , responseIndex :: Int
  , stimulusIndex :: Int
  , nodes :: Map.Map String Node
  }

parse :: Expr -> Either String Model
parse (ExperimentExpr preStimuli) =
  let
    initialState = ExperimentState 0 0 0 Map.empty
  in evalState (runErrorT parseModel) initialState
  where
    parseModel :: ModelState Model
    parseModel = do
      (postStimuli, _) <- parseUntilError preStimuli [] parseStimulus
      (postPopulations, _) <- parseUntilError postStimuli [] parsePopulation
      (postResponses, responses) <- parseUntilError postPopulations [] parseResponse
      (_, backends) <- parseUntilError postResponses [] parseBackend
      case (responses, backends) of
        ([response], [backend]) -> pure $ Model response backend
        (rs, _) -> throwError $ "Expected 1 response but got " ++ (show (length rs))
        (_, bs) -> throwError $ "Expected 1 backend but got " ++ (show (length bs))
parse s = Left $ "Expected full experiment, but got " ++ (show s)

parseBackend :: Expr -> ModelState Backend
parseBackend (BlockExpr "backend" (Just target) [FieldExpr "runtime" (RealExpr runtime)]) =
  let
    executionTarget = case target of
      "nest" -> Myelin.Nest 0 100
      "brainscales" -> Myelin.BrainScaleS 33 297
  in return $ Myelin executionTarget runtime
parseBackend s = throwError $ "Expected backend, but got " ++ (show s)

parseRetainError :: [Expr] -> [a] -> (Expr -> ModelState a) -> ModelState [a]
parseRetainError exprs@(next:remaining) parsed parser = do
  result <- parser next
  parseRetainError remaining (result:parsed) parser

parseUntilError :: [Expr] -> [a] -> (Expr -> ModelState a) -> ModelState ([Expr], [a])
parseUntilError exprs@(next:remaining) parsed parser = do
    result <- parser next
    return (remaining, (result:parsed))
  `catchError` (\err -> return (exprs, parsed))

parseConnection :: Expr -> ModelState Connection
parseConnection (BlockExpr "from" label [StringExpr name]) = do
  target <- getNode name
  return $ Connection target 1
parseConnection s = throwError $ "Expected connection, but got " ++ (show s)

parsePopulation :: Expr -> ModelState Node
--parseBlock (BlockExpr "stimulus" label entries) =
parsePopulation expr = throwError ("Unknown block " ++ (show expr))

parseResponse :: Expr -> ModelState Response
parseResponse (BlockExpr "response" label connections) = do
  connections <- parseRetainError connections [] parseConnection
  return $ Response connections
parseResponse s = throwError $ "Expected response, but got " ++ (show s)

parseStimulus :: Expr -> ModelState Node
parseStimulus (BlockExpr "stimulus" label [source]) = do
  name <- case label of
    Just n -> pure n
    Nothing -> do
      index <- getAndIncrementIndex StimulusIndex
      return $ "stimulus" ++ (show index)
  source <- parseDataSource source
  return $ Stimulus name source
parseStimulus s = throwError $ "Expected stimulus, but got " ++ (show s)

parseDataSource :: Expr -> ModelState DataSource
parseDataSource (FieldExpr "file" (StringExpr fileName)) = return (File fileName)
parseDataSource (FieldExpr "array" (ListExpr xs)) = fmap Array (parseNumberList xs)
parseDataSource _ = throwError "Error"

parseNumberList :: [Expr] -> ModelState [Float]
parseNumberList ((RealExpr d):rest) = fmap (\t -> ((realToFrac d):t)) (parseNumberList rest)
parseNumberList [] = return []

-- Internal

data Index
  = PopulationIndex
  | ResponseIndex
  | StimulusIndex
  deriving (Eq)

getAndIncrementIndex :: Index -> ModelState Int
getAndIncrementIndex stateIndex = do
  state <- get
  let index = (if stateIndex == PopulationIndex then (populationIndex state) else if stateIndex == ResponseIndex then (responseIndex state) else (stimulusIndex state)) + 1
  put $ case stateIndex of
    PopulationIndex -> state {populationIndex = index + 1}
    ResponseIndex -> state {responseIndex = index + 1}
    StimulusIndex -> state {stimulusIndex = index + 1}
  return index

addNode :: String -> Node -> ModelState ()
addNode key value = do
  state <- get
  put $ state { nodes = Map.insert key value (nodes state)}
  return ()

getNode :: String -> ModelState Node
getNode name = do
  state <- get
  case (nodes state) Map.!? name of
    Nothing -> throwError $ "Could not find module of name " ++ name
    Just m -> return m
