{-# LANGUAGE RecordWildCards #-}

module Volr.Model.Parser where

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

import Volr.Model.Model
import Volr.Syntax.AST

type ModelState = State ExperimentState

data ExperimentState = ExperimentState
  { populationIndex :: Int
  , responseIndex :: Int
  , stimulusIndex :: Int
  , blockNames :: Map.Map String Stimulatable
  }

parse :: Expr -> Either String Model
-- parse (ExperimentExpr experiments) =
parse s = Left $ "Expected full experiment, but got " ++ (show s)

parseBlock :: Expr -> ModelState (Either String Model)
--parseBlock (BlockExpr "stimulus" label entries) =
parseBlock expr = return $ Left ("Unknown block " ++ (show expr))

-- Internal
-- getAndIncrementStimulusIndex :: ModelState ()
-- getAndIncrementStimulusIndex = do
--   state{..} <- get
--
-- addBlockName :: String -> ModelState ()
-- addBlockName = do
--   state{..} <- get
