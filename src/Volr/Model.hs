{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Volr.Model
Description : The semantic model that represents Volr experiments

The model of Volr consists of an experiment with a neural network 'Model'
and one or more 'Target's that can execute the model.
-}
module Volr.Model where

import Data.Graph.Inductive hiding(Node)
import Data.Typeable

-- | A neural network experiment
data Experiment
  = Experiment
    { model :: Model
    , targets :: [Target]
    } deriving (Eq, Show)

-- | A model of the neural network itself, represented as a 'Graph' that
--   can be cyclic.
type Model = Gr Node Connection

-- | A node in the neural network graph, connected by 'Connection's
data Node
-- | A response that records a number of 'Population's or 'Stimulus'
  = Response
  -- | A group of neurons that connects to other neurons or stimuli
  | Population
    { name :: String -- ^ The name of the population
    , neurons :: Int -- ^ The number of neurons in the population
    -- TODO: Allow neuron archetypes
    }
  -- | A source of data that arrives from a 'DataSource'
  | Stimulus
    { name :: String -- ^ The name of the stimulus
    , source :: DataSource -- ^ The source of the stimulus data
    }
  deriving (Eq, Show)

-- | A connection between two 'Node's with a given weight.
--   If the weight is negative, the connection is treated as
--   inhibitory in spiking neural networks.
data Connection = Connection
  { weight :: Float
  } deriving (Eq, Ord, Show)

-- | The available target on which the models can be evaluated.
data Target
  = Futhark
  -- Myelin ExecutionTarget Double
  -- | Futhark String
  deriving (Eq, Show)

-- | A source of input data (stimulus)
data DataSource
  = File String
  | Array [Float]
  deriving (Eq, Show)
