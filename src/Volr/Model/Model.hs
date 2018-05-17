{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Volr.Model.Model where

import Data.Graph.Inductive hiding(Node)
import Data.Typeable
import Myelin.SNN (ExecutionTarget)

-- | A neural network experiment
data Experiment = Experiment Model [Backend] deriving (Eq, Show)

-- | A model of the neural network itself, represented as a 'Graph' that
--   can be cyclic.
type Model = Gr Node Connection

-- | A node in the neural network graph, connected by 'Connection's
data Node
-- | A response that records a number of 'Population's or 'Stimulus'
  = Response
  -- | A group of neurons that connects to other neurons or stimuli
  | Population String Integer
  -- | A source of data that arrives from a 'DataSource'
  | Stimulus String DataSource
  deriving (Eq, Show)

-- | A connection between two 'Node's with a given weight.
--   If the weight is negative, the connection is treated as
--   inhibitory in spiking neural networks.
data Connection = Connection
  { weight :: Float
  } deriving (Eq, Ord, Show)

-- | The available backends on which the models can be evaluated.
data Backend
  = Myelin ExecutionTarget Double
  -- | Futhark String
  deriving (Eq, Show)

-- | A source of input data (stimulus)
data DataSource
  = File String
  | Array [Float]
  deriving (Eq, Show)
