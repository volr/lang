{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Volr.Model.Model where

import Data.Typeable
import Myelin.SNN (ExecutionTarget)

-- | A model of the learning process
data Model = Model Response Backend deriving (Eq, Show)

-- | A response that accumulates one of more node
newtype Response = Response [Connection] deriving (Eq, Show)

-- | A node is a group of neurons or a source of input stimulus.
--   Common for them both is that they /emit/ data in some form
data Node
  = Population String Integer [Connection]
  | Stimulus String DataSource
  deriving (Eq, Show)

-- | A connection /from/ the given 'Node' with a given weight. If the weight
--   is negative, the connection is treated as inhibitory in spiking neural
--   networks.
data Connection = Connection
  { target :: Node
  , weight :: Float
  } deriving (Eq, Show)

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
