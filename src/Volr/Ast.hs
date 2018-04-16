{-# LANGUAGE ExistentialQuantification #-}

module Volr.Ast
  ( Backend(Futhark, Myelin)
  , Connection
  , DataSource(File, Array)
  , Model(Model)
  , Function(Function)
  , Stimulus(Stimulus)
  , Response(Response)
  , Features
  , Stimulatable(Stimulatable)
  , Target(Target)
  , WithStimulus(features)
  )
 where

import Data.Typeable
import Myelin.SNN (ExecutionTarget, SynapseEffect)

data Backend
  = Futhark
  | Myelin ExecutionTarget
  deriving (Eq, Show)

data Target
  = Target Backend DataSource String
  deriving (Eq, Show)

data DataSource
  = File String
  | Array [Float]
  deriving (Eq, Show)

type Connection = (Stimulatable, SynapseEffect)

  -- | A model of the learning process
data Model = Model Response Target deriving (Eq, Show)

class WithStimulus a where
  features :: a -> Features

data Function
  = Function String [Connection] Int
  deriving (Eq, Show)

instance WithStimulus Function where
  features (Function _ s _) = foldl (+) 0 (map (\(Stimulatable a, _) -> features a) s)

data Stimulus
  = Stimulus String Features
  deriving (Eq, Show)

instance WithStimulus Stimulus where
  features (Stimulus _ f) = f

data Response
  = Response [Connection]
  deriving (Eq, Show)

data Stimulatable = forall a. (Show a, Eq a, Typeable a, WithStimulus a) => Stimulatable a
instance Show Stimulatable where
  show (Stimulatable s) = show s
instance Eq Stimulatable where
  Stimulatable a == Stimulatable b = maybe False (== b) (cast a)

-- | A number of boolean features to express in the model
type Features = Int
