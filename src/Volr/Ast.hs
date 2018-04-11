{-# LANGUAGE ExistentialQuantification #-}

module Volr.Ast
  ( Model(Model)
  , Function(Function)
  , Stimulus(Stimulus)
  , Response(Response)
  , Features
  , Stimulatable(Stimulatable)
  , WithStimulus(features)
  )
 where

import Data.Typeable

  -- | A model of the learning process
data Model = Model Response deriving (Eq, Show)

class WithStimulus a where
  features :: a -> Features

data Function
  = Function String [Stimulatable] Int
  deriving (Eq, Show)

instance WithStimulus Function where
  features (Function _ s _) = foldl (+) 0 (map (\(Stimulatable a) -> features a) s)

data Stimulus
  = Stimulus String Features String
  deriving (Eq, Show)

instance WithStimulus Stimulus where
  features (Stimulus _ f _) = f

data Response
  = Response [Stimulatable] String Float
  deriving (Eq, Show)

data Stimulatable = forall a. (Show a, Eq a, Typeable a, WithStimulus a) => Stimulatable a
instance Show Stimulatable where
  show (Stimulatable s) = show s
instance Eq Stimulatable where
  Stimulatable a == Stimulatable b = maybe False (== b) (cast a)

-- | A number of boolean features to express in the model
type Features = Int
