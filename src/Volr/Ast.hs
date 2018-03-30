{-# LANGUAGE ExistentialQuantification #-}

module Volr.Ast
  ( Model(Model)
  , Strategy(Strategy)
  , SelectionCriterion(Accuracy, Random)
  , Stimuli(Stimuli)
  , Response(Response)
  , Features
  , Stimulatable(Stimulatable)
  )
 where

import Data.Typeable

  -- | A model of the learning process
data Model = Model
  { strategies :: [Strategy]
  , response :: Response
  } deriving (Eq, Show)

class WithStimuli a where
  features :: a -> Features

data Strategy
  = Strategy String [Stimulatable] Int
  deriving (Eq, Show)

instance WithStimuli Strategy where
  features (Strategy _ s _) = foldl (+) 0 (map (\(Stimulatable a) -> features a) s)

data SelectionCriterion
  = Accuracy
  | Random
  deriving (Eq, Show)

data Stimuli
  = Stimuli String Features String
  deriving (Eq, Show)

instance WithStimuli Stimuli where
  features (Stimuli _ f _) = f

data Response
  = Response [Stimulatable] String SelectionCriterion
  deriving (Eq, Show)

data Stimulatable = forall a. (Show a, Eq a, Typeable a, WithStimuli a) => Stimulatable a
instance Show Stimulatable where
  show (Stimulatable s) = show s
instance Eq Stimulatable where
  Stimulatable a == Stimulatable b = maybe False (== b) (cast a)

-- | A number of boolean features to express in the model
type Features = Int
