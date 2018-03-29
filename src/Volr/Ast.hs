module Volr.Ast
  ( Model(Model)
  , Strategy(Strategy)
  , SelectionCriterion(Accuracy, Random)
  , Stimuli(Stimuli)
  , Response(Response)
  , Features
  )
 where

  -- | A model of the learning process
data Model = Model
  { strategies :: [Strategy]
  , response :: Response
  } deriving (Eq, Show)

data Strategy
  = Strategy String [Stimuli] Int
  deriving (Eq, Show)

data SelectionCriterion
  = Accuracy
  | Random
  deriving (Eq, Show)

data Stimuli
  = Stimuli String String Features
  | StrategyStimuli Strategy
  deriving (Eq, Show)

data Response
  = Response String Features
  deriving (Eq, Show)

-- | A number of boolean features to express in the model
type Features = Int
