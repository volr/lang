module Volr.Ast
  ( Model(Model)
  , SurfacePhenomena(SurfacePhenomena)
  , Input(Input)
  , Output(Output)
  , As(As)
  , Ef(Ef)
  , Features
  )
 where

  -- | A model consisting of modules
data Model = Model
  { input :: Input
  , modules :: [SurfacePhenomena]
  , output :: Output
  } deriving (Eq, Show)

newtype SurfacePhenomena = SurfacePhenomena
  { strategies :: [As] } deriving (Eq, Show)
newtype Input = Input
  { inputFeatures :: Features } deriving (Eq, Show)
newtype Output = Output
  { outputFeatures :: Features } deriving (Eq, Show)

-- | An algorithmic strategy containing a number of EFs
newtype As = As
  { efs :: Int
  } deriving (Eq, Show)

-- | An elementary function (EF) consisting of a given number of neurons
--   and the probability threshold of connecting two neurons in the EF
data Ef = Ef
  { neurons :: Int
  , connectivityProbability :: Float
  }

-- | A number of boolean features to express in the model
type Features = Int
