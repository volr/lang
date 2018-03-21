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

class WithFeatures a where
  inputFeatures :: a -> Int
  outputFeatures :: a -> Int

class WithStrategies a where
  strategies :: a -> [As]
  -- TODO: pickStrategy :: [As] -> As

data SurfacePhenomena = SurfacePhenomena
  { inputFeaturesPhenomena :: Int
  , outputFeaturesPhenomena :: Int
  , strategiesFromPhenomena :: [As]
  } deriving (Eq, Show)

instance WithStrategies SurfacePhenomena where
  strategies = strategiesFromPhenomena

data Input = Input
  { inputFeaturesFromInput :: Features
  , outputFeaturesFromInput :: Features
  } deriving (Eq, Show)

instance WithFeatures Input where
  inputFeatures = inputFeaturesFromInput
  outputFeatures = outputFeaturesFromInput

data Output = Output
  { inputFeaturesFromOutput :: Features
  , outputFeaturesfromOutput :: Features
  } deriving (Eq, Show)

instance WithFeatures Output where
  inputFeatures = inputFeaturesFromOutput
  outputFeatures = outputFeaturesfromOutput

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
