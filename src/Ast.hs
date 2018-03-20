module Ast
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
  }

newtype SurfacePhenomena = SurfacePhenomena
  { strategies :: [As] }
newtype Input = Input
  { inputFeatures :: Features }
newtype Output = Output
  { outputFeatures :: Features }

-- | An algorithmic strategy containing a number of EFs
newtype As = As
  { efs :: Int
  }

-- | An elementary function (EF) consisting of a given number of neurons
--   and the probability threshold of connecting two neurons in the EF
data Ef = Ef
  { neurons :: Int
  , connectivityProbability :: Float
  }

-- | A number of boolean features to express in the model
type Features = Int
