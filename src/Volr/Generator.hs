module Volr.Generator where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text

import Volr.Model

-- import Volr.Backend.Futhark
import Volr.Generate.Myelin

generate :: Experiment -> Either String ByteString
generate _ = Left "Error"
-- run model@(Model _ (Ta!rget (Futhark _ _))) = runFuthark model
-- generate = fmap encode . generateMyelin
