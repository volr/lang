module Volr.Backend where

import Volr.Model.Model

-- import Volr.Backend.Futhark
import Volr.Backend.Myelin

run :: Experiment -> Either String (IO String)
-- run model@(Model _ (Ta!rget (Futhark _ _))) = runFuthark model
run experiment = runMyelin experiment
