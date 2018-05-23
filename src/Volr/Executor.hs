module Volr.Executor where

import Volr.Model

-- import Volr.Backend.Futhark
import Volr.Target.Myelin

execute :: Experiment -> Either String (IO String)
-- run model@(Model _ (Ta!rget (Futhark _ _))) = runFuthark model
execute = runMyelin
