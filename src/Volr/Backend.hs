module Volr.Backend where

import Volr.Model.Model

-- import Volr.Backend.Futhark
import Volr.Backend.Myelin

run :: Model -> Either String (IO String)
-- run model@(Model _ (Ta!rget (Futhark _ _))) = runFuthark model
run model = runMyelin model
