module Volr.Backend (Backend(Futhark, Myelin), run) where

import Volr.Ast

import Volr.Backend.Futhark
import Volr.Backend.Myelin

run :: Backend -> Model -> Either String (IO String)
run Futhark model = runFuthark model
run Myelin model = runMyelin model
