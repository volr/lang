module Volr.Backend (Backend(Futhark, Myelin), run) where

import Volr.Ast

import Volr.Backend.Futhark
import Volr.Backend.Myelin

run :: Model -> Either String (IO String)
run model@(Model _ (Target (Futhark _ _))) = runFuthark model
run model = runMyelin model
