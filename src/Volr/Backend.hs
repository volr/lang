module Volr.Backend (Backend(Futhark), run) where

import Volr.Ast

import Volr.Backend.Futhark

data Backend
  = Futhark

run :: Backend -> Model -> Either String (IO String)
run Futhark model = runFuthark model
