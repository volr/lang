{-# LANGUAGE RecordWildCards #-}

module Volr.Backend.Myelin (runMyelin) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Identity
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Typeable

import Volr.Ast

import Myelin.SNN
import qualified Myelin.Spikey

runMyelin :: Model -> Either String (IO String)
runMyelin model@(Model _ (Target (Myelin target@(Nest _ _)) _ _)) =
  let snn = parseMyelin model
      task = toTask snn target 100.0
  in Right $ pure $ taskToJSON task
runMyelin model@(Model _ (Target b _ _)) = Left $ "Unsupported backend " ++ (show b)

parseMyelin :: Model -> SNN () Identity
parseMyelin (Model (Response xs) (Target _ source outputFile)) = do
    output <- fileOutput outputFile
    sequence_ $ map (\n -> projectRecursively source output (AllToAll 1.0 False) (Static Excitatory) n) xs

projectRecursively :: DataSource -> Node -> ProjectionType -> ProjectionTarget -> Stimulatable -> SNN () Identity
projectRecursively source to t a (Stimulatable s) = case (cast s :: Maybe Stimulus) of
  Just (Stimulus name features) -> do
    from <- case source of
      Array xs -> spikeSourceArray xs
      File inputFile -> fileInput inputFile
    projection t a from to
  _ -> case (cast s :: Maybe Function) of
    Just (Function name xs features) -> do
      from <- population (toInteger features) if_current_alpha_default name
      projection t a from to
      sequence_ $ map (\ss -> projectRecursively source from t a ss) xs
    _ -> return ()
