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

import Volr.Model.Model

import qualified Myelin.SNN as Myelin
import qualified Myelin.Spikey

runMyelin :: Experiment -> Either String (IO String)
-- runMyelin model@(Model _ (Myelin target runtime)) =
--   case target of
--     (Myelin.Spikey _) -> Left $ "Unsupported backend " ++ (show target)
--     Myelin.SpiNNaker -> Left $ "Unsupported backend " ++ (show target)
--     _ -> let snn = parseMyelin model
--              task = Myelin.toTask snn target runtime
--          in Right $ pure $ Myelin.taskToJSON task
-- runMyelin model@(Model _ b) = Left $ "Unsupported backend " ++ (show b)
runMyelin _ = Left $ "Not implemented"
--
-- parseMyelin :: Model -> Myelin.SNN () Identity
-- parseMyelin (Model (Response xs) _) = do
--     output <- Myelin.fileOutput "__ignored__"
--     sequence_ $ map (\c -> projectRecursively output c) xs
--
-- projectRecursively :: Myelin.Node -> Connection -> Myelin.SNN () Identity
-- projectRecursively to (Connection target weight) = case target of
--   Stimulus name source -> do
--     from <- case source of
--       Array xs -> Myelin.spikeSourceArray xs
--       File inputFile -> Myelin.fileInput inputFile
--     Myelin.projection (Myelin.AllToAll weight False) (Myelin.Static effect) from to
--   Population name neurons connections -> do
--     from <- Myelin.population neurons Myelin.if_cond_exp_default name True
--     Myelin.projection (Myelin.AllToAll weight False) (Myelin.Static effect) from to
--     sequence_ $ map (\(newConnection) -> projectRecursively from newConnection) connections
--   where
--     effect = if weight >= 0 then Myelin.Inhibitory else Myelin.Excitatory
