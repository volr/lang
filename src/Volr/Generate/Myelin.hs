{-# LANGUAGE RecordWildCards #-}

module Volr.Generate.Myelin (generateMyelin) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Identity

import Data.Graph.Inductive.Graph as Graph
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Typeable

import Volr.Model as Volr

import qualified Myelin.SNN as Myelin
import qualified Myelin.Spikey

generateMyelin :: Experiment -> Either String Myelin.Task
generateMyelin (Experiment graph [Volr.Myelin target runtime]) =
  case target of
    (Myelin.Spikey _) -> Left $ "Unsupported backend " ++ (show target)
    Myelin.SpiNNaker -> Left $ "Unsupported backend " ++ (show target)
    _ -> let (_, snnBlock) = runState (parseMyelin graph) Myelin.initialBlockState
             network  = Myelin.Network [snnBlock]
             task = Myelin.Task target network runtime
         in Right task
generateMyelin model@(Experiment t b) = Left $ "Unsupported target and backend " ++ (show model)

parseMyelin :: Model -> Myelin.SNN () Identity
parseMyelin graph =
  do  let graphNodes = Graph.labNodes graph  -- Get labelled nodes
      myelinNodes <- defineNodes graphNodes
      let nodeMap = Map.fromList myelinNodes
      let nodeEdges = concat $ map (getLabelledSuc nodeMap) myelinNodes -- ... and edges
      projectEdges nodeEdges
  where
    getLabelledSuc :: Map.Map Int Myelin.Node -> (Int, Myelin.Node) -> [(Myelin.Node, Myelin.Node, Connection)]
    getLabelledSuc nodeMap (nodeId, fromNode) =
      let graphEdges = Graph.lsuc graph nodeId
          volrEdges = map (\(edgeId, connection) -> (nodeMap Map.! edgeId, connection)) graphEdges
      in  map (\(toNode, c) -> (fromNode, toNode, c)) volrEdges

defineNodes :: [(Int, Volr.Node)] -> Myelin.SNN [(Int, Myelin.Node)] Identity
defineNodes nodes =
  do  myelinNodes <- sequence $ map defineNode $ map snd nodes
      return $ zip (map fst nodes) $ myelinNodes
  where
    defineNode :: Volr.Node -> Myelin.SNN Myelin.Node Identity
    defineNode (Stimulus _ source) =
      case source of
        Volr.Array xs -> Myelin.spikeSourceArray xs
        Volr.File inputFile -> Myelin.fileInput inputFile
    defineNode (Population name neurons) =
      Myelin.population name (toInteger neurons) Myelin.if_cond_exp 
    defineNode Response =
      Myelin.fileOutput "file"

projectEdges :: [(Myelin.Node, Myelin.Node, Connection)] -> Myelin.SNN () Identity
projectEdges edges =
  do  sequence $ map edgeToProjection edges
      return ()
  where
    edgeToProjection :: (Myelin.Node, Myelin.Node, Connection) -> Myelin.SNN () Identity
    edgeToProjection (from, to, (Connection weight)) =
      let effect = if weight >= 0 then Myelin.Excitatory else Myelin.Inhibitory
      in  Myelin.projection (Myelin.AllToAll weight False) (Myelin.Static effect) from to

-- projectRecursively :: Myelin.Node -> Connection -> Myelin.SNN () Identity
-- projectRecursively to (Connection target weight) = case target of
--   Stimulus name source -> do
--     from <- case source of
--       Array xs -> Myelin.spikeSourceArray xs
--       File inputFile -> Myelin.fileInput inputFile
--     Myelin.projection (Myelin.AllToAll weight False) (Myelin.Static effect) from to
--   Population name neurons connections -> do
--     Myelin.projection (Myelin.AllToAll weight False) (Myelin.Static effect) from to
--     sequence_ $ map (\(newConnection) -> projectRecursively from newConnection) connections
--   where
--     effect = if weight >= 0 then Myelin.Inhibitory else Myelin.Excitatory
