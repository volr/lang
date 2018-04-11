{-# LANGUAGE RecordWildCards #-}

module Volr.Backend.Myelin (runMyelin) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Typeable

import Volr.Ast

import Myelin.SNN

runMyelin :: Model -> Either String (IO String)
runMyelin model =
  let s = parseMyelin model
  in Right $ evalStateT s initialBlockState

parseMyelin :: Model -> SNN String
parseMyelin (Model (Response xs fileName learningRate)) = do
    output <- fileOutput fileName
    res <- sequence $ map (\n -> projectRecursively output AllToAll n) xs
    s <- get
    pure $ T.unpack $ TL.toStrict $ renderNetwork s

projectRecursively :: Node -> ProjectionType -> Stimulatable -> SNN ()
projectRecursively to t (Stimulatable s) = case (cast s :: Maybe Stimulus) of
  Just (Stimulus name features fileName) -> do
    from <- fileInput fileName
    projection AllToAll from to
  _ -> case (cast s :: Maybe Function) of
    Just (Function name xs features) -> do
      from <- population (toInteger features) if_current_alpha_default name
      projection AllToAll from to
      sequence_ $ map (\ss -> projectRecursively from t ss) xs
    _ -> return ()
