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

runMyelin :: Model -> Either String (IO String)
runMyelin model =
  let s = parseMyelin model
  in Right $ evalStateT s initialBlockState

parseMyelin :: Model -> SNN String IO
parseMyelin (Model (Response xs) (Target Myelin inputFile outputFile)) = do
    output <- fileOutput outputFile
    res <- sequence $ map (\n -> projectRecursively inputFile output (AllToAll 1.0 False) (Static Excitatory) n) xs
    s <- get
    pure $ T.unpack $ TL.toStrict $ renderNetwork s

projectRecursively :: String -> Node -> ProjectionType -> ProjectionTarget -> Stimulatable -> SNN () IO
projectRecursively inputFile to t a (Stimulatable s) = case (cast s :: Maybe Stimulus) of
  Just (Stimulus name features) -> do
    from <- fileInput inputFile
    projection t a from to
  _ -> case (cast s :: Maybe Function) of
    Just (Function name xs features) -> do
      from <- population (toInteger features) if_current_alpha_default name
      projection t a from to
      sequence_ $ map (\ss -> projectRecursively inputFile from t a ss) xs
    _ -> return ()
