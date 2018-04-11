module Volr.Backend.Futhark (runFuthark) where

import Control.Monad
import Data.Typeable
import Text.Printf
import System.Directory
import System.Process
import GHC.IO.Handle

import Volr.Ast

futharkBackendDir = "futhark-backend/src/"

runFuthark :: Model -> Either String (IO String)
runFuthark model = (fmap evaluateFuthark) $ generateFuthark model

futharkPreample :: String
futharkPreample
  = "import \"neuralnetwork\"\n\
    \import \"prediction\"\n"

futharkPostample :: String
futharkPostample
  = "module P = Predict (N)\n\
   \let main [n] [m] (x: [n][m]N.t) (y: [n]i32) : N.t = P.training_test x y"
--
data FutharkEvaluation = FutharkEvaluation
  { code :: String
  , xFile :: String
  , yFile :: String
  }

discoverFeatureList :: [Int] -> [Stimulatable] -> Either String ([Int], String)
discoverFeatureList list [Stimulatable x]=
    case (cast x :: Maybe Function) of
      Just (Function _ xs size) -> discoverFeatureList (size : list) xs
      _ -> case (cast x :: Maybe Stimulus) of
        Just (Stimulus _ features xFile) -> Right ((features : list), xFile)
        _ -> Left ("Unknown entity with stimulus " ++ (show x))
discoverFeatureList list _ = Left "Responses with anything but one input not yet supported"

featureListToFuthark :: [Int] -> Either String String
featureListToFuthark [s1, s2, s3, out]
  = Right $ printf "let size1 = %d\nlet size2 = %d\nlet size3 = %d\nlet output = %d" s1 s2 s3 out
featureListToFuthark [s1, s2, out]
  = Right $ printf "let size1 = %d\nlet size2 = %d\nlet output = %d" s1 s2 out
featureListToFuthark s = Left $ "Unsupported number of layers " ++ (show (length s))

generateFuthark :: Model -> Either String FutharkEvaluation
generateFuthark (Model (Response xs yFile learning_rate)) =
  let
    modulePrefix n = "module N = Network" ++ n ++ " (f64) {\n"
    modulePostfix = "\n}\n"
  in case discoverFeatureList [] xs of
    Left error -> Left error
    Right (list, xFile) -> fmap (\code -> FutharkEvaluation
      { code
          =  futharkPreample
          ++ (modulePrefix (show (length list)))
          ++ code
          ++ "\nlet learning_rate = " ++ (show learning_rate)
          ++ modulePostfix
          ++ futharkPostample
      , xFile = xFile
      , yFile = yFile }) $ featureListToFuthark list

evaluateFuthark :: FutharkEvaluation -> IO String
evaluateFuthark (FutharkEvaluation code xFile yFile) = do
  let fileName = "bin/futhark-model"
  createDirectoryIfMissing False "bin"
  writeFile (fileName ++ ".fut") code
  (_, _, _, compileProcess) <- createProcess (proc "futhark-opencl" [fileName ++ ".fut", "-I", futharkBackendDir]) {std_err = CreatePipe }
  waitForProcess compileProcess
  (_, Just fileOut, _, _) <- createProcess (proc "cat" [xFile, yFile]) { std_out = CreatePipe }
  (_, Just runOut, _, _) <- createProcess (proc fileName []) { std_in = UseHandle fileOut, std_out = CreatePipe}
  result <- hGetContents runOut
  return result
