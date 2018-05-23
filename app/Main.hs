module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import System.IO (hPutStrLn, stderr)

import Volr.Model
import Volr.Parser
import Volr.Executor

data Configuration = Configuration
  { input :: Input
  , output :: Output
  }

data Input
  = StdInput
  | FileInput String

data Output
  = FileOutput String
  | StdOutput

parseFileInput :: Parser Input
parseFileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "input volr file"
  )

parseInput :: Parser Input
parseInput = parseFileInput
  <|> pure StdInput

parseFileOutput :: Parser Output
parseFileOutput = FileOutput <$> strOption
  (  long "output"
  <> short 'o'
  <> metavar "OUTPUT"
  <> help "redirect output to file")

parseOutput :: Parser Output
parseOutput = parseFileOutput <|> pure StdOutput

parseConfig :: Parser Configuration
parseConfig = Configuration
  <$> parseInput
  <*> parseOutput

readInput :: Input -> IO String
readInput (FileInput file) = readFile file
readInput StdInput = getContents

runBackend :: Experiment -> Configuration -> IO ()
runBackend experiment configuration = do
  let (Configuration input output) = configuration
  case execute experiment of
    Left error -> hPutStrLn stderr error
    Right resultIO -> do
      result <- resultIO
      case output of
        FileOutput file -> writeFile file result
        StdOutput -> putStrLn result

main :: IO ()
main = do
    configuration <- execParser configuration
    let (Configuration input _) = configuration
    content <- readInput input
    case parse content of
      Left error -> hPutStrLn stderr $ show error
      Right experiment -> runBackend experiment configuration
  where
    configuration = info (parseConfig <**> helper)
      ( fullDesc
     <> progDesc "Parses a volr file and evaluates the model"
     <> header "volr - a DSL for neuroscientific machine learning models" )
