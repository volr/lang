module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Semigroup ((<>))
import Options.Applicative

import System.IO (hPutStrLn, stderr)

import Volr.Model
import Volr.Parser
import Volr.Generator

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

generateBackend :: Experiment -> Configuration -> IO ()
generateBackend experiment configuration = do
  let (Configuration input output) = configuration
  case generate experiment of
    Left error -> hPutStrLn stderr error
    Right lazyResult ->
      let result = ByteString.Lazy.toStrict lazyResult
      in  case output of
            FileOutput file -> ByteString.writeFile file result
            StdOutput -> putStrLn $ show result

main :: IO ()
main = do
    configuration <- execParser configuration
    let (Configuration input _) = configuration
    content <- readInput input
    case parse content of
      Left error -> hPutStrLn stderr $ show error
      Right experiment -> generateBackend experiment configuration
  where
    configuration = info (parseConfig <**> helper)
      ( fullDesc
     <> progDesc "Parses a volr file and evaluates the model"
     <> header "volr - a DSL for neuroscientific machine learning models" )
