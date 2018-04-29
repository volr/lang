module Volr.ParserSpec (main, spec) where

import Control.Monad.State.Lazy
import Data.Map as Map

import Text.Megaparsec (ParseError)
import qualified Text.Megaparsec as P

import Test.Hspec
import qualified Test.QuickCheck

import Myelin.SNN
import Volr.Parser
import Volr.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The model parser" $ do

    it "can parse a block to a stimulus" $ do
      
