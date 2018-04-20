module Volr.Syntax.ParserSpec (main, spec) where

import Control.Monad.State.Lazy
import Data.Map as Map

import Text.Megaparsec (ParseError)
import qualified Text.Megaparsec as P

import Test.Hspec
import qualified Test.QuickCheck

import Myelin.SNN
import Volr.Syntax.AST
import Volr.Syntax.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The AST parser parser" $ do
    -- BlockExpr
    --   { category :: Maybe String
    --   , label :: String
    --   , entries :: [Expr]
    --   }
    -- | ExperimentExpr [Expr]
    -- | FieldExpr String Expr
    -- | IntExpr Integer
    -- | ListExpr [Expr]
    -- | RealExpr
    -- | StringExpr String

    it "can parse an empty experiment" $ do
      parse "" `shouldBe` (Right (ExperimentExpr []))

-- Scalars

    it "can parse a integer" $ do
      let (r, s) = runState (P.runParserT parseInt "" "101") Map.empty
      r `shouldBe` (Right (IntExpr 101))
      s `shouldBe` (Map.empty)

    it "can parse a real" $ do
      let (r, s) = runState (P.runParserT parseReal "" "10.2") Map.empty
      r `shouldBe` (Right (RealExpr 10.2))
      s `shouldBe` (Map.empty)

    it "can parse a string" $ do
      let (r, s) = runState (P.runParserT parseString "" "s101") Map.empty
      r `shouldBe` (Right (StringExpr "s101"))
      s `shouldBe` (Map.empty)
