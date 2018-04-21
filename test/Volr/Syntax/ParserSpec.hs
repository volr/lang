module Volr.Syntax.ParserSpec (main, spec) where

import Control.Monad.Identity
import Control.Monad.State.Lazy

import Data.Either (isLeft)
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

parseSuccess :: String -> Parser Expr -> Expr -> IO ()
parseSuccess code parser expected = do
    let r = P.runParser parser "" code
    r `shouldBe` (Right expected)

parseFail :: String -> Parser Expr -> IO ()
parseFail code parser = isLeft (P.runParser parser "" code) `shouldBe` True

spec :: Spec
spec = do
  describe "The AST parser" $ do
    -- BlockExpr
    --   { category :: Maybe String
    --   , label :: String
    --   , entries :: [Expr]
    --   }
    -- | ExperimentExpr [Expr]
    -- | FieldExpr String Expr
    -- | ListExpr [Expr]

    it "can parse an empty experiment" $ do
      parse "" `shouldBe` (Right (ExperimentExpr []))

-- Aggregations

    -- it "can parse a field" $ do
    --   let (r, s) = runState (P.runParserT parseField 1 "" "") Map.empty
    --   r `shouldBe` (Right (IntExpr ))
    --   s `shouldBe` (Map.empty)
    --
    -- it "can fail to parse a field with wrong indentation" $ do
    --   let (r, s) = runState (P.runParserT parseInt "" "101") Map.empty
    --   r `shouldBe` (Right (IntExpr 101))
    --   s `shouldBe` (Map.empty)
    --
    it "can parse a field" $ do
      parseSuccess "test: value" (parseField parseString) (FieldExpr "test" (StringExpr "value"))

    it "can parse a list of scalars" $ do
      parseSuccess "[1, 2, 3]" (parseList parseScalar) (ListExpr [RealExpr 1, RealExpr 2, RealExpr 3])

    it "can parse an empty list" $ do
      parseSuccess "[]" (parseList parseScalar) (ListExpr [])

-- Scalars

    it "can parse a integer" $ do
      parseSuccess "101" parseInt (IntExpr 101)

    it "can parse a quantity" $ do
      parseSuccess "10.2 ms" parseQuantity (QuantityExpr (RealExpr 10.2) (StringExpr "ms"))

    it "can parse a real number" $ do
      parseSuccess "-10.2" parseReal (RealExpr (-10.2))

    it "can parse a real number without consuming characters after the number" $ do
      parseSuccess "1]" (parseReal *> (IntExpr <$> (fmap fromIntegral P.getTokensProcessed))) (IntExpr 1)

    it "can parse a real number without consuming spaces after the number" $ do
      parseSuccess "1  " (parseReal *> (IntExpr <$> (fmap fromIntegral P.getTokensProcessed))) (IntExpr 1)

    it "can parse a string" $ do
      parseSuccess "s101" parseString (StringExpr "s101")
