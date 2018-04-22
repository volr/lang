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

parseSuccess :: (Eq a, Show a) => String -> Parser a -> a -> IO ()
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

--     it "can parse an empty experiment" $ do
--       parse "" `shouldBe` (Right (ExperimentExpr []))
--
--     it "can parse a block without a label or fields" $ do
--       parseSuccess "some" parseBlock (BlockExpr "some" Nothing [])
--
--     it "can parse a block with a label but without fields" $ do
--       parseSuccess "some a" parseBlock (BlockExpr "some" (Just "a") [])
--
--     it "can parse a block with without a label and with a single field" $ do
--       parseSuccess "some\n  thing: x" parseBlock (BlockExpr "some" Nothing [FieldExpr "thing" (StringExpr "x")])
--
--     it "can parse a block with a label and a single field, indented with two spaces" $ do
--       parseSuccess "some a\n  thing: x" parseBlock (BlockExpr "some" (Just "a") [FieldExpr "thing" (StringExpr "x")])
--
--     it "can parse a block with a label and a single field, indented with four spaces" $ do
--       parseSuccess "some a\n    thing: x" parseBlock (BlockExpr "some" (Just "a") [FieldExpr "thing" (StringExpr "x")])
--
--     it "can parse a block with a label and a single field, indented with tab" $ do
--       parseSuccess "some a\n\tthing: x" parseBlock (BlockExpr "some" (Just "a") [FieldExpr "thing" (StringExpr "x")])
--
--     it "can parse a block with a label and multiple fields" $ do
--       parseSuccess "some a\n  thing: x\n  else: y" parseBlock (BlockExpr "some" (Just "a") [FieldExpr "thing" (StringExpr "x"), FieldExpr "else" (StringExpr "y")])
--
--     it "can fail to parse a block with wrong indentation" $ do
--       parseFail "some a\nthing: x" parseBlock
--
--     it "can fail to parse a block with differing indentation" $ do
--       parseFail "some a\n  thing: x\n    else: y" parseBlock
--
-- Aggregations

    it "can parse a field" $ do
      parseSuccess "test: value" (parseField parseName) (FieldExpr "test" (StringExpr "value"))

    it "can parse a list of scalars" $ do
      parseSuccess "[1, 2, 3]" (parseList parseScalar) (ListExpr [IntExpr 1, IntExpr 2, IntExpr 3])

    it "can parse an empty list" $ do
      parseSuccess "[]" (parseList parseScalar) (ListExpr [])

    -- it "can parse a list with regular indentation" $ do
    --   parseSuccess "[1,\n 2,\n 3]" (parseList parseNumber) (ListExpr [IntExpr 1, IntExpr 2, IntExpr 3])
    --
    -- it "can fail to parse a list with irregular indentation" $ do
    --   parseFail "[1,\n  2,\n 3]" (parseList parseNumber)
    --
    -- it "can parse a list with haskell-like indentation" $ do
    --   parseSuccess "[ 1\n, 2\n, 3\n]" (parseList parseNumber) (ListExpr [IntExpr 1, IntExpr 2, IntExpr 3])

-- Scalars

    it "can parse a integer" $ do
      parseSuccess "101" parseNumber (IntExpr 101)

    it "can parse a name" $ do
      parseSuccess "hey-there" parseName (StringExpr "hey-there")

    it "can parse a quantity" $ do
      parseSuccess "10.2 ms" parseQuantity (QuantityExpr (RealExpr 10.2) (StringExpr "ms"))

    it "can parse a real number" $ do
      parseSuccess "-10.2" parseNumber (RealExpr (-10.2))

    it "can parse a real number without consuming characters after the number" $ do
      parseSuccess "1]" (parseNumber *> (IntExpr <$> (fmap fromIntegral P.getTokensProcessed))) (IntExpr 1)

    it "can parse a real number without consuming spaces after the number" $ do
      parseSuccess "1  " (parseNumber *> (IntExpr <$> (fmap fromIntegral P.getTokensProcessed))) (IntExpr 1)

    it "can parse a scalar to an int before a real" $ do
      parseSuccess "10" parseScalar (IntExpr 10)

    it "can parse a string" $ do
      parseSuccess "s101" parseString "s101"
