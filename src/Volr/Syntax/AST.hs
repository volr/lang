module Volr.Syntax.AST where

import Numeric.Units.Dimensional

data Expr
  = BlockExpr
    { category :: Maybe String
    , label :: String
    , entries :: [Expr]
    }
  | ExperimentExpr [Expr]
  | FieldExpr String Expr
  | IntExpr Integer
  | ListExpr [Expr]
  | StringExpr String
  | QuantityExpr Double Expr
  deriving (Eq, Show)
