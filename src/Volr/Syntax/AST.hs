module Volr.Syntax.AST where

import Numeric.Units.Dimensional

data Expr
  = BlockExpr
    { category ::  String
    , label :: Maybe String
    , entries :: [Expr]
    }
  | ExperimentExpr [Expr]
  | FieldExpr String Expr
  | IntExpr Integer
  | ListExpr [Expr]
  | RealExpr Double
  | StringExpr String
  | QuantityExpr Expr Expr
  deriving (Eq, Show)
