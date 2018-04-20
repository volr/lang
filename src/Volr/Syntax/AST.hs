module Volr.Syntax.AST where

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
  | RealExpr Double
  | StringExpr String
  deriving (Eq, Show)
