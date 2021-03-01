module Eval where

import Syntax

binaryOp :: Expr -> Expr -> (Double -> Double -> Double) -> Maybe Double
binaryOp e1 e2 op = do
  n1 <- eval e1
  n2 <- eval e2
  Just (n1 `op` n2)