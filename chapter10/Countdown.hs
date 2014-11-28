module Countdown where

import Control.Monad (replicateM)
import Data.List

data Operator = Add | Sub | Mult | Div deriving (Show, Eq)
data Expr = Val Int | Expr Expr Operator Expr deriving Show

evaluate :: Expr -> Int
evaluate (Val n) = n
evaluate (Expr x Add y) = evaluate x + evaluate y
evaluate (Expr x Sub y) = evaluate x - evaluate y
evaluate (Expr x Mult y) = evaluate x * evaluate y
evaluate (Expr x Div y) = evaluate x `div` evaluate y

isLegalExpr :: Expr -> Bool
isLegalExpr (Val _) = True
isLegalExpr (Expr x Sub y) = isLegalExpr x && isLegalExpr y &&
                             evaluate x >= evaluate y
isLegalExpr (Expr x Div y) = isLegalExpr x && isLegalExpr y &&
                             evaluate y /= 0 &&
                             evaluate x `mod` evaluate y == 0
isLegalExpr (Expr x _ y) = isLegalExpr x && isLegalExpr y

isSolution :: Int -> Expr -> Bool
isSolution v e = isLegalExpr e && evaluate e == v

permSubseqs :: Eq a => [a] -> [[a]]
permSubseqs = nub . filter (/= []) . concatMap permutations . subsequences

-- This function  takes an input list of ints and a list of operators
-- and returns all possible expressions that can be generated where
-- each int is used at most once and each operator is used as many
-- times as desired.
exprs :: [Int] -> [Operator] -> [Expr]
exprs ns os = concatMap (flip exprs' os) permNs
  where
    permNs = filter (/= []) $ permSubseqs ns

-- This function takes an input list of ints and a list of operators
-- and returns all possible expressions that can be generated where
-- each int is used exactly once and each operator is used as many
-- times as desired.
exprs' :: [Int] -> [Operator] -> [Expr]
exprs' ns os = map (expr ns) permOs
  where
    permOs = filter (/= []) $ replicateM (length ns - 1) os

-- This function takes an input list of ints and a list of operators
-- and returns an expression where each int and each operator is used
-- exactly once. This function assumes that the number of operators
-- provided is one less than the number of ints provided.
expr :: [Int] -> [Operator] -> Expr
expr [] _ = error "Invalid input"
expr [n] [] = Val n
expr [m,n] [o] = Expr (Val m) o (Val n)
expr (m:n:ns) (o1:o2:os) = Expr (expr [m,n] [o1]) o2 (expr ns os)

findSolutions :: [Int] -> [Operator] -> Int -> [Expr]
findSolutions ns os v = filter (isSolution v) $ exprs ns os
