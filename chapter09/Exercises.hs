module Exercises where

import Data.List
import Data.Char
import Data.Monoid
import Unsafe.Coerce

-- Exercise 0 --

data Nat = Zero
         | Succ Nat
         deriving Show

nat3 :: Nat
nat3 = (Succ (Succ (Succ Zero)))

nat5 :: Nat
nat5 = (Succ (Succ (Succ (Succ (Succ Zero)))))

myNatToInteger :: Nat -> Integer
myNatToInteger Zero = 0
myNatToInteger (Succ n) = 1 + myNatToInteger n

natToIntegerA :: Nat -> Integer
natToIntegerA Zero = 0
natToIntegerA (Succ n) = myNatToInteger n + 1

natToIntegerB :: Nat -> Integer
natToIntegerB (Succ n) = myNatToInteger n + 1
natToIntegerB Zero = 0

natToIntegerD :: Nat -> Integer
natToIntegerD (Succ n) = 1 + myNatToInteger n
natToIntegerD Zero = 0

natToIntegerF :: Nat -> Integer
natToIntegerF = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

natToIntegerG :: Nat -> Integer
natToIntegerG = \ n -> genericLength [c | c <- show n, c == 'S']

-- Won't compile since length returns a value of type `Int`
--natToIntegerH :: Nat -> Integer
--natToIntegerH = \ n -> length [c | c <- show n, c == 'S']

-- Exercise 1 --

myIntegerToNat :: Integer -> Nat
myIntegerToNat 0 = Zero
myIntegerToNat n = Succ $ myIntegerToNat (n - 1)

-- I think you'd have to enable an extension that allows n + k patterns,
-- but as-is this doesn't type check.
-- UPDATE: Gah. Should have read the instructions which said to do just that.
--integerToNatA 0 = Zero
--integerToNatA (n+1) = Succ (integerToNat n)

-- Exercise 2 --

myAdd :: Nat -> Nat -> Nat
myAdd Zero n = n
myAdd m Zero = m
myAdd (Succ m) n = myAdd m (Succ n)

addA :: Nat -> Nat -> Nat
addA Zero n = n
addA (Succ m) n = Succ (addA n m)

addG :: Nat -> Nat -> Nat
addG n Zero = n
addG n (Succ m) = Succ (addG m n)

-- Exercise 3 --

myMult :: Nat -> Nat -> Nat
myMult Zero _ = Zero
myMult _ Zero = Zero
myMult (Succ Zero) n = n
myMult (Succ m) n = myAdd (myMult m n) n

multA :: Nat -> Nat -> Nat
multA m Zero = Zero
multA m (Succ n) = myAdd m (multA m n)

-- Exercise 4 --

data Tree = Leaf Integer
          | Node Tree Integer Tree

-- This function assumes the `Tree` argument is a binary search tree.
myOccurs :: Integer -> Tree -> Bool
myOccurs n (Leaf m) = n == m
myOccurs n (Node t1 m t2) = case (compare n m) of
                              EQ -> True
                              LT -> myOccurs n t1
                              GT -> myOccurs n t2

-- Exercise 5 --

data TreeB = LeafB Integer
           | NodeB TreeB TreeB
           deriving Show

myBalanced :: TreeB -> Bool
myBalanced (LeafB _) = True
myBalanced (NodeB l r) = myBalanced l && myBalanced r &&
                        abs (leaves l - leaves r) <= 1

leaves :: TreeB -> Integer
leaves (LeafB _) = 1
leaves (NodeB l r) = leaves l + leaves r

-- Exercise 6 --

myHalve :: [a] -> ([a], [a])
myHalve xs = splitAt (length xs `div` 2) xs

myBalance :: [Integer] -> TreeB
myBalance (x:[]) = LeafB x
myBalance xs = NodeB (myBalance ys) (myBalance zs)
  where (ys, zs) = myHalve xs

-- Exercises 7-12 were straightforward so I didn't bother typing them out

-- Exercise 13 --

myFoldA :: Monoid m => [m] -> m
myFoldA = foldr (<>) mempty

myFoldB :: Monoid m => [m] -> m
myFoldB = foldl (<>) mempty
