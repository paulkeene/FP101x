module Exercises where

import Prelude hiding ((^), (!!))

-- Exercise 0 --

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * m ^ (n - 1)

-- Exercise 1 --

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + length xs

-- Exercise 2 --

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (_:xs) = drop (n - 1) xs

-- Exercise 3 --

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x : init xs

-- Exercise 4 --

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = and xs && x

-- Exercise 5 --

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ concat xs

-- Exercise 6 --

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n - 1) x

-- Exercise 7 --

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

-- Exercise 8 --

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
  | e == x = True
  | otherwise = myElem e xs

-- Exercise 9 --

myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] ys = ys
myMerge xs [] = xs
myMerge (x:xs) (y:ys)
  | x <= y = x : myMerge xs (y:ys)
  | otherwise = y : myMerge (x:xs) ys

-- Exercise 10 --

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = myMerge (msort ys) (msort zs)
  where (ys, zs) = halve xs
