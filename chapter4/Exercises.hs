module Exercises where

import Data.Char

-- Exercise 0 --

sum100 = sum [x ^ 2 | x <- [1..100]]

-- Exercise 1 --

myReplicate :: Int -> a -> [a]
myReplicate n a = [a | _ <- [1..n]]

-- Exercise 2 --

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
                       (x ^ 2 + y ^ 2) == (z ^ 2)]

-- Exercise 3 --

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num

-- Normally I would stop searching at n `div` 2, but this is how the factors
-- function was defined in the lecture slides and it affects the
-- implementation of perfects.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- Exercise 4 --

pairs1 :: [(Int, Int)]
pairs1 = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

pairs2 :: [(Int, Int)]
pairs2 = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

-- Exercise 5 --

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

-- Exercise 6 --

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

-- Exercise 7 --

let2int :: Char -> Int
let2int c = let x = if isLower c then 'a' else 'A'
            in ord c - ord x

int2let :: Int -> Bool -> Char
int2let n isLower = let x = if isLower then 'a' else 'A'
                    in chr (ord x + n)

shift :: Int -> Char -> Char
shift _ ' ' = ' '
shift n c = int2let ((let2int c + n) `mod` 26) (isLower c)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Exercise 8 --
ex8 :: [(Int, Int)]
ex8 = [(x, y) | x <- [1, 2], y <- [1, 2]]

-- Exercise 9 --

ex9 :: [Int]
ex9 = [x | x <- [1, 2, 3], y <- [1..x]]

-- Exercise 10 --

ex10 :: Int
ex10 = sum [x | x <- [1..10], even x]

-- Exercise 11 --

ex11 :: [Int]
ex11 = 1 : [x + 1 | x <- ex11]

-- Exercise 12 --

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

-- Exercise 13 --

-- This function is kind of awkward since when used in the infix form
-- it reads dividend `divides` divisor, but that's how it was defined
-- in the exercise.
divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `divides` d]
