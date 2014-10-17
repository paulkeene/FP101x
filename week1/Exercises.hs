module Exercises where

-- Exercise 0 --

a = 2 ^ 3 * 4 == (2 ^ 3) * 4
b = 2 * 3 + 4 * 5 == (2 * 3) + (4 * 5)
c = 2 + 3 * 4 ^ 5 == 2 + (3 * (4 ^ 5))

-- Exercise 1 --

n = a `div` length xs
  where a = 10
        xs = [1, 2, 3, 4, 5]

-- Exercise 2 --

myLast xs = head (drop (length xs - 1) xs)

myLast' xs = xs !! (length xs - 1)

myLast'' xs = head (reverse xs)

-- Exercise 3 --

myInit xs = reverse (tail (reverse xs))

-- Exercise 6 --

myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

-- Exercise 7 --

myQsort [] = []
myQsort (x:xs) = myQsort larger ++ [x] ++ myQsort smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

myQsort' [] = []
myQsort' (x:xs) = myQsort' larger ++ [x] ++ myQsort' smaller
  where smaller = [a | a <- xs, a < x || a == x]
        larger = [b | b <- xs, b > x]

myQsort''' [] = []
myQsort''' (x : xs)
  = reverse
      (reverse (myQsort''' smaller) ++ [x] ++ reverse (myQsort''' larger))
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
