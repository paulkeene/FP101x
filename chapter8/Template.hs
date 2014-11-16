module Lab4 where

-------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
-------------------------------------------------------------------------------


import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle 0 = 0
triangle n = n + triangle (n - 1)

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count a [] = 0
count a (x:xs) = if (a == x) then 1 + (count a xs)
                             else count a xs

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,
      7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,
      12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,
      29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y) = case x `compare` y of
                  LT -> euclid (x, y - x)
                  GT -> euclid (x - y, y)
                  EQ -> x

-- ===================================
-- Ex. 3
-- ===================================

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap f g xs = map func $ zip [0..] xs
  where
    func (i, x) = if i `mod` 2 == 0
                  then f x
                  else g x

-- Exercise 12 --

foo :: (a -> ([(a -> a)] -> a))
foo = foldr id

-- Exercise 13 --

-- This definition is what ghci reports for `:t flip foldr const`
myBar :: (a -> (b -> c -> b) -> (b -> c -> b)) -> [a] -> (b -> c -> b)
myBar = flip foldr const

-- This definition is the tersest we can achieve. I don't fully understand
-- the rules, but it appears that you have to parenthesize the first functional
-- argument to any function, but after that parentheses are optional. So here
-- we have to use parentheses to show that the first argument to myBar2 is a
-- function. We also have to use parentheses to show that the first argument to
-- the function that is the first argument to myBar2 is a function. After that
-- it doesn't matter since all arguments are curried anyway.
myBar2 :: (a -> (c -> b -> c) -> c -> b -> c) -> [a] -> c -> b -> c
myBar2 = flip foldr const

-- Bad; the additional parentheses make it appear like functions return
-- functions when they don't and causes type errors.
--barA :: (t2 -> ((t0 -> t1) -> t0) -> (t0 -> t1) -> t0) -> [t2] -> (t0 -> t1) -> t0
--barA = flip foldr const

-- Bad
--barB :: (a -> ((c -> b) -> c) -> (c -> b) -> c) -> [a] -> (c -> b) -> c
--barB = flip foldr const

barC :: (a -> (c -> (b -> c)) -> c -> (b -> c)) -> [a] -> c -> (b -> c)
barC = flip foldr const

-- Bad
--barD :: a -> c -> b -> c -> c -> b -> c -> [a] -> c -> b -> c
--barD = flip foldr const

-- Exercise 14 --

dup :: a -> (a, a)
dup a = (a, a)

dup3 :: a -> (((a, a), (a, a)), ((a, a), (a, a)))
dup3 = dup . dup . dup

dup3' :: (a) -> ((((a), (a)), ((a), (a))), (((a), (a)), ((a), (a))))
dup3' = dup . dup . dup

-- Exrcise 15 --

--h :: ((b -> c) -> b) -> (b -> c) -> c
h :: ((a -> b) -> a) -> ((a -> b) -> b)
h g f = (f . g) $ f

-- Exercise 16 --

fix = h fix

-- Exercise 18 --

baz :: (Num a, Eq a) => (a -> a) -> a -> a
baz = \baz n -> if (n == 0) then 1 else n * baz (n - 1)

-- Exercise 19 --

qux :: (Num a, Eq a) => a -> a
qux = fix $ baz
