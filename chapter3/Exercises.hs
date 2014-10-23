module Exercises where

-- Exercise 0 --

foo = [1, 2, 3, 4, 5, 6]
bar = [1,2]
baz = []

-- Doesn't compile
-- halveA :: [a] -> ([a], [a])
-- halveA xs = (take n xs, drop n xs)
--  where n = length xs / 2

halveB :: [a] -> ([a], [a])
halveB xs = splitAt (length xs `div` 2) xs

halveC :: [a] -> ([a], [a])
halveC xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

-- Doesn't compile
-- halveD :: [a] -> ([a], [a])
-- halveD xs = splitAt (length xs `div` 2)

-- Yields incorrect output
halveE :: [a] -> ([a], [a])
halveE xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2

halveF :: [a] -> ([a], [a])
halveF xs = splitAt (div (length xs) 2) xs

-- Doesn't compile
-- halveG :: [a] -> ([a], [a])
-- halveG xs = splitAt (length xs / 2) xs

halveH :: [a] -> ([a], [a])
halveH xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-- Exercise 1 --

safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safetailB :: [a] -> [a]
safetailB [] = []
safetailB (_ : xs) = xs

-- Doesn't work; non-exhaustive pattern match
safetailC :: [a] -> [a]
safetailC (_ : xs)
  | null xs = [] -- this will never be true
  | otherwise = tail xs

safetailD :: [a] -> [a]
safetailD xs
  | null xs = []
  | otherwise = tail xs

safetailE :: [a] -> [a]
safetailE xs
  | length xs == 0 = []
  | otherwise = tail xs

-- Doesn't work since the first pattern always matches
-- Commented out since this causes compiler warnings due to the overlapping
-- pattern.
-- safetailF :: [a] -> [a]
-- safetailF xs = tail xs
-- safetailF [] = [] -- this will never match

safetailG :: [a] -> [a]
safetailG [] = []
safetailG xs = tail xs

-- Doesn't work; non-exhaustive pattern match
safetailH :: [a] -> [a]
safetailH [x] = [x]
safetailH (_ : xs) = xs

-- Exercise 2 --

False `orA` False = False
_ `orA` _ = True

False `orB` b = b
True `orB` _ = True

-- This is the definition for xnor not or
b `orC` c
  | b == c = True
  | otherwise = False

b `orD` c
  | b == c = b
  | otherwise = True

b `orE` False = b
_ `orE` True = True

b `orF` c
  | b == c = c
  | otherwise = True

-- Incorrect logic and non-exhaustive pattern matching
-- b `orG` True = b
-- _ `orG` True = True

False `orH` False = False
False `orH` True = True
True `orH` False = True
True `orH` True = True

-- Exercise 3 --

True `andA` True = True
_ `andA` _ = False

a `andB` b = if a then if b then True else False else False

-- Incorrect output on False `andC` False
a `andC` b = if not (a) then not (b) else True

-- Doesn't compile
-- a `andD` b = if a then b

-- Incorrect output on True `andE` True
a `andE` b = if a then if b then False else True else False

a `andF` b = if a then b else False

a `andG` b = if b then a else False

-- Exercise 4 --

mult :: Num a => a -> a -> a -> a
mult x y z = x * y * z

multLambda :: Num a => a -> (a -> (a -> a))
multLambda = \x -> (\y -> (\z -> x * y * z))

mult2 :: Num a => a -> (a -> a)
mult2 x = \y -> x * y

-- Exercise 7 --

remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n + 1) xs

-- Exercise 8 --

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs
