module Exercises where

-- Exercise 0 --

f0 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
f0 p f xs = [f x | x <- xs, p x]

f0' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
f0' p f xs = map f (filter p xs)

-- Exercise 1 --

myAllA :: (a -> Bool) -> [a] -> Bool
myAllA p xs = and (map p xs)

myAllB :: (a -> Bool) -> [a] -> Bool
myAllB p = and . map p

myAllC :: (a -> Bool) -> [a] -> Bool
myAllC p = not . any (not . p)

myAllD :: (a -> Bool) -> [a] -> Bool
myAllD p xs = foldl (&&) True (map p xs)

myAllE :: (a -> Bool) -> [a] -> Bool
myAllE p = foldr (&&) True . map p

-- Exercise 2 --

myAnyA :: (a -> Bool) -> [a] -> Bool
myAnyA p = or . map p

myAnyB :: (a -> Bool) -> [a] -> Bool
myAnyB p xs = length (filter p xs) > 0

myAnyC :: (a -> Bool) -> [a] -> Bool
myAnyC p = not . null . dropWhile (not . p)

myAnyD :: (a -> Bool) -> [a] -> Bool
myAnyD p xs = not (all (\x -> not (p x)) xs)

myAnyE :: (a -> Bool) -> [a] -> Bool
myAnyE p xs = foldr (\x acc -> (p x) || acc) False xs

-- Exercise 3 --

myTakeWhileA :: (a -> Bool) -> [a] -> [a]
myTakeWhileA _ [] = []
myTakeWhileA p (x:xs)
  | p x = x : myTakeWhileA p xs
  | otherwise = []

myTakeWhileB :: (a -> Bool) -> [a] -> [a]
myTakeWhileB p = fst . foldl (\(xs, c) x -> if (p x) && c then (xs ++ [x], True)
                                            else (xs, False)) ([], True)

-- Exercise 4 --

myDropWhileA :: (a -> Bool) -> [a] -> [a]
myDropWhileA _ [] = []
myDropWhileA p (x:xs)
  | p x = myDropWhileA p xs
  | otherwise = (x:xs)

myDropWhileB :: (a -> Bool) -> [a] -> [a]
myDropWhileB p = fst . foldl (\(xs, c) x -> if (p x) && c then (xs, True)
                                            else (xs ++ [x], False)) ([], True)

-- Exercise 5 --

myMapA :: (a -> b) -> [a] -> [b]
myMapA f = foldr (\x ys -> (f x) : ys) []

myMapB :: (a -> b) -> [a] -> [b]
myMapB f = foldl (\ys x -> ys ++ [f x]) []

-- Exercise 6 --

myFilterA :: (a -> Bool) -> [a] -> [a]
myFilterA p = foldl (\xs x -> if p x then xs ++ [x] else xs) []

myFilterB :: (a -> Bool) -> [a] -> [a]
myFilterB p = foldr (\x xs -> if p x then x : xs else xs) []

-- Exercise 7 --

dec2intA :: [Integer] -> Integer
dec2intA = fst . foldr (\x (acc, c) -> (acc + x * 10 ^ c, c + 1)) (0, 0)

dec2intB :: [Integer] -> Integer
dec2intB = foldl (\acc x -> x + 10 * acc) 0

-- Exercise 8 --

-- The following function doesn't type check because sum has type [Int] -> Int
-- while map (^ 2) and filter even both have type [Int] -> [Int]. In Haskell
-- every element in a list has to be of the same type.

-- sumsqreven = compose [sum, map (^ 2), filter even]

compose :: [a -> a] -> (a -> a)
compose  = foldr (.) id

-- Exercise 9 --

myCurryA :: ((a, b) -> c) -> a -> b -> c
myCurryA f a b = f (a, b)

myCurryB :: ((a, b) -> c) -> a -> b -> c
myCurryB = \f -> (\a -> (\b -> f (a, b)))

myCurryC :: ((a, b) -> c) -> a -> b -> c
myCurryC f = \x y -> f (x, y)

-- Exercise 10 --

myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(a, b) -> f a b

-- Exercise 11 --

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8A :: [Bit] -> [[Bit]]
chop8A [] = []
chop8A bits = take 8 bits : chop8A (drop 8 bits)

chop8B :: [Bit] -> [[Bit]]
chop8B = unfold null (take 8) (drop 8)

-- Exercise 12 --

myMapC :: (a -> b) -> [a] -> [b]
myMapC f = unfold null (f . head)  tail

-- Exercise 13 --

myIterate :: (a -> a) -> a -> [a]
myIterate f = unfold (const False) id f

-- Exercises 14 - 30 were very straightforward so I didn't type them up --

-- Exercise 31 --

type Church = (Int -> Int) -> Int -> Int

zero :: Church
zero = \_ z -> z

one :: Church
one = \s z -> s z

two :: Church
two = \s z -> (s . s) z

five :: Church
five = \s z -> (s . s . s . s . s) z

myAdd :: Church -> Church -> Church
myAdd x y = \s z -> x s (y s z)

myMultiply :: Church -> Church -> Church
myMultiply x y = \s z -> x (y s) z

-- Still need to work out what this implementation looks like
--myExponent :: Church -> Church -> Church
--myExponent x y = \s z -> myMultiply x x
