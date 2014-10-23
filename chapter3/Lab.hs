module Lab where

-- Exercise 0 --

e0 :: [Bool]
e0 = [False, True, False, True]

-- Exercise 1 --

e1as :: [[Integer]]
e1as = [[1, 2], [3, 4]]

e1bs :: [[Int]]
e1bs = [[1, 2], [3, 4]]

e1cs :: Num a => [[a]]
e1cs = [[1, 2], [3, 4]]

-- Exercise 2 --

e2as :: [[[Integer]]]
e2as = [[[1, 2, 3]], [[3, 4, 5]]]

-- Didn't type out the other lists here since the answer was obvious

-- Exercise 3 --

e3 :: Num a => a -> a
e3 x = x * 2

-- Exercise 4 --

e4 :: (a, b) -> a
e4 (x, y) = x

-- Exercise 5 --

e5 :: (a, b, c) -> c
e5 (x, y, z) = z

-- Exercise 6 --

e6 :: Num a => a -> a -> a
e6 x y = x * y

-- Exercise 7 --

e7 :: (a, b) -> (b, a)
e7 (x, y) = (y, x)

-- Exercise 8 --

e8 :: a -> b -> (b, a)
e8 x y = (y, x)

-- Exercise 9 --

e9 :: [a] -> (a, Bool)
e9 [x, y] = (x, True)

-- Exercise 10 --

e10 :: (a, a) -> [a]
e10 (x, y) = [x, y]

-- Exercise 11 --

e11 :: (Char, Bool)
e11 = ('\a', True)

-- Exercise 12 --

e12 :: [(Char, Int)]
e12 = [('a', 1)]

-- Exercise 13 --

e13 :: Int -> Int -> Int
e13 x y = x + y * y

-- Exercise 14 --

e14 :: ([Char], [Float])
e14 = ("Haskell", [3.1, 3.14, 3.141, 3.1415])

-- Exercise 15 --

e15 :: [a] -> [b] -> (a, b)
e15 xs ys = (head xs, head ys)
