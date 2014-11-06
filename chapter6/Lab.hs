module Lab where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, even x]

-- ===================================
-- Ex. 3 - 4
-- ===================================

-- complete the following line with the correct type signature for this function
squares :: Integer -> [Integer]
squares n = [x * x | x <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
squares' :: Integer -> Integer -> [Integer]
squares' m n = [x * x | x <- [(n + 1)..(m + n)]]

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- Here I was just verifying that these two type signatures are equivalent.
-- foo :: Int -> Int -> Int -> Int -> Int
-- foo :: Int -> (Int -> Int -> (Int -> Int))
foo a b c d = a + b + c + d
