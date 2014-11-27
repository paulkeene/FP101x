module Exercises where

-- Exercise 0 --

list1 :: [Char]
list1 = ['a', 'b', 'c']

-- Exercise 1 --

tuple1 :: (Char, Char, Char)
tuple1 = ('a', 'b', 'c')

-- Exercise 2 --

list2 :: [(Bool, Char)]
list2 = [(False, '0'), (True, '1')]

-- Exercise 3 --

list3 :: ([Bool], [Char])
list3 = ([False, True], ['0', '1'])

-- Exercise 4 --

list4 :: [[a] -> [a]]
list4 = [tail, init, reverse]

-- Exercise 5 --

second :: [a] -> a
second xs = head (tail xs)

-- Exercise 6 --

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Exercise 7 --

pair :: a -> b -> (a, b)
pair x y = (x, y)

-- Exercise 8 --

double :: Num a => a -> a
double x = x * 2

-- Exercise 9 --

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- Exercise 10 --

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Exercise 14 --

ex14 :: [String]
ex14 = ["False", "True"]

-- Exercise 15 --

ex15 :: ([Bool], Bool)
ex15 = ([False, True], False)

-- Exercise 16 --

ex16 :: (String, String)
ex16 = ("1,2", "3,4")

-- Exercise 17 --

ex17 :: [(Int, Bool)]
ex17 = [(1, True), (0, False)]

-- Exercise 18 --

ex18 :: [a] -> [a]
ex18 xs = take 3 (reverse xs)

-- Exercise 20 --

ex20a :: [Int]
ex20a = 1 : [2, 3, 4]

ex20b :: [Int]
ex20b = [] ++ [1, 2, 3, 4]

ex20c :: [[Int]]
ex20c = [[1, 2]] ++ [[3, 4]]

ex20d :: [Int]
ex20d = 1 : 2 : 3 : 4 : []
