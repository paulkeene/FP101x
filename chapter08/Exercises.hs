module Exercises where

import Control.Monad (join, liftM)

-- Exercise 1 --

putStrA :: String -> IO ()
putStrA [] = return ()
putStrA (x:xs) = do
  putChar x
  putStrA xs

putStrB :: String -> IO ()
putStrB [] = return ()
putStrB (x:xs) = putChar x >>= \_ -> putStrB xs

putStrC :: String -> IO ()
putStrC [] = return ()
putStrC (x:xs) = putChar x >> putStrC xs

-- Exercise 2 --

putStrLnA :: String -> IO ()
putStrLnA [] = putChar '\n'
putStrLnA xs = putStrA xs >> putStrLnA ""

putStrLnB :: String -> IO ()
putStrLnB [] = putChar '\n'
putStrLnB xs = putStrA xs >> putChar '\n'

putStrLnC :: String -> IO ()
putStrLnC [] = putChar '\n'
putStrLnC xs = putStrA xs >>= \x -> putChar '\n'

putStrLnE :: String -> IO ()
putStrLnE [] = putChar '\n'
putStrLnE xs = putStrA xs >> putStrA "\n"

-- Bad; infinitely prints newlines
putStrLnF :: String -> IO ()
putStrLnF [] = putChar '\n'
putStrLnF xs = putStrA xs >> putStrLnF "\n"


-- Exercise 3 --

getLineA :: IO String
getLineA = do
  x <- getChar
  if x == '\n'
  then return []
  else getLineA >>= \xs -> return (x:xs)

getLineB :: IO String
getLineB = do
  x <- getChar
  if x == '\n'
  then return []
  else (\xs -> return (x:xs)) =<< getLineB

getLineC :: IO String
getLineC = getC []

getC :: String -> IO String
getC xs = do
  x <- getChar
  case x of
    '\n' -> return xs
    _ -> getC (xs ++ [x])

-- Exercise 3 --

interactA :: (String -> String) -> IO ()
interactA f = do
  xs <- getLineC
  putStrLnA $ f xs

interactB :: (String -> String) -> IO ()
interactB f = do
  input <- getLineA
  putStrLnA (f input)

-- Exercise 4 --

-- values for testing
foo :: [IO ()]
foo = [putStrLn "sup", putStrLn "breh"]

bar :: [IO String]
bar = [getLineC, getLineC]

mySequence :: Monad m => [m a] -> m ()
mySequence [] = return ()
mySequence (x:xs) = x >> (mySequence xs)

sequenceB :: Monad m => [m a] -> m ()
sequenceB [] = return ()
sequenceB (m:ms) = (foldl (>>) m ms) >> return ()

-- Bad; the input list won't necessarily be of type `m ()`, but that's what
-- this implementation assumes when it passes `return ()` to foldl.
--sequenceC :: Monad m => [m a] -> m ()
--sequenceC ms = foldl (>>) (return ()) ms

sequenceD :: Monad m => [m a] -> m ()
sequenceD [] = return ()
sequenceD (m:ms) = m >> sequenceD ms

sequenceE :: Monad m => [m a] -> m ()
sequenceE [] = return ()
sequenceE (m:ms) = m >>= \_ -> sequenceE ms

-- Bad; bind expects its second argument to be a function, but here we are
-- passing valuses of type `Monad m => m ()`
--sequenceF :: Monad m => [m a] -> m ()
--sequenceF ms = foldr (>>=) (return ()) ms

sequenceG :: Monad m => [m a] -> m ()
sequenceG ms = foldr (>>) (return ()) ms

-- Exercise 5 --

mySequenceI :: Monad m => [m a] -> m [a]
mySequenceI [] = return []
mySequenceI (m:ms) = do
  a <- m
  as <- mySequenceI ms
  return (a : as)

sequenceIA :: Monad m => [m a] -> m [a]
sequenceIA [] = return []
sequenceIA (m:ms) = m >>=
                      \ a ->
                        do as <- sequenceIA ms
                           return (a : as)

-- Bad; the default value passed to foldr should be `return []`
--sequenceIB :: Monad m => [m a] -> m [a]
--sequenceIB ms = foldr func (return ()) ms
--  where
--    func :: Monad m => m a -> m [a] -> m [a]
--    func m acc = do
--      a <- m
--      as <- acc
--      return (a : as)

-- Bad; in func we're trying to `cons` a value of type `m a` onto a value
-- of type `Monad m => m [a]`, when `cons` is expecting the second argument
-- to be of type list.
--sequenceIC :: Monad m => [m a] -> m [a]
--sequenceIC ms = foldr func (return []) ms
--  where
--    func :: (Monad m) => m a -> m [a] -> m [a]
--    func m acc = m : acc

-- Bad; we can't unwrap monadic values without using do-notation
--sequenceID :: Monad m => [m a] -> m [a]
--sequenceID [] = return []
--sequenceID (m : ms) = return (a : as)
--  where
--    a <- m
--    as <- sequenceID ms

sequenceIE :: Monad m => [m a] -> m [a]
sequenceIE ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = do
      x <- m
      xs <- acc
      return (x : xs)

-- Bad; `>>` is expecting a value of type `Monad m => m a` as its second
-- argument, but here we're passing it a function
--sequenceIF :: Monad m => [m a] -> m [a]
--sequenceIF [] = return []
--sequenceIF (m : ms) = m >> \ a ->
--                             do as <- sequenceIF ms
--                                return (a : as)

-- Bad; using `<-` without do-notation
--sequenceIG :: Monad m => [m a] -> m [a]
--sequenceIG [] = return []
--sequenceIG (m : ms) = m >>= \a ->
--                              as <- sequence ms
--                              return (a : as)

sequenceIH :: Monad m => [m a] -> m [a]
sequenceIH [] = return []
sequenceIH (m:ms) = do
  a <- m
  as <- sequenceIH ms
  return (a : as)

-- Exercise 7 --

-- values for testing
baz = ["hey", "there"]

-- did you know qux comes after baz?
-- http://en.wikipedia.org/wiki/Foobar
qux = putStrLn

myMapMA :: Monad m => (a -> m b) -> [a] -> m [b]
myMapMA f [] = return []
myMapMA f (a:as) = do
  b <- (f a)
  bs <- myMapMA f as
  return (b : bs)

mapMA :: Monad m => (a -> m b) -> [a] -> m [b]
mapMA f as = sequenceIA (map f as)

mapMB :: Monad m => (a -> m b) -> [a] -> m [b]
mapMB f [] = return []
mapMB f (a:as) = f a >>= \b -> mapMB f as >>= \bs -> return (b:bs)

-- Bad; types don't line up: `sequence` returns a value of type
-- `Monad m => m ()`
--mapMC :: Monad m => (a -> m b) -> [a] -> m [b]
--mapMC f as = sequenceB (map f as)

-- Bad; `>>` takes a monad as its second argument, not a function
--mapMD :: Monad m => (a -> m b) -> [a] -> m [b]
--mapMD f [] = return []
--mapMD f (a:as) = f a >> \b -> mapMD f as >> \bs -> return (b:bs)

-- Bad; `->` should be `<-`
--mapME :: Monad m => (a -> m b) -> [a] -> m [b]
--mapME f [] = return []
--mapME f (a:as) = do
--  f a -> b
--  mapME f as -> bs
--  return (b:bs)

mapMF :: Monad m => (a -> m b) -> [a] -> m [b]
mapMF f [] = return []
mapMF f (a:as) = do
  b <- f a
  bs <- mapMF f as
  return (b:bs)

mapMG :: Monad m => (a -> m b) -> [a] -> m [b]
mapMG f [] = return []
mapMG f (a:as) = f a >>= \b -> do bs <- mapMG f as
                                  return (b:bs)

-- Bad; this function typechecks, but doesn't make sense since the
-- resulting list wrapped in a monad is in reverse order compared to
-- the input list.
mapMH :: Monad m => (a -> m b) -> [a] -> m [b]
mapMH f [] = return []
mapMH f (a:as) = f a >>= \b -> do bs <- mapMH f as
                                  return (bs ++ [b])

-- Exercise 8 --

myFilterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
myFilterM f [] = return []
myFilterM f (a:as) = do
  p <- f a
  as <- myFilterM f as
  if p
  then return (a : as)
  else return as

-- Exercise 9 --

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft f b [] = b
foldLeft f b (a:as) = foldLeft f (f b a) as

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (b:bs) = f a b >>= \a' -> foldLeftM f a' bs

-- Exercise 10 --

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f b [] = b
foldRight f b (a:as) = f a $ foldRight f b as

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (a:as) =  (foldRightM f b as) >>= \b' -> f a b'

-- Exercise 11 --

myLiftM :: Monad m => (a -> b) -> m a -> m b
myLiftM f m = do
  a <- m
  return (f a)

liftMA :: Monad m => (a -> b) -> m a -> m b
liftMA f m = do
  x <- m
  return (f x)

liftMC :: Monad m => (a -> b) -> m a -> m b
liftMC f m = m >>= \ a -> return (f a)

-- Bad; this causes effectful computations to be executed twice
liftME :: Monad m => (a -> b) -> m a -> m b
liftME f m = m >>= \ a -> m >>= \ b -> return (f a)

-- Bad; this causes effectful computations to be executed twice
liftMF :: Monad m => (a -> b) -> m a -> m b
liftMF f m = m >>= \ a -> m >>= \ b -> return (f b)

-- Bad; mapM returns an argument of type `m [b]`
--liftMG :: Monad m => (a -> b) -> m a -> m b
--liftMG f m = mapM f [m]

-- Bad; `>>` takes a monad as it's second argument not a function
--liftMH :: Monad m => (a -> b) -> m a -> m b
--liftMH f m = m >> \ a -> return (f a)
