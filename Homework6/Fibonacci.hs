-- Fibonacci.hs (Homework 6 for CIS 194, Spring 2013)

--import Data.Foldable


-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2

-- (Hack of a Python algo, uses a tuple to maintain state)
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x, y) -> (y, x + y)) (0,1)

-- One-liner using GHC found online:
-- ghc -e 'let fibs = 1 : scanl (+) 1 fibs in fibs !! 20'

-- Exercise 3

-- Couldn't figure out most of these, cribbing a lot and
-- trying to understand.
-- Thank you, danielgrigg, for providing most of the below

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Sunchao's implementation
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs


-- Exercise 4

-- Wow, actually figured out this one myself!
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- This one too!
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- Danielgrigg's implementation
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))


-- Exercise 5

-- Figured this one out
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- sunchao
ruler :: Stream Integer
ruler = f 0
  where f n = interleaveStreams (streamRepeat n) (f $ n+1)

-- danielgrigg
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))

