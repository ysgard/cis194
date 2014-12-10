-- CIS 194 (Spring 2013) Homework 4
module Cis194H4 where

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x -2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldl (\a b -> (b - 2) * a) 1 $ filter (even) xs

-- Exercise 2

fun2 :: Integer -> Integer
fun2 1             = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- Derived from https://github.com/sunchao/upenn-cis194/blob/master/assignments/4/hw4.hs,
-- Because I was too stupid to figure it out.
fun2' :: Integer -> Integer
fun2' n = sum . filter even . takeWhile (/=1) $ iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n


-- Exercise 3
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- We have certain restrictions here.  Because we are using foldr, we can't use the usual algorithm for
-- creating a balanced bst from a sorted array because we have to parse the array one at a time (instead
-- of dividing the array in half and passing each half recursively to the adding function).
--
-- Instead we need to evaluate the tree at each addition to figure out where to place the node.  
foldTree :: [a] -> Tree a



