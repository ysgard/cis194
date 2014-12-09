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

                     
foldTree :: [a] -> Tree a
foldTree xs = foldr (\x tree -> insertInTree x tree) Leaf xs


insertInTree :: a -> Tree a -> Tree a
insertInTree x Leaf = Node 0 Leaf x Leaf
insertInTree x (Node n leftTree value rightTree)
  | lh < rh   = Node n (insertInTree x leftTree) value rightTree
  | lh > rh   = Node n leftTree value (insertInTree x rightTree)
  | otherwise = Node (height + 1) leftTree value (insertInTree x rightTree)
  where lh     = treeHeight leftTree
        rh     = treeHeight rightTree
        height = treeHeight (insertInTree x rightTree)

treeHeight :: Tree a -> Integer
treeHeight Leaf            = 0
treeHeight (Node _ _ _ _ ) = 0
