-- CIS 194 (Spring 2013) Homework 4
module Cis194H4 where

-- Exercise 1.1

fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x -2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldl (\a b -> (b - 2) * a) 1 $ filter (even) xs

-- Exercise 1.2

fun2 :: Integer -> Integer
fun2 1             = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- Derived from https://github.com/sunchao/upenn-cis194/blob/master/assignments/4/hw4.hs,
-- Because I was too stupid to figure it out.
fun2' :: Integer -> Integer
fun2' n = sum . filter even . takeWhile (/=1) $ iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n 


-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- We have certain restrictions here.  Because we are using foldr, we can't use the usual algorithm for
-- creating a balanced bst from a sorted array because we have to parse the array one at a time (instead
-- of dividing the array in half and passing each half recursively to the adding function).

-- Instead we need to evaluate the tree at each addition to figure out where to place the node.
-- Uuuuurg I can't reproduce what the exact tree for "ABCDEFGHIJ" is, but it is a balanced BST, so meh!
foldTree :: [a] -> Tree a
foldTree = foldr (\x tree -> treeInsert x tree) Leaf

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node _ left val right)
  | lh <= rh = Node (rh + 1) lInsert val right
  | lh > rh  = Node (lh + 1) left val rInsert
  where lh      = height left
        rh      = height right
        lInsert = treeInsert x left
        rInsert = treeInsert x right
treeInsert _ (Node _ _ _ _) = error "Should never happen!"

height :: Tree a -> Integer
height Leaf = 0
height (Node n _ _ _) = n

-- Exercise 3.1

-- Only return True if there is an odd number of True values.
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
xor :: [Bool] -> Bool
xor = foldr (\x y -> if x == True then not y else y) False

-- So much nicer in point-free style
xor' :: [Bool] -> Bool
xor' = odd . length . filter (==True)

-- Exercise 3.2

-- Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
-- Because we can't set an accumulator value (we don't know the type!)
-- we instead need to modify the function - in this case we
-- apply the function to the value in the list, then tack that
-- value onto the front our our accumulator, which is an empty list.
map' f = foldr ((:) . f) []


-- Exercise 4

-- Implement Sieve of Sundaram using function composition
-- generate all odd primes up to 2n + 2

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) $ filter (`notElem` sieve) [1 .. n]
  where sieve :: [Integer]
        sieve = map (\(i,j) -> i + j + 2*i*j) $ filter (\(i,j) -> i <= j) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]





        



 
