{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Folds and Monoids (Lecture 7 of CIS 194, Spring 2013)

-- Consider binary tree defn
data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

-- Compute # of Nodes
treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

-- Sum of data in tree of Integers
treeSum :: Tree Integer -> Integer
treeSum Empty        = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

-- Depth of a tree
treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

-- flatten tree into list
flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- patterns?  Each of these...
-- 1. takes a Tree
-- 2. pattern-matches on Tree
-- 3. Give a simple answer for Empty
-- 4. For Nodes,
--    a. calls itself recursively on the subtrees
--    b. combine the results from the recursions with the data x

-- Generalize
-- need to pass as parameters the parts of the above which
-- change from example to example
-- call the type of data contained in the tree a, and the type of the result, b
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

-- Now, can define the other functions more simply
treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> x + l + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l x r -> l `max` x `max` r)
                       
-- Folding expressions
-- Recall ExprT and corresponding eval from Homework 5:

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- looks familiar.  Fold for ExprT?
exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

-- Now we can do things like count the number of literals in an expression
numLiterals :: ExprT -> Int
numLiterals = exprTFold (const 1) (+) (+)


-- A fold for T will take one (higher-order) argument for each of T's constructors,
-- encoding how to turn the values stored by that constructor into a value of the
-- result type, assuming that any recursions of T have already been folded.


-- Monoids (type class in Data.Monoid)

class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
-- This latter is defined simply to avoid writing 'mappend' all the time.

-- types that are instances of Monoid have a special element called 'mempty',
-- and a binary op called mappend which takes 2 values of the type and produces
-- another one.
-- mempty is an identity for <>, and <> is associative:
-- 1. mempty <> x == x
-- 2. x <> mempty == x
-- 3. (x <> y) <> z == x <> (y <> z)
-- Can therefore unambiguously write: a <> b <> c <> d <> e

-- mconcat is used for combining whole list of values.
-- Implemented using foldr by default, but placed in Monoid
-- because particular instances of Monoid may have better
-- ways of implementing it.

-- Monoids show up everywhere.

-- The following instances are in the standard libraries, just showing for practice.

-- Lists form a monoid under concatenation
instance Monoid [a] where
  mempty = []
  mappend = (++)

-- Addition defines a monoid on integer - but so does multiplication.
-- Can't give two different instances of the same type class to the same type.
-- So, create two newtypes, one for each instance:

newtype Sum a = Sum a
              deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend = (+)

newtype Product a = Product a
                  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend = (*)

-- To find product of a list of integers using mconcat, turn them into values of type Product Integer
lst :: [Integer]
lst = [1,5,8,23,423,99]

prod :: Integer
prod = getProduct . mconcat . map Product $ lst
-- (note: example is silly, since we could just use product.  Just showing the pattern)

-- pairs form a monoid as long as the individual components do:

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (a, b) `mappend` (c, d) = (a `mappend` c, b `mappend` d)
                           
