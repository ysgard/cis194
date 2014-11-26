-- CIS 194 - 3, Recursion Patterns, Polymorphism, and the Prelude

-- Recursion is powerful and the only way to loop over things in Haskell
-- in practice recursion is typically eschewed in favor of stdlib funcs
-- that solve common recursion cases.

data IntList = Empty | Cons Int IntList
             deriving Show

-- Map
absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

-- Redundant - all have the same thing in common
-- calling a function repeatedly on the elements of IntList
exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))
addOne x = x + 1
square x = x * x

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList f Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs) 

ex01 = mapIntList addOne exampleList
ex02 = mapIntList square exampleList
ex03 = mapIntList abs exampleList

-- filter
-- remove elements based on a predicate
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x    = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

