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

-- fold
-- summarize the elements of a list

-- Polymorphism

-- declare a polymorphic data type
data List t = E | C t (List t) -- E is 'Empty', C is 'Cons'

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

-- now some polymorphic functions
filterList _ E = E -- can't filter an empty list
filterList p (C x xs)
| p x          = C x (filterList p xs)
| otherwise    = filterList p xs

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)

-- caller gets to pick the types for polymorphic functions
-- polymorphic functions have to work for all possible input type

-- Prelude is an automatically-included module that includes may useful
-- polymorphic functions, such as filter and map.
-- Data.List has more

-- Another one is Maybe:  data Maybe a = Nothing | Just a
-- Data.Maybe has more funcs for working with Maybe values

-- Total and Partial Functions

-- Some functions cannot match all possible inputs.  For example, 'head' returns the first item of a list.
-- But when the list is empty, head will crash!
-- head is an example of a partial functions, a function that cannot handle all possible inputs.
-- functions that handle all possible combinations as input are total functions.

-- head is a mistake, should not be in Prelude.  Ditto with tail, init, last, !!.  Using these functions
-- in homework will lose style points!

-- How to replace?

-- Use pattern matching:
doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + xs

-- Both of these compute the same results, and are both total
-- second one is *obviously* total, and much easier to read, to boot.

-- What about writing partial functions?

-- Two approaches:
-- 1. Change the output type of the function to indicate possible failure.
-- example: rewriting head using Maybe:
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
-- exact same function is defined in the 'safe' package
-- reflected its partial nature in the type returned

-- if some condition is guaranteed, then the types ought to reflect the guarantee.
-- Example: list that should *never* be empty.
data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (Nex x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NoneEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
