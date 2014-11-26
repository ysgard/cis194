-- Algebraic Data Types

-- Declares a new type called 'Thing'
-- with five data constructors
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
           deriving Show -- generate default code to convert Things to Strings

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

-- Write functions on Things by pattern-matching
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

-- Make a bit shorter
isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True

-- Not just an enumeration - algebraic data type
data FailableDouble = Failure
                    | OK Double
                    deriving Show

ex01 = Failure
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- Store a person's name, age and favorite thing
data Person = Person String Int Thing
            deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

-- An algebraic data type has one or more data constructors, and
-- each data constructor can have zero or more arguments
-- data AlgDataType = Constr1 Type11 Type12
--                 | Constr2 Type21
--                 | Constr3 Type31 Type32 Type33
--                 | Constr4

-- vars always start with lower-case, type/data constructors upper case

-- pattern-matching is about taking apart a value to see what constructors
-- it was made with.
-- _ matches anything
-- x@pat can be used to match a value against the pattern pat, but *also*
-- give the name x to the entire value being matched.
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n
-- patterns can be nested
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _) = n ++ ", your favorite thing is lame."

-- this grammar defines what can be used as a pattern
-- pat :: = _
--        | var
--        | var @ (pat)
--        | (Constructor pat1 pat2 ... patn)

-- case is used for basic pattern matching outside of funcs
ex03 = case "Hello" of
  []      -> 3
  ('H':s) -> length s
  _       -> 7

-- syntax of defining functions is just syntactic sugar for a case statement
-- for example:
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                    Failure -> 0
                    OK d -> d

-- data types can be recursive
data IntList = Empty | Cons Int IntList

-- Can use recursive functions to process recursive data types
intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

-- Another simple example - binary tree with int value on each internal
-- node, and char stored in each leaf (not really useful, example only)
data Tree = Leaf Char
          | Node Tree Int Tree
          deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
