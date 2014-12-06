-- CIS 194 Lecture 4
-- Higher-order programming and type inference
module Cis194L4 where
-- See Learn You a Haskell - Chapter 6

-- Anonymous functions

gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 = filter gt100

-- annoying to have to use g100
-- Instead, use an anonymous function

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' = filter (\x -> x > 100)

-- Can omit a lambda definition
greaterThan100'' :: [Integer] -> [Integer]
greaterThan100'' = filter (>100)

-- '(>100)' is an 'operator section'.  If '?' is an operator, then
-- (?y) is equivalent to \x -> x ? y, and
-- (y?) is equivalent to \x -> y ? x

-- Function Composition
foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)
-- Better: foo f g = f . g

-- Consider
myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

-- rewritten
myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

-- Why does
-- :t (.) yield (b -> c) -> (a -> b) -> a -> c
-- instead of   (b -> c) -> (a -> b) -> (a -> c) ?
-- because of currying.

-- Currying, Partial Application

-- all functions in Haskell take one argument, and either return a value or a
-- partially applied function.

fn :: Int -> Int -> Int
fn x y = 2 * x + y

-- can also write as:
fn' :: Int -> (Int -> Int)
fn' x y = 2 * x + y

-- Function arrows associate to the right, that is
-- W -> X -> Y -> Z
-- is W -> (X -> (Y -> Z))
-- But function application is left-associative, that is
-- f 3 2
-- is (f 3) 2
-- Which makes sens - try replacing types: (f Int) Int -> (Int -> Int) Int
-- Because function application is left-associative, we can omit the parens
-- (f 3) 2 === f 3 2



