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

-- multi-argument lambda abstraction
-- \x y z -> ...
-- is really just syntactic sugar for
-- \x -> (\y -> (\z -> ...))
-- and f x y z = ... is really just
-- f = \x -> (\y -> (\z -> ...))

-- Can thus rewrite the composition function to
comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

-- To actually represent a function that takes two arguments
-- and not curry them, use a tuple:
f'' :: (Int, Int) -> Int
f'' (x, y) = 2 * x + y
-- Not really, it's still just one argument - it just happens to be a pair

-- to convert between the two representations of a two-argument function
-- use curry and uncurry - defined like this:
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- example
five = uncurry (+) (2,3)
    
-- Partial Application

-- Because Haskell doesn't make it escpecially convenient to partially apply
-- multiple operators (exception: infix operators) it's a bit of an art
-- figuring out the most useful ordering of the arguments to a function.

-- in practice, use "least ot greatest variation", i.e. the argument that
-- changes the least should come first, followed by arguments that are
-- more likely to change and ending with the most variable arguments last.

-- Examples. Whee!

foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7 * x + 2) + foobar xs
  | otherwise = foobar xs

-- ^ is not good Haskell style.  Does too much at once, works at too low a level
-- more idiomatic:
foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

-- the style of coding in which we define a function without referring to its arguments
-- saying what a function is rather than what it does
-- is called "point-free" style

-- Folds

sum' :: [Integer] -> Integer
sum' []         = 0
sum' (x:xs)     = x + sum' xs

product' :: [Integer] -> Integer
product' []     = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []      = 0
length' (_:xs)  = 1 + length' xs

-- All of these condense lists down to a single value.  In other words, [a] -> a
fold' :: b -> (a -> b -> b) -> [a] -> b
fold' z _ []    = z
fold' z f (x:xs) = f x (fold' z f xs)

-- fold' essentially replaces [] with z and (:) with f, so...
-- fold' f z [a, b, c] == a `f` (b `f` (c `f` z))

-- rewriting sum', product' and length' in terms of fold
sum'' = fold' 0 (+)
product'' = fold' 1 (*)
length'' = fold' 0 (\_ s -> 1 + s)  -- could also write (\_ -> (1+))

-- foldr means 'fold from the right'
-- foldl - fold from the left
-- foldr f z [a,b,c] == a `f` (b `f` (c `f` x))
-- foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c

-- Should use foldl' from Data.List though, it's more efficient.
