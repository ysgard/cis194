-- More Polymorphism and type classes (Lecture 5)

-- Given type: a -> a -> a
-- What functions can have this?  Only two!
f1 :: a -> a -> a
f1 x y = x

-- What behaviours can these types have?

-- a -> a : Can only return itself
-- a -> b : Nothing, unless Haskell allows casts - which it doesn't (can't determine type)
-- a -> b -> a : Just returns the first argument
-- [a] -> [a] : Functions that manipulate lists but not their values (reverse, init, tail, etc..)
-- (b -> c) -> (a ->b) -> (a -> c) : function composition (.)
-- (a -> a) -> a -> a : Uhhhh... Takes identity and applies it?

-- Typeclasses - Num, Eq, Ord, Show
-- Functions with these in their signatures are 'type-class polymorphic'

-- Example: Eq
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
-- Read this as: Eq is declared to be a type class with a single parameter, a.
-- Any type a which wants to be an instance of Eq must define two functions,
-- (==) and (/=), with the indicated type signatures.

-- If a normal polymorphic type is a promise that the function will work for
-- whatever type the caller chooses, a type class polymorphic function is a
-- restricted promise that the function will work for any type the caller
-- chooses, as long as the chosen type is an instance of the required
-- type class(es).

-- Example:
data Foo = F Int | G Char
instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False
  foo1 /= foo2 = not (foo1 == foo2)

-- Annoying to have to declare '/=' as well - but we can define it in the typeclass, like so:
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x /= y = not (x == y)

-- In fact, can go one step further - define each function in terms of the other!
-- class Eq a where
--   (==), (/=) :: a -> a -> Bool
--   x == y = not (x /= y)
--   x /= y = not (x == y)

-- GHC is able to automatically generate instances of Eq for us.
data Foo' = F' Int | G' Char
          deriving (Eq, Ord, Show)

-- Typeclasses

-- Ord: ordered types, provides (<) and (<=), and the compare function
-- Num: numeric types, supports addition, subtraction, multiplication.  Integer literals are Nums (polymorphic!)
-- Show: convert values into strings
-- Read: Dual of show
-- Integral: Represents a whole number, like Int and Integer.

-- Example: Making our own type class

-- class of things that can be converted to a list of Ints
class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

-- Implementing other functions in terms of toList also get a Listable constraint
-- to compute sumL, first convert to a list of Ints, then sum
sumL x = sum (toList x)
-- type of this is automatically sumL :: Listable a => a -> Int

foo x y = sum (toList x) == sum (toList y) || x < y
-- type of this is foo :: (Listable a, Ord a) => a -> a -> Bool

instance (Listable a, Listable b) => Listable (a, b) where
  toList (x, y) = toList x ++ toList y


