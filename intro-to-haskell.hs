-- Following http://www.seas.upenn.edu/~cis194/spring13/lectures/01-intro.html

x :: Int
x = 3

-- x = 4 -- this will not work, redeclaration

y :: Int
y = y + 1

-- Machine size integers
i :: Int
i = -78

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

-- Arbitrary-precision integers
n :: Integer
n = 1234567890987654321987340982334987349872349874534

reallyBig :: Integer
reallyBig = 2 ^ (2 ^ (2 ^ (2 ^ 2)))

numDigits :: Int
numDigits = length (show reallyBig)

-- Double-precision floating point
d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4

-- Booleans
b1, b2 :: Bool
b1 = True
b2 = False

-- Unicode characters
c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'

-- Strings are lists of characters with special syntax
s :: String
s = "Hello, Haskell!"

-- Ghci
-- :load, :l a file
-- :reload, :r a file
-- :type, :t of an expression
-- :? would you like to know more?

ex01 = 3 + 2
ex02 = 19 - 27
ex03 = 2.35 * 8.6
ex04 = 8.7 / 3.1
ex05 = mod 19 3
ex06 = 19 `mod` 3
ex07 = 7 ^ 222
ex08 = (-3) * (-7)

-- badArith1 = i + n
-- Haskell does not do implicit conversion
-- Use fromIntegral to convert from any integral type (Int, Integer) to other numeric.
-- round, floor, ceiling converts floating point numbers to Int, Integer

-- badArith1 = i / i
-- Error because '/' is for floating-point division only.  Use div instead for integral division
ex09 = i `div` i
ex10 = 12 `div` 5

-- Bools
ex11 = True && False
ex12 = not (False || True)

ex13 = ('a' == 'a')
ex14 = (16 /= 3)
ex15 = (5 > 3) && ('p' <= 'q')
ex16 = "Haskell" > "C++"
       
-- Defining Basic Functions

-- Compute the sum of the integers from 1 to n
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

-- Guards
hailstone :: Integer -> Integer
hailstone n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise         = 4
foo n
    | n < 0           = 0
    | n `mod` 17 == 2 = -43
    | otherwise       = n + 3 

isEven :: Integer -> Bool
isEven n
       | n `mod` 2 == 0 = True
       | otherwise      = False

-- Better
isEven' :: Integer -> Bool
isEven' n = n `mod` 2 == 0

-- Pairs
p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

-- Don't use triples, quadruples, etc.... (see next section for better)

-- Using functions, multiple args
f :: Int -> Int -> Int -> Int
f x y z = x + y + z
ex17 = f 3 17 8

-- function application has higher precedence than infix operators
-- so writing
-- f 3 n+1 7 is incorrect, as it parses to (f 3 n) + (1 7)
-- Instead, write f 3 (n + 1) 7

-- Lists

nums, range, range2 :: [Integer]
nums   = [1,2,3,19]
range  = [1..100]
range2 = [2,4..100]

-- hello1 and hello2 are exactly the same
hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2

-- Constructing Lists
emptyList = []

ex18 = 1 : [] -- cons operator
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []

ex21 = [2,3,4] == 2 : 3 : 4 : []

-- Lists are singly-linked lists, not arrays

-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Functions on lists

-- Compute the length of a list of integers
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs
                          
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = [] -- do nothing to the empty list
sumEveryTwo (x:[])     = [x] -- do nothing to lists of a single element
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs

-- Combining functions

-- The number of hailstone steps needed to reach 1 from a starting number
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
