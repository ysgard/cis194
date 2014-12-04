-- Golf.hs (Homework, Week 3)

module Golf where

import Data.List

-- Exercise 1

-- skips "ABCD" -> ["ABCD", "BD", "C", "D"]
-- skips "hello!" -> ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] -> [[1]]
-- skips [True, False] -> [[True, False], [False]]
-- skips [] -> []

-- skips works by taking a function, f, and mapping it over
-- a sequence of integers up to , and including, the length
-- of the list given.  The function f builds a list from
-- a provided integer and a list where the items of that
-- list correspond to the elements that occur at every nth position.
-- It does this by taking the first n elements of the list
-- (provided that there are enough elements to take) and grabbing
-- the last value before calling it again on the remnants of the list.

skips :: [a] -> [[a]]
skips lst = map (f lst) [1..length lst]
  where f :: [a] -> Int -> [a]
        f [] _ = []
        f lst n
          | n > length lst = []
          | otherwise = lst !! (n - 1) : f (drop n lst) n

-- Exercise 2

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

-- localMaxima works by checking to see if it is passed an array capable of
-- having a local maximum - i.e., it has at least three elements to work with
-- and then uses pattern matching to examine the first three elements.  If the
-- element in the middle of those three elements is a local maxima, it is
-- appended to the results of calling localMaxima with the next three elements being
-- examined being the last two elements of the last pattern along
-- with a new element obtained from the list.
-- 'f' is the a helper function that actually does the element examination, while
-- localMaxima basically checks for the end condition (the remaining list is too small
-- to contain any more local Maxima) while calling f recursively. 

localMaxima :: [Integer] -> [Integer]
localMaxima lst
  | length lst < 3 = []
  | otherwise = f lst
  where f :: [Integer] -> [Integer]
        f (x:y:z:xs)
          | y > x && y > z = y : localMaxima (y:z:xs)
          | otherwise = localMaxima (y:z:xs)

-- wish this worked
-- localMaxima' :: [Integer] -> [Integer]
-- localMaxima' lst@(x:y:z:xs)
--   | length lst < 3 = []
--   | otherwise = if y > x && y > z then y : localMaxima (y:z:xs)
--                 else localMaxima (y:z:xs)

-- Exercise 3

-- histogram [1,1,1,5] ==
--
--  *
--  *
--  *   *
-- ==========
-- 0123456789
--
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--
--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789

-- NB program should output a string like "    *  *    \n==========\n0123456789\n"
-- for histogram [3,5]        

-- For a given array of integers and a particular value, return a string
-- of '*' whose length corresponds to the number of times that value occurs
        
freqStr :: [Integer] -> Integer -> String
freqStr xs n = take (freq xs n)  (repeat '*')
  where freq xs n = length (filter (== n) xs) 

-- Pad a string out to len characters.  Needed when forming a square array
-- for transposition.
pad:: Int -> String -> String
pad len xs = xs ++ take diff (repeat ' ')
  where diff = len - length xs

-- Get a list of strings representing a raw, horizontal histogram
rawStrings :: [Integer] -> [String]
rawStrings xs = map (freqStr xs) [0..9]

-- Make the raw, horizontal histogram square so we can flip it on its side
fixStrings :: [String] -> [String]
fixStrings xxs = map (pad mx) xxs ++ extra
  where mx = maximum (map length xxs)
        extra = take (mx - length xxs) (repeat (take mx (repeat ' ')))

-- Cut out empty strings resulting from the transpose, and add footer
wrapAndClean :: [String] -> [String]
wrapAndClean xs = (clean (reverse xs)) ++ ["=========="] ++ ["0123456789"]
  where clean :: [String] -> [String]
        clean = filter ('*' `elem`)

-- Get the proper, upright histogram
histo :: [Integer] -> [String]
histo = wrapAndClean . transpose . fixStrings . rawStrings

-- Produce a string histogram from a list of integers
histogram :: [Integer] -> String
histogram = (foldr f "") . histo 
  where f :: String -> String -> String
        f a b = a ++ "\n" ++ b









