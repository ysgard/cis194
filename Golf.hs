-- Golf.hs (Homework, Week 3)

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



