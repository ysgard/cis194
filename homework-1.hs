-- Homework for CIS 194 (1) Intro

-- CIS 194 : Homework 1

-- Question 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev i
    | i <= 0 = []
    | otherwise = i `mod` 10 : toDigitsRev (i `div` 10)
            
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev  

-- Question 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther i = reverse (f (reverse i))
    where
      f [] = []
      f [x] = [x]
      f (x:y:xs) = x : (*2) y : f xs

-- Question 3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map (sum . toDigits) xs)

-- Question 4
validate :: Integer -> Bool
validate c = (sumDigits . doubleEveryOther $ toDigits c) `mod` 10 == 0

-- Question 5
-- hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a 
