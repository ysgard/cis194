-- Golf.hs (Homework, Week 3)

-- Exercise 1

-- skips "ABCD" -> ["ABCD", "BD", "C", "D"]
-- skips "hello!" -> ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] -> [[1]]
-- skips [True, False] -> [[True, False], [False]]
-- skips [] -> []
skips :: [a] -> [[a]]
