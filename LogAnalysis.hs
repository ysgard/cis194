{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


-- Homework 2 (LogAnalysis)

-- Exercise 1
-- ex: parseMessage "E 2 562 help help" --> LogMessage (Error 2) 562 "help help"
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E" : sv : t : msg) -> LogMessage (Error (read sv :: Int)) (read t :: Int) (unwords msg)
  ("I" : t : msg)   -> LogMessage Info (read t :: Int) (unwords msg)
  ("W" : t : msg)   -> LogMessage Warning (read t :: Int) (unwords msg)
  m             -> Unknown (unwords m)

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ msgTime _) (Node ltree l@(LogMessage _ treeTime _) rtree)
  | msgTime <= treeTime = Node (insert msg ltree) l rtree 
  | msgTime > treeTime = Node ltree l (insert msg rtree)
insert _ _  = error "Should never occur!"
                          
-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = Node Leaf x Leaf
build (x : xs) = insert x (build xs)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree msg rtree) = inOrder ltree ++ [msg] ++ inOrder rtree

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logs = toStrings . inOrder . build $ filterSeverity logs
  where
    filterSeverity :: [LogMessage] -> [LogMessage]
    filterSeverity [] = []
    filterSeverity (x@(LogMessage (Error sv) _ _):xs)
      | sv > 50 = x : filterSeverity xs
      | otherwise = filterSeverity xs
    filterSeverity (_ : xs) = filterSeverity xs
    toStrings :: [LogMessage] -> [String]
    toStrings [] = []
    toStrings (LogMessage _ _ msg : xs) = msg : toStrings xs
    toStrings (Unknown _ : _) = []
