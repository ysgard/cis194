{-# LANGUAGE FlexibleInstances #-}
-- Calc.hs (Homework 5 for CIS 194 Spring '13)
module Calc where
import Data.Maybe (fromJust)
import ExprT
import Parser (parseExp)


import qualified StackVM as VM

-- Exercise 1

-- Write an evaluator for ExprT
-- example: eval (Mul (Ass (Lit 2) (Lit 3)) (Lit 4)) == 20

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s
  | r == Nothing = Nothing
  | otherwise = Just $ eval jr
  where r :: Maybe ExprT
        r = parseExp Lit Add Mul s
        jr :: ExprT
        jr = fromJust r

-- Exercise 3

class Expr a where
  mul, add :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  mul = Mul
  add = Add
  lit = Lit

-- Exercise 4

newtype MinMax = MinMax Integer deriving (Ord, Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)


instance Expr Integer where
  mul x y = x * y
  add x y = x + y
  lit x   = x

instance Expr Bool where
  mul x y = x && y
  add x y = x || y
  lit x   = if x <= 0 then False else True

instance Expr MinMax where
  mul x y = min x y
  add x y = max x y
  lit     = MinMax

instance Expr Mod7 where
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `rem` 7
  lit x                 = Mod7 $ x `mod` 7


-- Exercise 5
-- 

compile :: String -> Maybe VM.Program

instance Expr VM.Program where
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]
  lit x = [VM.PushI x]

compile x = parseExp lit add mul x 
