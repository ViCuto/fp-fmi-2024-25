{-# LANGUAGE EmptyDataDeriving #-}

module Expr where

import Prelude hiding (lookup, unlines, unwords)

data Expr
  = Var String
  | Val Integer
  | Oper OperType Expr Expr
  | If Expr Expr Expr
  | SumList [Expr]
  | Sum String Expr Expr
  deriving (Show)

data OperType
  = Plus 
  | Mult 
  deriving (Show)

type Context = [(String, Integer)]

extend :: String -> Integer -> Context -> Context
extend x n = ((x, n) :)

lookup :: String -> Context -> Maybe Integer
lookup _ [] = Nothing
lookup x ((y,z) : xs)
  | x == y = Just z
  | otherwise = lookup x xs

maybeAndThen :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeAndThen Nothing _ = Nothing
maybeAndThen (Just x) f = f x

infixl 1 `maybeAndThen`

traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe _ [] = Just []
traverseListMaybe f (x:xs) = f x `maybeAndThen` \y ->
  traverseListMaybe f xs `maybeAndThen` \ys ->
  Just (y : ys)

freeVars :: Expr -> [String]
freeVars (Var x) = [x]
freeVars (Val _) = []
freeVars (Oper _ x y) = freeVars x ++ freeVars y
freeVars (If x y z) = freeVars x ++ freeVars y ++ freeVars z
freeVars (SumList xs) = concatMap freeVars xs
freeVars (Sum x y z) = freeVars y ++ filter (/= x) (freeVars z)

eval :: Context -> Expr -> Maybe Integer
eval x (Var y) = lookup y x
eval _ (Val x) = Just x
eval x (Oper Plus y z) = do
  v1 <- eval x y
  v2 <- eval x z
  return (v1 + v2)
eval x (Oper Mult y z) = do
  v1 <- eval x y
  v2 <- eval x z
  return (v1 * v2)
eval x (If y z u) = do
  v <- eval x y
  if v /= 0 then eval x u else eval x z
eval x (SumList xs) = foldr addEval (Just 0) xs
  where
    addEval y ys = do
      v <- eval x y  
      rest <- ys      
      return (v + rest) 
eval x (Sum y z u) = do
  start <- eval x z
  if start < 0
    then Just 0
    else sumLoop 0 0 start
  where
    sumLoop acc i limit
      | i > limit = Just acc
      | otherwise = do
          rest <- eval ((y, i) : x) u
          sumLoop (acc + rest) (i + 1) limit

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse y (x:xs) = x : y : intersperse y xs

unwords :: [String] -> String
unwords [] = ""
unwords [x] = x
unwords (x : xs) = x ++ " " ++ unwords xs

unlines :: [String] -> String
unlines [] = ""
unlines [x] = x
unlines (x : xs) = x ++ "\n" ++ unlines xs

solvingCompiler :: Bool
solvingCompiler = True

newtype RacketProgram = MkRacketProgram [RacketExpr]

data RacketExpr
  = Name String
  | List [RacketExpr]
  deriving (Show)

printRacketExpr :: RacketExpr -> String
printRacketExpr (Name x) = x
printRacketExpr (List xs) = "(" ++ unwords (map printRacketExpr xs) ++ ")"

printRacketProgram :: Context -> RacketProgram -> String
printRacketProgram x (MkRacketProgram y) = unlines (["#lang racket"] ++ map printContextEntry x ++ map printRacketExpr y)

--helper
printContextEntry :: (String, Integer) -> String
printContextEntry (x, y) = "(define " ++ x ++ " " ++ show y ++ ")"

compileToRacket :: Expr -> RacketExpr
compileToRacket (Val x) = Name (show x) 
compileToRacket (Var x) = Name x         
compileToRacket (Oper Plus x y) = List [Name "+", compileToRacket x, compileToRacket y]  
compileToRacket (Oper Mult x y) = List [Name "*", compileToRacket x, compileToRacket y]  
compileToRacket (If x y z) = List [Name "if", List [Name "zero?", compileToRacket x], compileToRacket y, compileToRacket z]
compileToRacket (SumList []) = Name "0"  
compileToRacket (SumList xs) = List (Name "+" : map compileToRacket xs)  
compileToRacket (Sum _ _ _) = error "not supported"  

solvingPartialEval :: Bool
solvingPartialEval = True

partialEval :: Expr -> Expr
partialEval (Val x) = Val x
partialEval (Var x) = Var x
partialEval (Oper Plus x y) =
  let x' = partialEval x
      y' = partialEval y
   in case (x', y') of
        (Val n1, Val n2) -> Val (n1 + n2)
        _ -> Oper Plus x' y'
partialEval (Oper Mult x y) =
  let x' = partialEval x
      y' = partialEval y
   in case (x', y') of
        (Val n1, Val n2) -> Val (n1 * n2)
        _ -> Oper Mult x' y'
partialEval (If x y z) =
  let x' = partialEval x
      y' = partialEval y
      z' = partialEval z
   in case x' of
        Val 0 -> y'
        Val _ -> z'
        _     -> If x' y' z'
partialEval (SumList x) =
  let simplified = map partialEval x
   in if all isVal simplified
        then Val (sum (map getVal simplified))
        else SumList simplified
partialEval (Sum _ _ _) = error "not supported"

--helper
isVal :: Expr -> Bool
isVal (Val _) = True
isVal _ = False

--helper
getVal :: Expr -> Integer
getVal (Val n) = n
getVal _ = error "getVal: not a value"

solvingSum :: Bool
solvingSum = False
