{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
import Data.Binary.Get (Decoder(Done))
import Data.IntMap (difference)

data Task 
    = Done
    | Undone
    deriving(Show)

data ArithExpr
    = Const Float               
    | Var String                
    | Add ArithExpr ArithExpr   
    | Sub ArithExpr ArithExpr   
    | Mul ArithExpr ArithExpr   
    | Div ArithExpr ArithExpr  
    | Pow ArithExpr Float      
    deriving (Show)

differentiate :: String -> ArithExpr -> ArithExpr
differentiate _ (Const _) = Const 0
differentiate var (Var x)
    | x == var  = Const 1
    | otherwise = Const 0
differentiate var (Add x y) = Add (differentiate var x) (differentiate var y)
differentiate var (Sub x y) = Sub (differentiate var x) (differentiate var y) 
differentiate var (Div x y) = Div (Sub (differentiate var x) (differentiate var y)) (Pow y 2)
differentiate var (Mul x y) = Add (Mul (differentiate var x) y) (Mul x (differentiate var y))
differentiate var (Pow x y) = 

