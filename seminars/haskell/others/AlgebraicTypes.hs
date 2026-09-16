{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors


data MyBool
    = MyTrue
    | MyFalse
    deriving (Show)

data Colour
    = Red
    | Green
    | Cyan
    | Magenta
    | Yellow
    | Blue
    deriving (Show)

data Shape
    =
    Point
    | Circle Float
    | Rectangle Float Float
    | Ngon Int Float
    deriving (Show)


area :: Shape -> Float
area Point = 0
area (Circle r) = r * r * pi
area (Rectangle a b) = a * b
area (Ngon _ _) = error ""


perimeter :: Shape -> Float
perimeter Point = 0
perimeter (Circle _) = error ""
perimeter (Rectangle a b) = 2 * a + 2 * b  
perimeter (Ngon _ _) = error ""

data RPS
    = Rock
    | Paper
    | Scissors


beats :: RPS -> RPS -> Bool
beats Rock Scissors = True
beats Paper Rock = True
beats Scissors Paper = True
beats _ _ = False


data GShape n
    =
    GPoint
    | GCircle n
    | GRectangle n n
    | GNgon Int n
    deriving (Show)

garea :: Floating n => GShape n -> n
garea GPoint = 0
garea (GCircle r) = r * r * pi
garea (GRectangle a b) = a * b
garea (GNgon _ _) = error ""

data List a
    = Cons a (List a)
    | Empty


lmap :: (t -> a) -> List t -> List a
lmap _ Empty = Empty
lmap f (Cons x xs) = Cons (f x) (lmap f xs)

lfilter :: (a -> Bool) -> List a -> List a
lfilter _ Empty = Empty
lfilter p (Cons x xs) = if p x then Cons x (lfilter p xs) else lfilter p xs



lfoldr :: List (t1 -> t2 -> t2) -> t1 -> t2 -> t2
lfoldr Empty _ init1 = init1
lfoldr (Cons x xs) op init1 =  x op (lfoldr xs op init1)


isum :: Num a => List a -> a
isum Empty = 0
isum (Cons x xs) = x + isum xs


count2 :: List a -> Int
count2 Empty = 0
count2 (Cons _ xs) = 1 + count2 xs

class Countable a where
    count :: a -> Int

instance Countable (List a) where
    count Empty = 0
    count (Cons _ xs) = 1 + count xs

instance Countable [a] where
    count [] = 0
    count (_ : xs) = 1 + count xs

class Averageable a where
    avg :: a -> Float

-- instance Averageable (List Float) where
--         avg x = if count x == 0 
--              then 0 
--              else isum x / count x

instance Show RPS where
    show Rock = "🪨"
    show Paper = "🧻"
    show Scissors = "✂️"


-- instance (Show n) => Show (List n) where
-- show Empty = "List()"
-- show (Cons x xs) = (show xs) ++ (show x) ++ " "
