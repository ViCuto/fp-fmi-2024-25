{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors


nats :: [Integer]
nats = [1..]

-- >>> take 5 nats
-- [1,2,3,4,5]

isPrime :: Integer -> Bool
isPrime x = x /= 1 && null [y | y <- [2..div x 2], x `mod` y == 0]

primes :: [Integer]
primes = filter isPrime nats

-- >>> take 5 primes
-- [2,3,5,7,11]

iterate2 :: (t -> t) -> t -> [t]
iterate2 f x = x : iterate2 f (f x)

-- >>> take 5 $ iterate2 (1+) 1
-- [1,2,3,4,5]

rats :: [(Integer, Integer)]
rats = (0,1) : [(x, y) | y <- [1..], x <- [-y..y], gcd x y == 1, x /= 0]

-- >>> take 5 $ rats
-- [(0,1),(-1,1),(1,1),(-1,2),(1,2)]


fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- >>> take 5 fibs
-- [0,1,1,2,3]


data BinTree a =
    Node a (BinTree a) (BinTree a)
    | Empty
    deriving (Show)


trimBinTree :: Integer -> BinTree a -> BinTree a
trimBinTree 0 _ = Empty
trimBinTree _ Empty = Empty
trimBinTree depth (Node val left right) = Node val (trimBinTree (depth - 1) left) (trimBinTree (depth - 1) right)

babaTree :: BinTree String
babaTree = generateTree ""
    where
    generateTree str = Node str (generateTree (str ++ "a")) (generateTree (str ++ "b"))

-- >>> trimBinTree 3 babaTree
-- Node "" (Node "a" (Node "aa" Empty Empty) (Node "ab" Empty Empty)) (Node "b" (Node "ba" Empty Empty) (Node "bb" Empty Empty))
