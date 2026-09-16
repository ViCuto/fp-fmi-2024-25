repeat1 :: t -> [t]
repeat1 x = x : repeat1 x

-- >>> take 5 $ repeat 5
-- [5,5,5,5,5]

-- >>> take 8 $ cycle1 [1,2,3]
-- [1,2,3,1,2,3,1,2]
cycle1 :: [a] -> [a]
cycle1 xs = xs ++ cycle1 xs 

-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


-- >>> take 7 facts
-- [1,1,2,6,24,120,720]
facts :: [Integer]
facts = scanl (*) 1 [1..]


nats :: [Integer]
nats = [1..]

isPrime :: Integral a => a -> Bool
isPrime n = n /= 1 && null [x | x <- [2..div n 2], mod n x == 0]


primes :: [Integer]
primes = filter isPrime nats

-- >>> take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]


natTuples :: [(Integer, Integer)]
natTuples = [(x, y) | s <- nats, x <- [1..s], y <- [1..s], x + y == s]

-- >>> take 10 natTuples
-- [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]

compositions :: [String]
compositions = undefined

filterTuples :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
filterTuples p xs ys = [(x,y) | x <- xs, y <- ys, p x y]