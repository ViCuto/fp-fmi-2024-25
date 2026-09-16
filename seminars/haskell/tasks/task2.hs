
main :: IO ()
main = do
    print $ factRec 11 == 39916800
    print $ factIter 11 == 39916800
    print $ factXs 11 == 39916800


factRec :: (Eq t, Num t) => t -> t
factRec 0 = 1
factRec x = x * factRec (x - 1)

factIter :: Int -> Int
factIter x = helper x 1
    where
        helper :: Int -> Int -> Int
        helper 0 result = result
        helper current result = helper (current - 1) (result * current)


factXs :: (Num a, Enum a) => a -> a
factXs x = product [1..x]
