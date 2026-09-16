import System.Win32 (xBUTTON1)
main :: IO ()
main = do
    print $ fibRec 11 == 89
    print $ fibIter 11 == 89
    print $ fibIter 110 == 43566776258854844738105

fibRec :: (Eq t, Num t, Num a) => t -> a
fibRec x
 | x == 0 = 0
 | x == 1 = 1
 | otherwise = fibRec (x - 1) + fibRec (x - 2)




fibIter :: Int -> Integer
fibIter x = helper 0 1 x
    where
        helper :: Integer -> Integer -> Int -> Integer
        helper n0 _ 0 = n0
        helper _ n1 1 = n1
        helper n0 n1 current = helper n1 (n0 + n1) (current - 1) 