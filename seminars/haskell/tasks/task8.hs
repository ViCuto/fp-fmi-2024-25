main :: IO()
main = do
    print $ isPrime 1 == False
    print $ isPrime 2 == True
    print $ isPrime 3 == True
    print $ isPrime 6 == False
    print $ isPrime 61 == True

isPrime :: Integral a => a -> Bool
isPrime x = x /= 1 && null [y | y <- [2.. (div x 2)], (mod x y) == 0]