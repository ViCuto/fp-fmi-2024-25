main :: IO()
main = do
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5

sqAvg :: Fractional a => a -> a -> a
sqAvg x y = (x ^ 2 + y ^ 2) / 2