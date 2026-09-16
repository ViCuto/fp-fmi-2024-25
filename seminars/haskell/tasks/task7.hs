main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

areAmicable :: Integral a => a -> a -> Bool
areAmicable a b = (sum [x | x <- [1..a], (mod a x) == 0]) == (sum [x | x <- [1..b], (mod b x) == 0])