quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (pivot : xs) = quickSort [x | x <- xs,  x <= pivot]
                        ++ [pivot] ++
                        quickSort [x | x <- xs, x > pivot]


intValue :: Integer
intValue = 5

dValue :: Double
dValue = 2.51


avg :: [Float] -> Float
avg [] = 0
avg xs = helper xs 0 0
    where
        helper :: [Float] -> Float -> Int -> Float
        helper [] sum n = sum / fromIntegral n
        helper (x : xs) sum n = helper xs (sum + x) (n + 1)


movingAverage :: [Float] -> Int -> [Float]
movingAverage [] _ = []
movingAverage xs n
    | length xs < n = []  -- Ако останалата част от списъка е по-къса от n, спираме
    | otherwise = avg (take n xs) : movingAverage (tail xs) n



allAverages :: [Float] -> [[Float]]
allAverages [] = []
allAverage xs = helper xs 2
    where
        helper :: [Float] -> Int -> [[Float]]
        helper xs n = movingAverage xs n : helper xs (n+1)

main :: IO ()
main = do
    print $ allAverages [1076,1356,1918,6252,6766,5525] 
    