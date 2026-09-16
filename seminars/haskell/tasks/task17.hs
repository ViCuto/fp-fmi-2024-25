
myIterate :: (t -> t) -> t -> [t]
myIterate  f x = x : myIterate  f (f x)

-- >>> (take 8 $ myIterate (*2) 5) == [5, 10, 20, 40, 80, 160, 320, 640]
-- True

nats :: [Integer]
nats = [1..]

natPairs :: [(Integer, Integer)]
natPairs = [(x, y) | s <- nats, x <- [1 .. s], y <- [1 .. s], x + y == s]

-- >>> take 10 natPairs == [(1,1), (1,2), (2,1), (1,3), (2,2), (3,1), (1,4), (2,3), (3,2), (4,1)]
-- True

ints :: [Integer]
ints = [x | y <- [0..], x <-[-y..y]]

-- >>> take 10 ints 
-- [0,-1,0,1,-2,-1,0,1,2,-3]


gen_KS :: Int -> Int -> [[Int]]
gen_KS 1 s = [[s]]
gen_KS k s = [ h : t | h <- [0 .. s], t <- gen_KS (k - 1) (s - h)]

isUniqueList :: [Int] -> Bool
isUniqueList [] = True
isUniqueList (x:xs) = not (x `elem` xs) && isUniqueList xs

isNPerm :: Int -> (Int -> Int) -> Bool
isNPerm n f = isUniqueList permList && all (\x -> (elem x [0..n-1])) permList
    where
        permList :: [Int]
        permList = [(f x) | x <- [0..n-1]]

-- >>> isNPerm 3 (\x -> (3 - x) `mod` 3) == True
-- True
-- >>> isNPerm 10 (`div` 2) == False
-- True
-- >>> isNPerm 10 (\x -> (x + 2) `mod` 10) == True
-- True

takeFirst :: [(Int, Int)] -> [Int]
takeFirst [] = []
takeFirst ((x, _):xs) = x : takeFirst xs

maxCycle :: Int -> (Int -> Int) -> [Int] 
maxCycle n f =  perm
    where
        perm = [(f x) | x <- [0.. n-1]]

-- >>> quickSort [321,421,1,5,21,2]
-- [1,2,5,21,321,421]



movingAverage :: [Float] -> Int -> [Float]
movingAverage _ 0 = []  -- Базов случай: ако n е 0, връщаме празен списък
movingAverage (x:xs) n = (avg [x] (take (n-1) xs)) : movingAverage xs n
    where
        -- Функция, която изчислява средната стойност на първите n елемента
        avg :: [Float] -> [Float] -> Float
        avg xs ys = (sum xs + sum ys) / fromIntegral n

-- >>>  
-- parse error on input `]'







quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (pivot : xs) = quickSort [x | x <- xs,  x <= pivot]
                        ++ [pivot] ++
                        quickSort [x | x <- xs, x > pivot]

