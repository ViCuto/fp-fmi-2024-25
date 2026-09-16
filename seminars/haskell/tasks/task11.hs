main :: IO()
main = do
    print $ addOneXs [1, 2, 3, 4, 5] == [2, 3, 4, 5, 6]
    print $ addOneN 5 == 6
    print $ sqPlusOne 5 == 26


addOneXs :: [Integer] -> [Integer]
addOneXs = map (+1) 

addOneN :: Integer -> Integer
addOneN = (1 +)

sqPlusOne :: Integer -> Integer
sqPlusOne = (1 + ) . (^2)