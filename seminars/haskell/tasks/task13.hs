import Data.Char
import Data.List

numToXs :: Int -> [Int]
numToXs = map digitToInt . show

isAscending :: Int -> Bool
isAscending x = numToXs x == (sort $ numToXs x)

main :: IO()
main = do
    print $ isAscending 123 == True
    print $ isAscending 122 == True
    print $ isAscending 0 == True
    print $ isAscending 10 == False
    print $ isAscending 12340 == False
    print $ isAscending 12349 == True