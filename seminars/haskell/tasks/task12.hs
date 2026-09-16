isPalindrome :: Int -> Bool
isPalindrome x = show x == reverse (show x)

sumMinMaxPalindromes :: Int -> Int
sumMinMaxPalindromes x = minVal + maxVal
    where 
        devAndPalindromeList x =  if isPalindrome x
             then x : [y | y <- [2..div x 2], x `mod` y == 0, isPalindrome y]
             else [y | y <- [2..div x 2], x `mod` y == 0, isPalindrome y]
        minVal = minimum $ devAndPalindromeList x
        maxVal = maximum $ devAndPalindromeList x

main :: IO()
main = do
    print $ sumMinMaxPalindromes 132465 == 8
    print $ sumMinMaxPalindromes 654546 == 8
    print $ sumMinMaxPalindromes 100001 == 100012
    print $ sumMinMaxPalindromes 21612 == 21614
    print $ sumMinMaxPalindromes 26362 == 26364
