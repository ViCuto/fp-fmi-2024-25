import GHC.Base (VecElem(Int16ElemRep))
import Distribution.Simple.BuildTarget (resolveBuildTargets)
main :: IO()
main = do
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False


getReverseNumber :: Int -> Int
getReverseNumber n = helper n 0
    where
        helper :: Int -> Int -> Int
        helper 0 result = result
        helper current result = helper (div current 10) (result * 10 + mod current 10)

isPalindrome :: Int -> Bool
isPalindrome x = x == getReverseNumber x