main :: IO()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True

isPerfect :: Integral a => a -> Bool
isPerfect x = x == sum [y | y <- [1..div x 2], mod x y == 0]