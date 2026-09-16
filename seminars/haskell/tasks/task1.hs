import System.Win32 (COORD(yPos))

main :: IO ()
main = do
    print $ minIf 15 60 == 15
    print $ minIf 60 15 == 15
    print $ minGuard 15 60 == 15
    print $ minGuard 60 15 == 15
    print $ minBuiltIn 60 15 == 15

    print $ lastDigit 154 == 4

    print $ quotientWhole 64 2 == 32
    print $ divWhole 154 17 == 9.058823529411764

    print $ removeLastDigit 154 == 15    

    print $ divReal 154.451 10.01 == 15.42967032967033
    print $ quotientReal 154.21 17.17 == 8

    -- print $ avgWhole 5 1542 == 773.5

    -- print $ roundTwoDig 3.1413465345321 == 3.14
    -- print $ roundTwoDigButWithMagic 3.1413465345321 == 3.14



minIf :: Ord a => a -> a -> a
minIf x y = if x > y then y else x
-- >>> minIf 2 3
-- 2

minGuard :: Ord a => a -> a -> a
minGuard x y
 | x > y = y
 | otherwise  = x
-- >>> minGuard 2 5
-- 2

minBuiltIn :: Ord a => a -> a -> a
minBuiltIn x y = min x y

lastDigit :: Integral a => a -> a
lastDigit x = mod x 10

quotientWhole :: Integral a => a -> a -> a
quotientWhole x y = div x y 

divWhole :: Fractional a => Integer -> Integer -> a
divWhole x y = fromInteger x / fromInteger y

removeLastDigit :: Integral a => a -> a
removeLastDigit x = div x 10

divReal :: Fractional a => a -> a -> a
divReal a b = a / b

quotientReal :: (RealFrac a, Integral b) => a -> a -> b
quotientReal a b = truncate $ a / b
