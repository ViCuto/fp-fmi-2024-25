main :: IO()
main = do
    print $ revOneLine 123 == 321
    print $ revOneLineMagic 123 == 321


revOneLine :: Int -> Int
revOneLine x = read $ reverse $ show x
-- >>> read "14521" :: Int
-- 14521

revOneLineMagic :: Int -> Int
revOneLineMagic = read . reverse . show
