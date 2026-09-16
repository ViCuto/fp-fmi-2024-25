upperLetters  :: [Char]
upperLetters  = ['A'..'Z']

-- >>> upperLetters
-- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

toLower :: Char -> Char
toLower c = if c `elem` upperLetters then toEnum (fromEnum c + 32) else c

-- >>> toLower 'a'
-- 'a'


strToLower :: String -> String
strToLower = map toLower

-- >>> strToLower "ASFvsdavmassfcADSA"
-- "asfvsdavmassfcadsa"


quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (pivot : xs) = quickSort (filter (<= pivot) xs)
                            ++ [pivot] ++
                         quickSort (filter (> pivot) xs)

sortString :: String -> String
sortString str = map toEnum (quickSort (map fromEnum (strToLower str)))

-- >>> sortString "ASFvsdavmassfcADSA"
-- "aaaaacddffmsssssvv"

brokenStr :: String -> [String]
brokenStr [] = []
brokenStr str = reverse (helper [] (sortString str))
    where
        helper :: [String] -> String -> [String]
        helper cur [] = cur
        helper [] (y:ys) = helper [[y]] ys
        helper (x : cur) (y : left) = if y `elem` x then helper ((y:x):cur) left
                                                    else helper ([y]:x:cur) left
-- >>> brokenStr "ASFvsdavmassfcADSA"
-- ["aaaaa","c","dd","ff","m","sssss","vv"]


countOccurrences :: String -> [(Char, Int)]
countOccurrences str  = map (\x -> (head x, length x)) (brokenStr str)

-- >>> countOccurrences "Test" == [('e',1),('s',1),('t',2)]
-- True

-- >>> countOccurrences "ThisIsAReallyLongWordContaingAlmostEveryCharacter" == [('a',6),('c',3),('d',1),('e',4),('g',2),('h',2),('i',3),('l',4),('m',1),('n',3),('o',4),('r',5),('s',3),('t',4),('v',1),('w',1),('y',2)]
-- True


-- >>> zipWith (+) [1,2,3] [4,5,6]
-- [5,7,9]


myAbs :: (Ord a, Num a) => a -> a
myAbs x = if x > 0 then x else  -x


apply :: (t1 -> t2) -> t1 -> t2
apply f x = f x

compose :: (a -> b) -> (t -> a) -> t -> b
compose f g = f . g


prefix :: Eq a => [a] -> [a] -> Bool
prefix xs ys = take (length xs) ys == xs

sufix :: Eq a => [a] -> [a] -> Bool
sufix xs ys = drop (length ys - length xs) ys == xs

myFilter :: Foldable t => (a -> Bool) -> t a -> [a]
myFilter p  = foldr (\x ys-> if p x then x : ys else ys) []


myMap :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
myMap f ys = foldr (\x xs -> f x : xs) [] ys

weakListComprehension :: (t -> a) -> (t -> Bool) -> [t] -> [a]
weakListComprehension f p xs = [f x | x <- xs, p x]

-- >>> weakListComprehension (^2) odd [1,2,3,4]
-- [1,9]

closed :: Eq a => (a -> a) -> [a] -> [a]
closed f xs = [x | x <- xs, f x `elem` xs]

-- >>> closed (^3) [1,2,0,-1,20]
-- [1,0,-1]

-- >>> concatMap (\x -> [x,-x]) [1,2,3]
-- [1,-1,2,-2,3,-3]
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr (\x acc -> f x ++ acc) []

removeElements :: Eq a => a -> [a] -> [a]
removeElements x [] = []
removeElements x (y:ys) = if x == y then removeElements x ys else x : removeElements x ys


nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)


prime :: Int -> Bool
prime x = 1 < x && null [y | y <- [2..div x 2], mod x y == 0]

-- >>> prime 4
-- False
-- >>> prime 0
-- False
--
-- >>> prime 1
-- False
--
-- >>> prime 2
-- True
--
-- >>> prime 3
-- True
--


-- >>> primes 7
-- [2,3,5,7,11,13,17]
primes :: Int -> [Int]
primes n = take n $ filter prime [2..]

factorize :: Int -> [Int]
factorize n = factorize' n [2..]
    where
        factorize' 1 _ = []
        factorize' n (p:ps)
            | n `mod` p == 0 = p : factorize' (n `div` p) (p:ps)
            | otherwise      = factorize' n ps

-- Примери:
-- >>> factorize 360
-- [2,2,2,3,3,5]
--


-- >>> take 10 nats
-- [0,1,2,3,4,5,6,7,8,9]
nats :: [Integer]
nats = [0..]

-- >>> take 5 $ repeat 'a'
-- "aaaaa"
repeat :: a -> [a]
repeat x = x : repeat x
