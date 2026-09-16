{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

exists :: (a -> Bool) -> ([a] -> Bool)
exists p = foldr (\x b -> p x || b) False

member :: Eq a => a -> [a] -> Bool
member x = exists (== x)

forall1 :: Foldable t1 => (t2 -> Bool) -> t1 t2 -> Bool
forall1 p = foldr (\x xs -> p x && xs) True

listMap :: (t -> a) -> [t] -> [a]
listMap _ [] = []
listMap f (x:xs) = f x : listMap f xs

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter  _ [] = []
listFilter  f (x:xs) = if f x then x : listFilter f xs else listFilter f xs

push :: t -> [t] -> [t]
push y [] = [y]
push y (x:xs) = x : push y xs

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

insert :: t1 -> Int -> [t1] -> [t1]
insert x 0 ys = x : ys
insert _ _ [] = []
insert x n (y:ys) = y : insert x (n - 1) ys

append :: [t] -> t -> [t]
append [] y = [y]
append (x:xs) y = x : append xs y

listFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
listFoldr _ init1 [] = init1
listFoldr op init1 (x:xs) = op x (listFoldr op init1 xs)

listFoldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
listFoldl _ init1 [] = init1
listFoldl op init1 (x:xs) = op (listFoldl op init1 xs) x

sum1 :: Num t2 => [t2] -> t2
sum1 xs = listFoldr (+) 0 xs

-- Bonus
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (pivot:xs) =
    quickSort (listFilter (<= pivot) xs)
    ++ [pivot] ++
    quickSort (listFilter (> pivot) xs)