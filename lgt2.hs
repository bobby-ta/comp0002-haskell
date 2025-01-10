safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

safetail' xs = if xs == [] then [] else tail xs

safetail'' xs
    | xs == [] = []
    | otherwise = tail xs

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)
    | n < x = n:x:xs
    | otherwise = x : insert n xs

elemNum :: (Eq a) => a -> [a] -> Integer
elemNum x [] = 0
elemNum x (y:ys)
    | x == y = 1 + elemNum x ys
    | otherwise = elemNum x ys


uniqueHelper :: (Eq a) => [a] -> [a] -> [a] -> [a]
--xs is raw input array
--ys is input array for recursion (gets shrunk)
-- zs is output array
uniqueHelper xs [] zs = zs
uniqueHelper xs (y:ys) zs
    | elemNum y xs == 1 = uniqueHelper xs ys (y:zs) --add y to output, keep going thru ys
    | otherwise = uniqueHelper xs ys zs

unique xs = uniqueHelper xs xs []

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert2 x (isort xs)

insert2 :: Ord a => a -> [a] -> [a]
insert2 n [] = [n]
insert2 n (x:xs)
    | elemNum n (x:xs) > 0 = x:xs
    | n > x = n:x:xs
    | otherwise = x : insert2 n xs

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x:xs) = f x (Main.foldr1 f xs)
