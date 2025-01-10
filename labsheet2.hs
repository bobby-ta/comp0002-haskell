import Data.Char

inRange :: Int -> Int -> [Int] -> [Int]
inRange x y [] = []
inRange x y (z:zs)
    | x <= z && z <= y = z : (inRange x y zs)
    | otherwise = inRange x y zs

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (y:ys)
    | y > 0 = 1 + countPositives ys
    | otherwise = countPositives ys

capitalised xs = capAux xs xs where
    capAux xs "" = ""
    capAux xs (y:ys)
        | y:ys == xs = (toUpper y) : (capAux xs ys)
        | otherwise = (toLower y) : (capAux xs ys)

--alternatively
capitalised' (x:xs) = (toUpper x) : (decap xs) where
    decap "" = ""
    decap (x:xs) = (toLower x) : (decap xs)

title :: [String] -> [String]
title (x:xs) = (capitalised' x) : (titleAux xs) where
    decap "" = ""
    decap (x:xs) = (toLower x) : (decap xs)

    titleAux [] = []
    titleAux (x:xs)
        | length x >= 4 = (capitalised' x) : (titleAux xs)
        | otherwise = (decap x) : (titleAux xs)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs) where
    insert x [] = [x]
    insert x (y:ys)
        | x < y = x:y:ys
        | otherwise = y : (insert x ys)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys)


rotor :: Int -> String -> String
rotor n [] = []
rotor 0 str = str
rotor n (x:xs)
    | n < 0 = error "Shift must be positive integer"
    | n > length (x:xs) = error "Shift must be less than string length"
    | otherwise = rotor (n-1) (xs ++ [x])

--makeKey :: Int -> [(Char, Char)]
makeKey offset = zip ['A'..'Z'] (rotor offset ['A'..'Z'])

lookUp mykey [] = mykey
lookUp mykey (x:xs)
    | (fst x) == mykey = snd x
    | otherwise = lookUp mykey xs

encipher :: Int -> Char -> Char
encipher n c = lookUp c (makeKey n)

normalise :: String -> String
normalise [] = []
normalise (x:xs)
    | x `elem` ['a'..'z'] = (toUpper x) : normalise xs
    | x `elem` ['A'..'Z'] || x `elem` ['1'..'9'] = x : normalise xs
    | otherwise = normalise xs

encipherStr :: Int -> String -> String
encipherStr n str = map (encipher n) (normalise str)

encipherStr' :: Int -> String -> String
encipherStr' n str = encipherAux n (normalise str) where
    encipherAux n [] = []
    encipherAux n (x:xs) = (encipher n x) : (encipherAux n xs)