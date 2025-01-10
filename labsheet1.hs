import Data.Char

isTriple :: Int -> Int -> Int -> Bool
isTriple x y z = x^2 + y^2 == z^2

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny x y z = isTriple x y z || isTriple y z x || isTriple z x y

halfEvens :: [Int] -> [Int]
halfEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]
inRange x y zs = [z | z <- zs, z `elem` [x..y]]

countPositives :: [Int] -> Int
countPositives xs = sum [1 | x <- xs, x > 0]

capitalised :: String -> String
capitalised (x:xs) = toUpper x : [toLower y | y <- xs]

lowercased :: String -> String
lowercased xs = [toLower x | x <- xs]

title :: [String] -> [String]
title [] = []
title (x:xs) = capitalised x : [if length y >= 4 then capitalised y else lowercased y | y <- xs]