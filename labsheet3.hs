mult :: Num a => [a] -> a
mult xs = foldr (*) 1 xs

posList xs = filter (>0) xs

trueList xs = xs == filter (== True) xs

evenList xs = xs == filter (\x -> x `mod` 2 == 0) xs

maxList :: (Ord a) => [a] -> a
maxList (x:xs) = foldr max x (x:xs)

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = filter (\x -> x >= lo && x <= hi) xs 

countPositives xs = length (posList xs)

myLength :: [a] -> Int
--turn each element into 1, add up
myLength xs = foldr (+) 0 (map (\x -> 1) xs)

myMap f xs = foldr (\x acc -> (f x) : acc) [] xs

myLength' xs = foldr (\x acc -> acc + 1) 0 xs    