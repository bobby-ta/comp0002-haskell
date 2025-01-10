giveSquares :: [Int] -> [Int]
giveSquares ns = map (^2) ns

sumSquares ns = sum (map (^2) ns)

allPos ns = null (filter (<=0) ns)
allPos' ns = all (>0) ns

funky n = 2*n^3 - 50*n^2 + 7*n + 10

minOutput f ns = minimum (map f ns)

allEqual f ns = all (==x) (x:xs) where
    x:xs = map f ns

allOutPos f ns = map (>0) (map f ns)

outInOrder f ns = inOrder (map f ns) where
    inOrder [x] = True
    inOrder (x:y:ys)
        | x > y = False
        | otherwise = inOrder (y:ys)

iter :: Int -> (a -> a) -> a -> a
iter n f x
    | n == 0 = x
    | otherwise = f (iter (n-1) f x)

doubleMe x = x * 2

twoToN :: Int -> Int
twoToN n = iter n doubleMe 1