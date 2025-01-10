--Section 2
type Horse = [[Char]]

horse :: Horse
horse = ["         ,---, ",
         "   _ ___/ / /| ",
         "  ;( )__, )    ",
         " ; //   '--;   ",
         "   ||     |    ",
         "    ^    ^     "]

horse2 = ["ABC", "DEF", "GHI"]

transpose :: Horse -> Horse
transpose [] = []
transpose ([]:_) = []
transpose xs = reverse (map head xs) : transpose (map tail xs)

mirror:: Horse -> Horse
mirror xs = map reverse xs

rotate180 :: Horse -> Horse
rotate180 xs = transpose (transpose xs)

rotate270 :: Horse -> Horse
rotate270 xs = reverse (mirror (transpose xs))

--Section 3
tribonacci :: Int -> [Int]
tribonacci n = take n (map head (iterate (\[a, b, c] -> [b, c, a + b + c]) [0, 0, 1]))

lazycaterer :: Int -> [Int]
lazycaterer n = take n (map head (iterate (\[a, b] -> [b, b + (b - a + 1)]) [1, 2]))

--Section 4
pretty :: Horse -> IO ()
pretty horse
    | all (\x -> null x) horse = return ()
    | otherwise = putStr (unlines horse)

horseSeq :: (Int -> [Int]) -> Int -> Horse -> IO ()
horseSeq f n h = mapM_ (\x -> pretty (consecutiveHorses x h)) (f n) where
    consecutiveHorses :: Int -> Horse -> Horse
    consecutiveHorses n h = map (\line -> concat (replicate n line))  h

--Section 5
shead :: [a] -> Maybe a
shead [] = Nothing
shead (x:_) = Just x

stail :: [a] -> Maybe [a]
stail [] = Nothing
stail (_:xs) = Just xs