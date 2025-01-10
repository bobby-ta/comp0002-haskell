--Question 1
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

--Question 2
tribonacci :: Int -> [Int]
--tribonacci n = map tribonacci' [0..n] where
    --tribonacci' 0 = 0
    --tribonacci' 1 = 0
    --tribonacci' 2 = 1
    --tribonacci' n = tribonacci' (n - 1) + tribonacci' (n - 2) + tribonacci' (n - 3)
tribonacci n = take (n) (map head (iterate (\[a, b, c] -> [b, c, a + b + c]) [0, 0, 1]))

lazycaterer :: Int -> [Int]
lazycaterer n = take (n) (map head (iterate (\[a, b] -> [b, b + (b - a + 1)]) [1, 2]))