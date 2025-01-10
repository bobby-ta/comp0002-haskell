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