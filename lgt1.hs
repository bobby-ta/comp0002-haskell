firstPlusOne (x:xs) = x + 1

addSome [] = 0
addSome [x] = x
addSome (x:y:xs) = x + y

firstPlusOne' xs = succ (head xs)

addSome' xs
    | length xs >= 2 = head xs + head (tail xs)
    | length xs == 1 = head xs
    | otherwise = 0

firstDigit' [] = []
firstDigit' (x:xs)
    | x `elem` ['1'..'9'] = show x
    | otherwise = firstDigit' xs

exOr True True = False
exOr True False = True
exOr False True = True
exOr False False = False

exOr2 x y
    | x == y = False
    | otherwise = True