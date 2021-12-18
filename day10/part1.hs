main = do
    raw_input <- readFile "input.txt"
    let navlines = lines raw_input
    print $ scoreLines navlines

scoreLines :: [String] -> Int
scoreLines [] = 0
scoreLines x = sum (map (scoreLines' []) x)

scoreLines' :: [Char] -> String -> Int
scoreLines' _ "" = 0
scoreLines' stack (x:xs)
    | x `elem` ['(', '[', '{', '<'] = scoreLines' (x : stack) xs
    | null stack = scoreError x
    | otherwise = let lastopen = head stack in
        if checkPair lastopen x then scoreLines' (tail stack) xs
        else scoreError x

checkPair :: Char -> Char -> Bool
checkPair x y
    | (x == '(') && (y == ')') = True
    | (x == '[') && (y == ']') = True
    | (x == '{') && (y == '}') = True
    | (x == '<') && (y == '>') = True
    | otherwise = False

scoreError :: Char -> Int
scoreError x
    | x == ')' = 3
    | x == ']' = 57
    | x == '}' = 1197
    | x == '>' = 25137
    | otherwise = error ("Unrecognised closing character: " ++ [x])