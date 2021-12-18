
import Data.List (sort)

main = do
    raw_input <- readFile "input.txt"
    let navlines = lines raw_input
    let noncorrupt_navlines = filterCorrupt navlines
    print $ scoreToComplete noncorrupt_navlines

filterCorrupt :: [String] -> [String]
filterCorrupt = filter (filterCorrupt' [])

filterCorrupt' :: [Char] -> String -> Bool
filterCorrupt' _ "" = True
filterCorrupt' stack (x:xs)
    | x `elem` ['(', '[', '{', '<'] = filterCorrupt' (x : stack) xs
    | null stack = False
    | otherwise = checkPair (head stack) x && filterCorrupt' (tail stack) xs

scoreToComplete :: [String] -> Int
scoreToComplete [] = 0
scoreToComplete x = median (map (scoreToComplete' [] 0 . reverse) x)

scoreToComplete' :: [Char] -> Int -> String -> Int
scoreToComplete' _ score "" = score
scoreToComplete' stack score (x:xs)
    | x `elem` [')', ']', '}', '>'] = scoreToComplete' (x : stack) score xs
    | null stack = scoreToComplete' stack (5 * score + scoreCharacter x) xs
    | otherwise = let lastclose = head stack in
        if checkPair x lastclose
            then scoreToComplete' (tail stack) score xs
            else scoreToComplete' stack (5 * score + scoreCharacter x) xs

checkPair :: Char -> Char -> Bool
checkPair x y
    | (x == '(') && (y == ')') = True
    | (x == '[') && (y == ']') = True
    | (x == '{') && (y == '}') = True
    | (x == '<') && (y == '>') = True
    | otherwise = False

scoreCharacter :: Char -> Int
scoreCharacter x
    | x == '(' = 1
    | x == '[' = 2
    | x == '{' = 3
    | x == '<' = 4
    | otherwise = error ("Unrecognised character: " ++ [x])

median :: [Int] -> Int
median x = 
    let l = length x
        mid = div l 2
        in sort x !! mid