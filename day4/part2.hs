import Data.List (transpose)

main = do
    rawnumbers <- readFile "numbers.txt"
    rawboards <- readFile "boards.txt"
    let numbers = map read $ wordsWhen (==',') rawnumbers :: [Int]
    let boards = parseBoards rawboards
    print $ playBingo numbers boards

playBingo :: [Int] -> [[[Int]]] -> Int
playBingo [] _ = error "No board won the game"
playBingo x boards 
    | length boards == 1 = scoreBingo x boards
    | otherwise = filterBingo x boards

filterBingo :: [Int] -> [[[Int]]] -> Int
filterBingo [] _ = error "No board won the game"
filterBingo (x:xs) boards  = 
    let boardsnew = filter (not . getSuccess) $ removeNumber x boards in
        playBingo xs boardsnew
        
scoreBingo :: [Int] -> [[[Int]]] -> Int
scoreBingo [] _ = error "No board won the game"
scoreBingo (x:xs) boards =
    let boardsnew = removeNumber x boards
        success = getSuccess $ head boardsnew in 
            if success 
                then sum (map (sum . filter (>0)) $ head boardsnew) * x
                else scoreBingo xs boardsnew

getSuccess :: [[Int]] -> Bool
getSuccess x = getSuccess' x || getSuccess' (transpose x)

getSuccess' :: [[Int]] -> Bool
getSuccess' [] = False
getSuccess' (x:xs)
    | sum x == -5 = True
    | otherwise = getSuccess' xs

removeNumber :: Int -> [[[Int]]] -> [[[Int]]]
removeNumber x = map (map (map (replaceMinusOne x)))

replaceMinusOne:: Int -> Int -> Int
replaceMinusOne x y
    | x == y = -1
    | otherwise = y

parseBoards :: String -> [[[Int]]]
parseBoards x = let x' = map (map read.words) $ lines x :: [[Int]] in
    parseBoards' x' []

parseBoards' :: [[Int]] -> [[[Int]]] -> [[[Int]]]
parseBoards' [] a = a
parseBoards' x a
    | null (head x) = parseBoards' (tail x) a
    | otherwise = parseBoards' (drop 5 x) (a ++ [take 5 x])

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'