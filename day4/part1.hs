import Data.List (transpose)

main = do
    rawnumbers <- readFile "numbers.txt"
    rawboards <- readFile "boards.txt"
    let numbers = map read $ wordsWhen (==',') rawnumbers :: [Int]
    let boards = parseBoards rawboards
    let (num_across, result_across) = playBingo (zip [0.. ] numbers) boards
    let (num_up, result_up) = playBingo (zip [0.. ] numbers) $ map transpose boards
    if num_up <= num_across
        then print result_up
        else print result_across

playBingo :: [(Int, Int)] -> [[[Int]]] -> (Int, Int)
playBingo [] _ = error "No board won the game"
playBingo ((i, x):xs) boards =
    let boardsnew = removeNumber x boards
        y = getSuccess boardsnew in case y of
            Nothing -> playBingo xs boardsnew
            Just y -> (i, sum (map sum y) * x)

getSuccess :: [[[Int]]] -> Maybe [[Int]]
getSuccess x 
    | null y = Nothing
    | otherwise = Just $ head y
    where y = filter getSuccess' x

getSuccess' :: [[Int]] -> Bool
getSuccess' [] = False
getSuccess' (x:xs)
    | null x = True
    | otherwise = getSuccess' xs

removeNumber :: Int -> [[[Int]]] -> [[[Int]]]
removeNumber x = map (map (filter (/=x)))

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