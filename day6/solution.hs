-- Note this solution works for both part 1 and part 2

main = do
    raw_fish_state <- readFile "input.txt"
    let fish_state = map read $ wordsWhen (==',') raw_fish_state :: [Int]
    print $ simulateFish 80 $ getCounts fish_state
    print $ simulateFish 256 $ getCounts fish_state

simulateFish :: Int -> [Int] -> Int
simulateFish 0 fishcounts = sum fishcounts
simulateFish days fishcounts = simulateFish (days - 1) (spawnNew fishcounts)            

spawnNew :: [Int] -> [Int]
spawnNew fishcounts =
    let x = take 7 fishcounts in 
        tail x ++ [head x + fishcounts!!7] ++ [fishcounts!!8] ++ [head x]

getCounts :: [Int] -> [Int]
getCounts x = map (getCounts' x) [0..8]

getCounts' :: [Int] -> Int -> Int
getCounts' y x = (length.filter (x==)) y

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'