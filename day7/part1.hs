main = do
    raw_crab_pos <- readFile "input.txt"
    let crab_pos = map read $ wordsWhen (==',') raw_crab_pos :: [Int]
    let min_pos = minimum crab_pos
        max_pos = maximum crab_pos in
            print $ (minimum . map (calcFuel crab_pos)) [min_pos..max_pos]

calcFuel :: [Int] -> Int -> Int
calcFuel crab_pos align_pos = (sum . map (\x -> abs (x - align_pos))) crab_pos

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'