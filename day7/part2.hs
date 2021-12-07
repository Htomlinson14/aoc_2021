main = do
    raw_crab_pos <- readFile "input.txt"
    let crab_pos = map read $ wordsWhen (==',') raw_crab_pos :: [Int]
    let min_pos = minimum crab_pos
        max_pos = maximum crab_pos in
            print $ (minimum . map (calcFuelTotal crab_pos)) [min_pos..max_pos]

calcFuelTotal :: [Int] -> Int -> Int
calcFuelTotal crab_pos align_pos = (sum . map (\x -> calcFuel (x - align_pos))) crab_pos

calcFuel :: Int -> Int
calcFuel x = let y = abs x in sum [0..y]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'