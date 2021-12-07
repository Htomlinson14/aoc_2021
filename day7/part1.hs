import Data.List (sort)

main = do
    raw_crab_pos <- readFile "input.txt"
    let crab_pos = map read $ wordsWhen (==',') raw_crab_pos :: [Int]
    let min_pos = minimum crab_pos
        max_pos = maximum crab_pos
        med_pos = median crab_pos
        max_int = 2147483647
        best_low = searchFuel (-1) max_int med_pos min_pos crab_pos
        best_high = searchFuel 1 max_int med_pos max_pos crab_pos
            in print $ minimum [best_low, best_high]

searchFuel :: Int -> Int -> Int -> Int -> [Int] -> Int
searchFuel increment curr_best curr_pos limit crab_pos
    | (curr_pos == limit) || (fuel > curr_best) = curr_best
    | otherwise = searchFuel increment fuel (curr_pos + increment) limit crab_pos
    where fuel = calcFuelTotal crab_pos curr_pos

calcFuelTotal :: [Int] -> Int -> Int
calcFuelTotal crab_pos align_pos = (sum . map (\x -> abs (x - align_pos))) crab_pos

median :: [Int] -> Int
median x = 
    let l = length x
        mid = div l 2
        in sort x !! mid

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'