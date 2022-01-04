{-# LANGUAGE TupleSections #-}

import qualified Data.Text as T

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let coord_limits = map getCoordLimits (lines contents)
    -- let coords = concatMap limitsToCoords coord_limits
    let coords = concatMap limitsToCoordswithDiagonal coord_limits
    let field = getField coords 1000
    print $ countOverlaps field

countOverlaps:: [[Int]] -> Int
countOverlaps = sum . map (length . filter (>1))

getField:: [(Int, Int)] -> Int -> [[Int]]
getField coords field_size = 
    let field = replicate field_size (replicate field_size 0) in
        foldl getField' field coords

getField':: [[Int]] -> (Int, Int) -> [[Int]]
getField' field coord =
    let (fx_before,fx:fx_after) = splitAt (fst coord) field
        (fy_before,fy:fy_after) = splitAt (snd coord) fx
        fx_new = fy_before ++ (fy + 1) : fy_after
        field_new = fx_before ++ fx_new : fx_after
        in field_new

getCoordLimits:: String -> [[Int]]
getCoordLimits x = 
    let noarrow = split "->" x
        nocomma = map (split ",") noarrow
        in map (map read) nocomma

limitsToCoords:: [[Int]] -> [(Int, Int)]
limitsToCoords coord_limits
    | x0 == x1 = map (x0,) (getRange y0 y1)
    | y0 == y1 = map (, y0) (getRange x0 x1)
    | otherwise = []
    where
        x0 = head (head coord_limits)
        y0 = last (head coord_limits)
        x1 = head (last coord_limits)
        y1 = last (last coord_limits)

limitsToCoordswithDiagonal:: [[Int]] -> [(Int, Int)]
limitsToCoordswithDiagonal coord_limits
    | x0 == x1 = map (x0,) (getRange y0 y1)
    | y0 == y1 = map (, y0) (getRange x0 x1)
    | otherwise = zip (getRange x0 x1) (getRange y0 y1)
    where
        x0 = head (head coord_limits)
        y0 = last (head coord_limits)
        x1 = head (last coord_limits)
        y1 = last (last coord_limits)

getRange:: Int -> Int -> [Int]
getRange a b
    | a < b = [a..b]
    | a > b = [a, a-1..b]
    | otherwise = error "Don't use this function for horizontal lines"

split :: String -> String -> [String]
split y x = map (T.unpack . T.strip) $ T.splitOn (T.pack y) (T.pack x)