import Control.Arrow
import Control.Monad (join)
import Data.Function (on)
import Data.Char (digitToInt)
import Data.List (group, maximumBy, minimumBy, sort, transpose)

main = do
    contents <- readFile "input.txt"
    let posbits = transpose $ map (map (:[])) (lines contents) :: [[String]]
    print $ computePower posbits

computePower:: [[String]] -> Int
computePower [] = error "Empty list"
computePower x = 
    let gamma = toDec $ join $ map mostCommon x
        epsilon = toDec $ join $ map leastCommon x
    in gamma * epsilon

leastCommon :: Ord a => [a] -> a
leastCommon [] = error "Empty list"
leastCommon list = fst . minimumBy (compare `on` snd) $ elemCount
      where elemCount = map (head &&& length) . group . sort $ list

mostCommon :: Ord a => [a] -> a
mostCommon [] = error "Empty list"
mostCommon list = fst . maximumBy (compare `on` snd) $ elemCount
      where elemCount = map (head &&& length) . group . sort $ list

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0