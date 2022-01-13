{-# LANGUAGE TupleSections #-}

import Data.Function (on)
import Data.List (lookup, sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T

main :: IO ()
main = do
    raw_contents <- readFile "input.txt"
    let contents = lines raw_contents
    let template = head contents
    let insertion_rules = map getRuleTuple ((tail . tail) contents)
    let pair_counts = pairMapSteps 40 template insertion_rules
    let char_counts = sortBy (compare `on` snd) $ pairCountstoCharCounts pair_counts template
    print $ snd (last char_counts) - snd (head char_counts)

pairCountstoCharCounts :: [(String, Int)] -> String -> [(Char, Int)]
pairCountstoCharCounts pair_map_list template = 
    let init_char_map = Map.fromList $ map (, 0) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        new_char_map = pairCountstoCharCounts' pair_map_list init_char_map
        char_map_head = Map.insertWith (+) (head template) 1 new_char_map
        char_map_last = Map.insertWith (+) (last template) 1 char_map_head
        char_map_list = Map.toList char_map_last
        in filter (\x -> snd x > 0) $ map (\x -> (fst x, snd x `div` 2)) char_map_list

pairCountstoCharCounts' :: [(String, Int)] -> Map.Map Char Int -> Map.Map Char Int
pairCountstoCharCounts' [] curr_char_map = curr_char_map
pairCountstoCharCounts' (x:xs) curr_char_map = 
    let curr_pair = fst x
        num_occur = snd x
        char_map_head = Map.insertWith (+) (head curr_pair) num_occur curr_char_map
        char_map_last = Map.insertWith (+) (last curr_pair) num_occur char_map_head
        in pairCountstoCharCounts' xs char_map_last

pairMapSteps :: Int -> String -> [(String, Char)] -> [(String, Int)]
pairMapSteps num_steps template insertion_rules = 
    let pair_map = buildMap template insertion_rules
    in Map.toList $ pairMapSteps' num_steps pair_map insertion_rules

pairMapSteps' :: Int -> Map.Map String Int -> [(String, Char)] -> Map.Map String Int
pairMapSteps' 0 pair_map _ = pair_map
pairMapSteps' num_steps pair_map insertion_rules = 
    pairMapSteps' (num_steps - 1) (splitMap pair_map insertion_rules) insertion_rules

splitMap :: Map.Map String Int -> [(String, Char)] -> Map.Map String Int
splitMap pair_map insertion_rules = 
    let pair_map_list = Map.toList pair_map
        init_map = initMap insertion_rules
        in splitMap' pair_map_list init_map insertion_rules

splitMap' :: [(String, Int)] -> Map.Map String Int -> [(String, Char)] -> Map.Map String Int
splitMap' [] curr_pair_map _ = curr_pair_map
splitMap' (x:xs) curr_pair_map insertion_rules = 
    let curr_pair = fst x
        num_occur = snd x
        char_to_insert = fromJust $ lookup curr_pair insertion_rules
        first_pair_map = Map.insertWith (+) [head curr_pair, char_to_insert] num_occur curr_pair_map
        snd_pair_map = Map.insertWith (+) [char_to_insert, last curr_pair] num_occur first_pair_map
        in splitMap' xs snd_pair_map insertion_rules

buildMap :: String -> [(String, Char)] -> Map.Map String Int
buildMap template insertion_rules = 
    let init_map = initMap insertion_rules in 
        buildMap' template init_map

buildMap' :: String -> Map.Map String Int -> Map.Map String Int
buildMap' [] pair_map = pair_map
buildMap' (x:xs) pair_map
    | length xs == 1 = new_pair_map
    | otherwise = buildMap' xs new_pair_map
    where new_pair_map = Map.insertWith (+) [x, head xs] 1 pair_map

initMap :: [(String, Char)] -> Map.Map String Int
initMap x = Map.fromList $ map ((, 0) . fst) x

getRuleTuple:: String -> (String, Char)
getRuleTuple x = let y = split "->" x in (head y, head (last y))

split :: String -> String -> [String]
split y x = map (T.unpack . T.strip) $ T.splitOn (T.pack y) (T.pack x)