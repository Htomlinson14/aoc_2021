import Control.Monad (join)
import qualified Data.Map.Lazy as Map
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

main = do
    contents <- readFile "input.txt"
    let posbits = map (map (:[])) (lines contents) :: [[String]]
    let o2rating = toDec . join . traverseMax $ mkBinTree posbits
        co2rating = toDec . join . traverseMin $ mkBinTree posbits
        in print $ o2rating * co2rating

data BinTree a = BinTree Int (Map.Map a (BinTree a)) deriving (Eq, Read, Show)

empty :: BinTree a
empty = BinTree 0 Map.empty

insert :: Ord a => [a] -> BinTree a -> BinTree a
insert [] (BinTree _ nodes) = BinTree 1 nodes
insert (x:xs) (BinTree num nodes) = BinTree (num + 1) (Map.alter (Just . insert xs . fromMaybe empty) x nodes)

mkBinTree :: Ord a => [[a]] -> BinTree a
mkBinTree as = mkBinTree' as empty
  where
    mkBinTree' [] bintree = bintree
    mkBinTree' (x:xs) bintree = mkBinTree' xs $ insert x bintree

binTreeLookup :: String -> BinTree String -> (Int, BinTree String)
binTreeLookup x (BinTree _ nodes) = let val = Map.lookup x nodes in case val of
  Just val -> ((\(BinTree num _) -> num) val, val)
  Nothing -> (0, empty)

traverseMax :: BinTree String -> [String]
traverseMax = traverseMax' []

traverseMax':: [String] -> BinTree String -> [String]
traverseMax' maxlist bintree
  | (nodes0 == 0) && (nodes1 == 0) = maxlist
  | (nodes0 == 0) && (nodes1 > 0) = traverseMax' (maxlist ++ ["1"]) bintree1
  | (nodes0 > 0) && (nodes1 == 0) = traverseMax' (maxlist ++ ["0"]) bintree0
  | nodes0 <= nodes1 = traverseMax' (maxlist ++ ["1"]) bintree1 
  | nodes0 > nodes1 = traverseMax' (maxlist ++ ["0"]) bintree0
  | otherwise = error "This won't happen???"
    where
      (nodes0, bintree0) = binTreeLookup "0" bintree
      (nodes1, bintree1) = binTreeLookup "1" bintree

traverseMin :: BinTree String -> [String]
traverseMin = traverseMin' []

traverseMin':: [String] -> BinTree String -> [String]
traverseMin' minlist bintree
  | (nodes0 == 0) && (nodes1 == 0) = minlist
  | (nodes0 == 0) && (nodes1 > 0) = traverseMin' (minlist ++ ["1"]) bintree1
  | (nodes0 > 0) && (nodes1 == 0) = traverseMin' (minlist ++ ["0"]) bintree0
  | nodes0 > nodes1 = traverseMin' (minlist ++ ["1"]) bintree1
  | nodes0 <= nodes1 = traverseMin' (minlist ++ ["0"]) bintree0
  | otherwise = error "This won't happen???"
    where
      (nodes0, bintree0) = binTreeLookup "0" bintree
      (nodes1, bintree1) = binTreeLookup "1" bintree

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0