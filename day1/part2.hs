import Data.List (transpose)

main = do
    contents <- readFile "input.txt"
    let values = map read $ words contents :: [Int]
    print $ rollingIncrease values

rollingIncrease:: [Int] -> Int
rollingIncrease x
    | length x < 4 = error "List is too small"
    | otherwise = let
        a = init $ init x
        b = init $ drop 1 x
        c = drop 2 x
        in rollingIncrease' $ map sum $ transpose [a,b,c]

rollingIncrease':: [Int] -> Int 
rollingIncrease' [] = error "Empty list"
rollingIncrease' x =
    let a = init x
        b = tail x
        in sum $ zipWith (curry isincrease) a b

isincrease:: (Int, Int) -> Int
isincrease (x, y)
    | y > x = 1
    | otherwise = 0