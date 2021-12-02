
main = do
    contents <- readFile "input.txt"
    let values = map read $ words contents :: [Int]
    let x = init values
        y = tail values
        in print $ sum $ zipWith (curry isincrease) x y

isincrease:: (Int, Int) -> Int
isincrease (x, y)
    | y > x = 1
    | otherwise = 0