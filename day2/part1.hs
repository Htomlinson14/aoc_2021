import Data.Bifunctor (bimap)

main = do
    contents <- readFile "input.txt"
    let coords = map (convertCoords . getInstruction) $ lines contents
    print $ uncurry (*) $ foldl1 addCoords coords

getInstruction:: String -> (String, Int)
getInstruction x
    | length w == 2 = (head w, read $ last w)
    | otherwise = error "should just be two"
    where w = words x

convertCoords:: (String, Int) -> (Int, Int)
convertCoords (x, y)
    | x == "forward" = (y, 0)
    | x == "up" = (0, -y)
    | x == "down" = (0, y)
    | otherwise = (0, 0)

addCoords:: (Int, Int) -> (Int, Int) -> (Int, Int)
addCoords x = bimap (fst x +) (snd x +)