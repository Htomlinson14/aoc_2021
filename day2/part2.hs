main = do
    contents <- readFile "input.txt"
    let instructions = map getInstruction $ lines contents
    print $ uncurry (*) $ steerSub instructions (0, 0) 0

getInstruction:: String -> (String, Int)
getInstruction x
    | length w == 2 = (head w, read $ last w)
    | otherwise = error "should just be two"
    where w = words x

steerSub:: [(String, Int)] -> (Int, Int) -> Int -> (Int, Int)
steerSub [] coords aim = coords
steerSub ((direction, amount):xs) (c1, c2) aim 
    | direction == "forward" = steerSub xs (c1 + amount, c2 + (amount * aim)) aim
    | direction == "up" = steerSub xs (c1, c2) (aim - amount)
    | direction == "down" = steerSub xs (c1, c2) (aim + amount)
    | otherwise = error "This won't happen"