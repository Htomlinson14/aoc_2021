import Data.Function (on)
import Data.List (lookup, sortBy)
import Data.Map (toList, fromListWith)
import Data.Maybe (fromJust)
import qualified Data.Text as T

main :: IO ()
main = do
    raw_contents <- readFile "input.txt"
    let contents = lines raw_contents
    let template = head contents
    let insertion_rules = map getRuleTuple ((tail . tail) contents)
    let update_10 = updateTemplateSteps 10 template insertion_rules
    let char_counts = sortBy (compare `on` snd) $ toList $ fromListWith (+) [(c, 1) | c <- update_10]
    print $ snd (last char_counts) - snd (head char_counts)

updateTemplateSteps:: Int -> String -> [(String, Char)] -> String
updateTemplateSteps 0 template _ = template
updateTemplateSteps n_steps template insertion_rules = 
    updateTemplateSteps (n_steps - 1) (updateTemplate template insertion_rules) insertion_rules

updateTemplate:: String -> [(String, Char)] -> String
updateTemplate template insertion_rules = 
    updateTemplate' template insertion_rules ""

updateTemplate':: String -> [(String, Char)] -> String -> String
updateTemplate' "" _ _ = error "Empty template"
updateTemplate' (x:xs) insertion_rules result
    | length xs == 1 = result ++ [x, char_to_insert, head xs]
    | otherwise = updateTemplate' xs insertion_rules (result ++ [x, char_to_insert])
    where char_to_insert = fromJust $ lookup [x, head xs] insertion_rules

getRuleTuple:: String -> (String, Char)
getRuleTuple x = let y = split "->" x in (head y, head (last y))

split :: String -> String -> [String]
split y x = map (T.unpack . T.strip) $ T.splitOn (T.pack y) (T.pack x)