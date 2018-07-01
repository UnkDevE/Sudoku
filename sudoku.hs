import qualified Data.Map as Map
import Data.Maybe

type Row = Map.Map Int Char
type Grid = Map.Map Int Row

makeEmptyGrid :: Int -> Grid
makeEmptyGrid size = 
    Map.fromList $ zip [1..size] 
        $ map Map.fromList $ take size $ repeat (zip [1..size] $ repeat '.')

makeEmptySudokuGrid :: Grid
makeEmptySudokuGrid = makeEmptyGrid 9

digits = ['1'..'9']

insertToGrid :: Int -> Int -> Char -> Grid -> Grid
insertToGrid row col digit grid 
    | elem digit digits = Map.insert row (Map.insert col digit $ fromJust $ Map.lookup row grid) grid
    | otherwise = error "not a digit"

flattenHelper :: [(Int, Map.Map Int a)] -> [(Int, a)] 
flattenHelper [x] = Map.toList(snd x)
flattenHelper (x:xs) = Map.toList (snd x) ++ flattenHelper xs

flatten :: Map.Map Int (Map.Map Int a) -> [a]
flatten map = snd $ unzip $ flattenHelper $ Map.toList map

get3by3 :: Int -> Int -> Grid -> String
