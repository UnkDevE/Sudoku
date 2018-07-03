module Sudoku(
    Row(..),
    Grid(..),
    makeEmptySudokuGrid,
    valid
) where

import qualified Data.Map as Map
import Data.Bifunctor (bimap)
import Data.Maybe
import Data.Monoid
import Data.List.Split
import Data.List
import Control.Monad (join)

type Row = Map.Map Int Char
type Grid = Map.Map Int Row

makeEmptyGrid :: Int -> Grid
makeEmptyGrid size = 
    Map.fromList $ zip [1..size] 
        $ map Map.fromList $ take size $ repeat (zip [1..size] $ repeat '.')

makeEmptySudokuGrid :: Grid
makeEmptySudokuGrid = makeEmptyGrid 9

digits :: [Char]
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

flattenRow :: Map.Map Int a -> [a]
flattenRow map = snd $ unzip $ Map.toList map

gridSize :: Grid -> Int
gridSize grid = length $ Map.toList grid

get3by3Helper :: Int -> (Map.Map Int a) -> [a]
get3by3Helper row grid = snd $ unzip 
    $ filter (\a -> ((fst a)-1) `div` 3 == (row-1)) $ Map.toList grid

get3by3 :: (Int, Int) -> Grid -> String
get3by3 (row, col) grid = foldr (\x acc -> get3by3Helper col x ++ acc) [] $ get3by3Helper row grid

getRow :: Int -> Grid -> String
getRow row grid = flattenRow $ fromJust $ Map.lookup row grid 

getCol :: Int -> Grid -> String
getCol col grid = 
    foldr (\x acc -> if (fst x) `rem` 9 == col then (snd x):acc else acc) [] $ zip [1..] $ flatten grid 

find3by3 :: (Int, Int) -> (Int, Int)
find3by3 pos = join bimap (`rem` 3) pos 

isInGrid :: (Int, Int) -> Grid -> Bool
isInGrid pos grid =
    '.' /=  (fromJust $ Map.lookup (snd pos) $ fromJust $ Map.lookup (fst pos) grid)

specialRem :: Int -> Int -> Int 
specialRem index size
    | r == 0 = size
    | otherwise = r
    where r = index `rem` size 

getGridPos :: Int -> Int -> (Int, Int)
getGridPos size index = 
    (index `specialRem` size, ceiling((fromIntegral index)/(fromIntegral size)))

valid :: Char -> (Int, Int) -> Grid -> Bool
valid digit pos grid = 
    not $ getAny $ mconcat $ map (Any . (elem digit)) 
        [get3by3 (find3by3 pos) grid, getRow (fst pos) grid, getCol (snd pos) grid]  

unflatten :: Int -> String -> Grid
unflatten size flatgrid =
    Map.fromList $ zip [1..] $ map (Map.fromList . zip [1..]) $ chunksOf size flatgrid

instance Functor Grid where
    fmap f grid = unflatten (gridSize grid) $ map f $ flatten grid

printSudokuGrid :: Grid -> IO[()]
printSudokuGrid grid = do
    let rows = map flattenRow $ snd $ unzip $ Map.toList grid
    mapM putStrLn rows


