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
import Debug.Trace
import Control.Monad (join)
import System.Random
import Control.Arrow

type Row = Map.Map Int Char
type Grid = Map.Map Int Row

makeEmptyGrid :: Int -> Grid
makeEmptyGrid size = 
    Map.fromList $ zip [1..size] 
        $ map Map.fromList $ take size $ repeat (zip [1..size] $ repeat '.')

makeEmptySudokuGrid :: Grid
makeEmptySudokuGrid = makeEmptyGrid 9

insertToGrid :: (Int, Int) -> Char -> Grid -> Grid
insertToGrid (row, col) char grid = 
    Map.insert row (Map.insert col char $ fromJust $ Map.lookup row grid) grid

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

getCharInGrid :: (Int, Int) -> Grid -> Char 
getCharInGrid pos grid = fromJust $ Map.lookup (snd pos) $ fromJust $ Map.lookup (fst pos) grid

isInGrid :: (Int, Int) -> Grid -> Bool
isInGrid pos grid = '.' /= getCharInGrid pos grid 

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

insertManyToGrid :: [((Int, Int), Char)] -> Grid -> Grid
insertManyToGrid xs grid = 
    foldl (\grid item -> insertToGrid (fst item) (snd item) grid) grid xs 

testGrid :: Grid
testGrid = insertManyToGrid 
    [
        ((1,2), '8'),
        ((1,5), '5'),
        ((1,6), '1'),
        ((1,9), '3'),
        ((2,3), '7'),
        ((2,5), '2'),
        ((2,7), '4'),
        ((2,8), '5'),
        ((3,1), '5'),
        ((3,7), '8'),
        ((3,8), '1'),
        ((4,3), '1'),
        ((4,4), '7'),
        ((4,6), '3'),
        ((4,7), '2'),
        ((4,8), '9'),
        ((4,9), '8'),
        ((5,3), '8'),
        ((5,7), '3'),
        ((6,1), '7'),
        ((6,2), '3'),
        ((6,3), '6'),
        ((6,4), '2'),
        ((6,6), '8'),
        ((6,7), '5'),
        ((7,2), '7'),
        ((7,3), '5'),
        ((7,9), '4'),
        ((8,2), '9'),
        ((8,3), '3'),
        ((8,5), '8'),
        ((8,7), '6'),
        ((9,1), '8'),
        ((9,4), '6'),
        ((9,5), '1'),
        ((9,8), '3')
    ] 
    $ makeEmptySudokuGrid

digits :: [Char]
digits = ['1'..'9']

solveProgDebug :: Grid -> Int -> Int -> Maybe Grid
solveProgDebug grid index digitIndex = trace 
    ("calling solveProg with g: " ++ show grid ++ "\n i: " ++ show index ++ "\n dI: " ++ show digitIndex )
    solveProg grid index digitIndex

solveProg :: Grid -> Int -> Int -> Maybe Grid
solveProg grid index digitIndex
    | digitIndex > 8 = Nothing
    | valid (digits!!digitIndex) pos grid && not (isInGrid pos grid) = 
        let solvedGrid = solveProgDebug (trace ("inserting pos: " ++ show pos ++ " digit: " ++ [digits!!digitIndex]) (insertToGrid pos (digits!!digitIndex) grid)) (index+1) (digitIndex) 
        in if solvedGrid /= Nothing then solvedGrid else solveProgDebug (insertToGrid pos (digits!!digitIndex) grid) (index+1) (digitIndex+1)
    | isInGrid pos grid && valid (getCharInGrid pos grid) pos grid = trace "valid and in grid" solveProgDebug grid (index+1) 0 
    | otherwise = trace ("g: " ++ show grid ++ "\npos: " ++ show pos ++ "\ndigitIndex: " ++ show digitIndex) Nothing
    where pos = (getGridPos 9 index)
     
solveP :: Grid -> Int -> Maybe Grid
solveP grid digitIndex  
    | digitIndex > 8 = trace ("digitIndex : " ++ show digitIndex) Just grid
    | otherwise = let solvedGrid = solveProgDebug grid 1 digitIndex
        in if solvedGrid /= Nothing then solvedGrid else solveP grid (digitIndex+1)  

solve :: Grid -> Maybe Grid
solve grid = solveP grid 0

getDigitInGrid :: Grid -> StdGen -> (Int, StdGen)
getDigitInGrid grid gen = first (abs . (\x y -> specialRem y x) (gridSize grid)) $ random gen 

fillSudokuGrid :: Grid -> StdGen -> Int -> Grid
fillSudokuGrid grid gen 0 = grid
fillSudokuGrid grid gen clues
    | valid digit (x, y) grid = fillSudokuGrid (insertToGrid (x, y) digit grid) gen'' (clues-1) 
    | otherwise = fillSudokuGrid grid gen' clues
    where (x, gen') = getDigitInGrid grid gen
          (y, gen'') = getDigitInGrid grid gen'
          digit = digits!!(abs $ rem (fst $ random gen'') $ gridSize grid)

printSudokuGrid :: Grid -> IO[()]
printSudokuGrid grid = do
    let rows = map flattenRow $ snd $ unzip $ Map.toList grid
    mapM putStrLn rows


