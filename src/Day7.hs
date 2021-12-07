module Day7 where

import Data.List (sort, sortBy, minimumBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

getInput = do
    input <- lines <$> readFile "input7.txt"
    pure $ concat $ map (read :: String -> Int) . splitOn "," <$> input 

move crabs target = map (abs . subtract target) crabs

moves crabs points cf = map (sum . map cf . move crabs) points

solve xs = moves xs [ head sorted .. last sorted ]
    where
        sorted = sort xs

day7_1 = do
    input <- getInput
    pure $ minimum (solve input id)

day7_2 = do
    input <- getInput
    let cost d = (fromIntegral d/2) * fromIntegral (1+d)
    pure $ minimum (solve input cost)
