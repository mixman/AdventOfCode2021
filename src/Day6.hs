{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import qualified Data.Text as T (splitOn, pack, unpack, lines)
import qualified Data.Map.Strict as Map

getInput = do
    input <- T.lines . T.pack <$> readFile "input6.txt"
    pure $ concat $ map ((read :: String -> Int) . T.unpack) . T.splitOn "," <$> input

solve [] ys     = ys
solve (x:xs) ys = case x of
    0 -> solve xs (6:8:ys)
    _ -> solve xs ((x-1):ys)

day n t input
    | n == t    = input
    | otherwise = day (n+1) t (solve input [])

day6_1 = do length . day 0 80 <$> getInput

solve' [] ys           = ys
solve' m@((k,v):xs) ys = case k of
    0 -> solve' xs (Map.insertWith (+) 6 v $ Map.insertWith (+) 8 v ys)
    _ -> solve' xs (Map.insertWith (+) (k-1) v ys)

day' n t input
    | n == t    = input
    | otherwise = day' (n+1) t (solve' (reverse $ Map.assocs input) Map.empty)

day6_2 = do
    input <- getInput
    pure $ sum . Map.elems $ day' 0 256 (foldl (\y x -> Map.insertWith (+) x 1 y) Map.empty input)

