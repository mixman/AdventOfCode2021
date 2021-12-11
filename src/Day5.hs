{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import qualified Data.Text as T (splitOn, pack, unpack, lines)
import qualified Data.Map as Map

getInput = do
    input <- T.lines . T.pack <$> readFile "input5.txt"
    let parse row = (map . map) ((read :: String -> Int) . T.unpack) $ map (T.splitOn ",") $ T.splitOn "->" row
    pure $ parse <$> input

coords x1 x2 y1 y2
 | x1 == x2  = zip (replicate (length (range y1 y2)) x1) (range y1 y2)
 | y1 == y2  = zip (range x1 x2) (take (length (range x1 x2)) $ repeat y1)
 | otherwise = zip (range x1 x2) (range y1 y2)

range a b
 | a > b     = [a,a-1..b]
 | otherwise = [a..b]

update rs (x,y) = Map.insertWith (+) (x,y) 1 rs

day5_1 = do
    input <- getInput
    let points = concat [ coords x1 x2 y1 y2 | [[x1,y1],[x2,y2]] <- input, x1==x2 || y1 == y2 ]
    pure $ length . filter (>1) . Map.elems $ foldl update Map.empty points

day5_2 = do
    input <- getInput
    let points = concat [ coords x1 x2 y1 y2 | [[x1,y1],[x2,y2]] <- input ]
    pure $ length . filter (>1) . Map.elems $ foldl update Map.empty points
