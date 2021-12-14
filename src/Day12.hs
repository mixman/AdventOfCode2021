{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import Data.List
import Data.Ord
import Data.List.Split
import Data.Char
import Debug.Trace (trace)
import Text.Pretty.Simple (pPrint)

getInput = do
    input <- lines <$> readFile "input12.txt"
    pure $ splitOn "-" <$> input

plotCave input = [ (node, paths node input) | node <- nub . concat $ input]
paths node input = if node == "end" then [] else left ++ right
    where
        left = map (!!1) $ filter (\x -> head x == node) input
        right = map (!!0) $ filter (\x -> last x == node) input
pathsFor s = snd . head . filter (\x -> fst x == s)
bigCave s = s /= map toLower s

solve cave = visit ["start"] []
    where
        visit [] visited        = error "wat"
        visit ("end":_) visited = [visited++["end"]]
        visit (node:_) visited  = concat [visit [n] visited' | n <- nextNodes]
            where
                visited' = visited ++ [node]
                nextNodes = pathsFor node cave \\ filter (not . bigCave) visited'

day12_1 = do
    input <- getInput
    pure $ solve (plotCave input)

solve2 cave = visit ["start"] []
    where
        visit [] visited        = error "wat"
        visit ("end":_) visited = [visited++["end"]]
        visit (node:_) visited  = concat [visit [n] visited' | n <- nextNodes]
            where
                visited' = visited ++ [node]
                condition = filter (\x -> length x == 2) . group . sort . filter (not . bigCave)
                nextNodes = if not . null $ condition visited'
                                then pathsFor node cave \\ ("start" : filter (not . bigCave) visited')
                                else pathsFor node cave \\ ["start"]

day12_2 = do
    input <- getInput
    pure $ solve2 (plotCave input)
