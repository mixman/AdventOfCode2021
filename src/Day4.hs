{-# LANGUAGE OverloadedStrings #-}
module Day4 where

import qualified Data.Text as T

gridInput xs r
    | length xs > 5 = gridInput (drop 6 xs) (parseGrid (drop 1 $ take 6 xs) r)
    | otherwise     = r

parseGrid xs r = r ++ [map lines xs]
    where
        lines x = map ((read :: String -> Int) . T.unpack) $ filter (not . T.null) $ T.splitOn " " x

getInput = do
    input <- T.lines . T.pack <$> readFile "input4.txt"
    let numbers = concat $ map ((read :: String -> Int) . T.unpack) . T.splitOn "," <$> [head input]
    let boards = gridInput (drop 1 input) []
    pure (numbers, boards)

hasBingo marks = all (`elem` marks)

markedRow marks = any (hasBingo marks)

markedCol marks board = any (hasBingo marks) checkCol
    where
        numCols = length (head board)
        splitter :: [[Int]] -> [Int] -> [[Int]]
        splitter r [] = r
        splitter r xs = splitter (r ++ [grid]) rest
            where
                (grid, rest) = splitAt numCols xs
        checkCol = splitter [] [ row !! col | col <- [0..numCols-1], row <- board ]

sumOfUnmarked marks board = sum [number | grid <- board, number <- grid, notElem number marks]

playTurn marks boards = [ (markedRow marks board, markedCol marks board, board, marks) | board <- boards ]

playBingo nums boards = concat [playTurn (take c nums) boards | c <- [5..length nums]]

day4_1 = do
    (nums, boards) <- getInput
    let (r,c,b,m) = head [ rs | rs@(r,c,b,m) <- playBingo nums boards, r || c]
    pure $ sumOfUnmarked m b * last m

day4_2 = do
    (nums, boards) <- getInput
    let history = [ (b, m) | rs@(r,c,b,m) <- playBingo nums boards, r || c]
    let winners xs = go xs [] []
            where
                go (win@(b,m):xs) r bs
                    | b `notElem` bs = go xs (r++[win]) (bs++[b])
                    | otherwise = go xs r bs
                go _ r bs = r
    let (board, marks) = last $ winners history
    pure $ sumOfUnmarked marks board * last marks
