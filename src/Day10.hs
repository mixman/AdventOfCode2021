module Day10 where

import Data.List
import Data.Maybe
import Data.List.Split (splitOn)
import Debug.Trace (trace)

getInput = lines <$> readFile "input10.txt"

opening = "({<["
closing = ")}>]"

isMatching open close = close == openClosing open
openClosing open = closing !! fromJust (open `elemIndex` opening)

data Result a = Incomplete a | Error a | OK | Empty deriving (Show)

solve xs = check xs Empty []
    where
        check [] err stack
            | (not . null) stack = Incomplete stack
            | otherwise          = err
        check (x:xs) err stack = case err' of
                                    OK -> check xs err' stack'
                                    _  -> check "" err' stack'
            where
                (err', stack') = determine x stack
        determine x stack = if x `elem` opening
                                then (OK, stack++[x])
                                else pop x stack
        pop x stack = if (not . null) stack && isMatching open x
                        then (OK, init stack)
                        else (Error msg, "")
            where
                open = last stack
                msg = if null stack
                        then "Empty stack while matching " ++ show x
                        else openClosing open : "," ++ [x]

errs (Error a) = last $ splitOn "," a
errs _ = ""

points x = case x of
            ")" -> 3
            "]" -> 57
            "}" -> 1197
            ">" -> 25137
            _   -> 0

day10_1 = do sum . map ((points . errs) . solve) <$> getInput

incs (Incomplete a) = Just a
incs _              = Nothing

points2 x = case x of
            ')' -> 1 
            ']' -> 2
            '}' -> 3
            '>' -> 4
            _   -> 0

calc :: Int -> Char -> Int
calc score x = 5*score + points2 x

total [] score     = score
total (x:xs) score = total xs (calc score x)

day10_2 = do
    input <- getInput
    let rs = map solve input
    let completions = map (map openClosing . reverse) $ mapMaybe incs rs
    let scores = sort $ map (`total` 0) completions
    let idx scores = ceiling $ fromIntegral (length scores) / 2
    pure $ scores !! subtract 1 (idx scores)
