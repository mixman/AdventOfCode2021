{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Day13 (day13_1, day13_2) where

import Data.List
import Data.Ord
import Data.List.Split
import Data.Char
import Debug.Trace (trace)
import Text.Pretty.Simple (pPrint)

import Data.Maybe (fromJust)
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char
import Data.Void

data Axis = X | Y deriving Show
data Fold = Fold { axis :: Axis, pos :: Int } deriving Show
type Parser = Parsec Void String
newtype Dots = Dot { getDot :: (Int,Int) } deriving Show
data Paper = Paper { width :: Int, height :: Int, dots :: [Dots] } deriving Show

fold :: Parser Fold
fold = do
  string "fold along "
  axis <- some letterChar
  char '='
  num <- some digitChar
  eol
  return $ Fold (if axis == "x" then X else Y) (read num)

folds :: Parser [Fold]
folds = many fold

parseFolds = fromJust . parseMaybe folds

getInput = do
    input <- lines <$> readFile "input13.txt"
    let dots = sort . map ( map (read :: String -> Int)) . map (splitOn ",") . filter (',' `elem`) $ input
    let xmax = (+1) . maximum . map (!!0) $ dots
    let ymax = (+1) . maximum . map (!!1) $ dots
    let dots'' = [ Dot (x,y) | [x,y] <- dots ]
    let paper = Paper {width=xmax, height=ymax, dots=dots''}
    let folds = parseFolds (unlines $ filter ('=' `elem`) input)
    pure (paper, folds)

hasDot x y = any (\dot -> (x,y) == getDot dot)
printPaper paper = output [ if hasDot x y (dots paper) then "#" else "." | y <- [0..height paper-1], x <- [0..width paper-1] ] (width paper)
output lines n = if null r then concat l else concat l ++ "\n" ++ output r n
    where
        (l, r) = splitAt n lines

doFold (Fold X n) paper = paper { width=n, dots=dots' }
    where
        dots'= filter (\case (Dot (x,y)) -> x < n) $
                    map (\case dot@(Dot (x,y)) -> if x > n
                                                     then Dot (n-(x-n), y)
                                                     else dot) (dots paper)

doFold (Fold Y n) paper = paper { height=n, dots=dots' }
    where
        dots'= filter (\case (Dot (x,y)) -> y < n) $
                    map (\case dot@(Dot (x,y)) -> if y > n
                                                     then Dot (x, n-(y-n))
                                                     else dot) (dots paper)

day13_1 = do
    (paper, folds) <- getInput
    -- duplicate dots in this version TODO: nub Dots
    print (length . dots $ doFold (head folds) paper)
    putStrLn $ printPaper $ foldl (flip doFold) paper [head folds]

day13_2 = do
    (paper, folds) <- getInput
    putStrLn $ printPaper $ foldl (flip doFold) paper folds
