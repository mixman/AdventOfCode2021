module Day11 where

import qualified Data.Vector as V
import Data.List (sort, sortBy, minimumBy, nub, elemIndex, sortOn, group, union, delete, reverse, (\\))
import Data.List.Split (splitOn)
import Data.Maybe

getInput = do
    input <- lines <$> readFile "input11.txt"
    let toDigits xs = map (read . (:"")) xs :: [Int]
    pure $ V.fromList $ map toDigits input

data Move = L | R | U | D | UR | DR | DL | UL
moves = [L,R,U,D,UR,DR,DL,UL]

steps vgrid grid = (numFlashes, g''')
    where
        -- update cells by +1
        g' = alterWith (\a y (i,v) -> a++[(i,v+1)]) grid
        -- radiate
        (flashed, g'') = updateGrid vgrid [] g'
        -- set each flashed cell to 0
        g''' = alterWith (\a y (i,v) -> if v>9 then a++[(i,0)] else a++[(i,v)]) g''
        numFlashes = sum $ map (\(y, xs) -> length xs) $ alterWith (\a y (i,v) -> if v>9 then a++[1] else a) g''

updateGrid vgrid flashed grid = if remainingFlashes fls
                                 then updateGrid vgrid flashed' grid'
                                 else (flashed, grid)
    where
        grid' = updateProximity ads grid
        fls = findFlashes grid flashed
        fls' = filter ((not . null) . snd) $ alterWith (\a y (i,v) -> a++[i]) fls
        -- (y,x) coordinates :mind-melt
        flsPs = concat $ map (\(y,row) -> map (\x -> (y,x)) row) fls'
        flsPs' = flsPs \\ flashed
        ads = flsPs' ++ adjacent fls vgrid []
        flashed' = flashed ++ rowCoords fls

rowCoords g = concat $ map (\(y,row) -> map (\(i,v) -> (y,i)) row) g

printGrid [] = do
    pure $ putStrLn ""
printGrid ((y,row):vs') = do
    putStrLn $ concat $ map(\(x,v) -> show v) row
    printGrid vs'

findFlashes grid flashed = alterWith (\a y (i,v) -> 
                            if v>9
                                then
                                  if (y,i) `elem` flashed
                                    then
                                        a
                                    else
                                        a++[(i,v)]
                                else a) grid
remainingFlashes pos = any (not . null) $ map snd pos

alterWith cf = foldl (\a (y, xs) -> a ++ [(y, alter cf y xs)]) []
alter cf y = foldl (\a pos@(i,v) -> cf a y pos) []

updateProximity updates grid = foldl (\a (y, row) -> a ++ [(y, updateS (updatesForRow y updates) row)]) [] grid
updatesForRow y updates = if null u' then [] else u'
    where
        u' = filter (\(y',x') -> y' == y) updates
updateS updates = foldl (\a' pos@(x,v) -> a' ++ [(x, updatesForPos pos updates)]) []
updatesForPos pos@(x,v) updates = v + length (filter (\(y',x') -> x'==x) updates)

adjacent [] vgrid rs             = rs
adjacent ((y, row):row') vgrid rs = adjacent row' vgrid (rs ++ go [] row)
    where
        go r []          = catMaybes r
        go r ((i,v):xxs) = go (r++[ validPos (move' m) | m <- moves]) xxs
            where
                move' cmd =
                  case cmd of
                    L  -> (Just y, hpos (i - 1))
                    R  -> (Just y, hpos (i + 1))
                    U  -> (vpos (y - 1), Just i)
                    D  -> (vpos (y + 1), Just i)
                    UR -> (vpos (y - 1), hpos (i + 1))
                    UL -> (vpos (y - 1), hpos (i - 1))
                    DL -> (vpos (y + 1), hpos (i - 1))
                    DR -> (vpos (y + 1), hpos (i + 1))

                validPos (Just a, Just b) = Just (a,b)
                validPos _                = Nothing

                hpos l
                    | l < 0                            = Nothing
                    | l > (length ((V.!) vgrid y) - 1) = Nothing
                    | otherwise                        = Just l
                vpos l
                    | l < 0                  = Nothing
                    | l > (length vgrid - 1) = Nothing
                    | otherwise              = Just l

solve vgrid sgrid n = go sgrid 0 n
    where
        go grid flashes n = if n>0
                        then go grid' (flashes+num) (n-1)
                        else (flashes, grid)
            where
                (num, grid') = steps vgrid grid

day11_1 = do
    input <- getInput
    let input' = [ (row, V.toList $ V.indexed $ V.fromList val) | (row, val) <- V.toList $ V.indexed input ]
    pure $ fst $ solve input input' 100

solve2 vgrid sgrid = go sgrid 0 0
    where
        go grid flashes n = if not allZero
                             then go grid' (flashes+num) (n+1)
                             else (flashes, grid, n)
            where
                (num, grid') = steps vgrid grid
                allZero = all (==0) $ concat $ map (\(y,row) -> map (\(i,v) -> v) row) grid

day11_2 = do
    input <- getInput
    let input' = [ (row, V.toList $ V.indexed $ V.fromList val) | (row, val) <- V.toList $ V.indexed input ]
    pure $ solve2 input input'
