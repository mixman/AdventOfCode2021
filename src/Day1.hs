module Day1 where

getInput = do
    input <- lines <$> readFile "input1.txt"
    pure $ (read :: String -> Int) <$> input

day1_1 = do
    numbers <- getInput
    pure $ length $ filter (\x -> snd x > fst x) $ zip numbers (tail numbers)

sliding :: [Int] -> [[Int]] -> [[Int]]
sliding [] r = r
sliding xs r = sliding (tail xs) (take 3 xs:r)

day1_2 = do
    numbers <- getInput
    let numbers' = map sum $ reverse $ sliding numbers []
    pure $ length $ filter (\x -> snd x > fst x) $ zip numbers' (tail numbers')
