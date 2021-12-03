module Day3 where

getInput = do
    input <- lines <$> readFile "input3.txt"
    let toDigits xs = map (read . (:"")) xs :: [Int]
    pure $ map toDigits input

total :: Int -> [[Int]] -> [Int] -> [Int]
total n [] r = r
total n (x:xs) r = total n xs (zipWith (\a b -> if b == n then a+1 else a) r x)

binToDecimal 0 = 0
binToDecimal i = 2 * binToDecimal (div i 10) + mod i 10

toDecimal = binToDecimal . asInt

asInt :: [Int] -> Int
asInt = read . concatMap show

day3_1 = do
    input <- getInput
    let rs = replicate (length $ head input) 0
    let countZeros = total 0 input rs
    let countOnes = total 1 input rs
    let gamma = zipWith (\a b -> if a>b then 0 else 1) countZeros countOnes
    let epsilon = zipWith (\a b -> if a<b then 0 else 1) countZeros countOnes
    pure $ toDecimal gamma * toDecimal epsilon

rating f [] c = error "dead end"
rating f xs c = if length xs == 1 then head xs
                else rating f (filter (\x -> x !! c == criteria !! c) xs) (c+1)
    where
        rs = replicate (length $ head xs) 0
        criteria = f (total 0 xs rs) (total 1 xs rs)

mostCommon = zipWith (\a b -> if a>b then 0 else 1)
leastCommon = zipWith (\a b -> if a>b then 1 else 0)

day3_2 = do
    input <- getInput
    pure $ toDecimal (rating mostCommon input 0) * toDecimal (rating leastCommon input 0)
