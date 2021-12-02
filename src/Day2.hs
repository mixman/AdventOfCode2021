module Day2 where

getInput = do
    input <- lines <$> readFile "input2.txt"
    let conv x = (head x, read (last x) :: Int)
    pure $ map (conv . words) input

navigate :: [(String, Int)] -> Int -> Int -> (Int, Int)
navigate (("forward", move):xs) hPos vPos = navigate xs (hPos+move) vPos
navigate (("down", move):xs) hPos vPos = navigate xs hPos (vPos+move)
navigate (("up", move):xs) hPos vPos = navigate xs hPos (vPos-move)
navigate _ hPos vPos = (hPos, vPos)

day2_1 = do
    input <- getInput
    pure $ uncurry (*) $ navigate input 0 0

navigateAim :: [(String, Int)] -> Int -> Int -> Int -> (Int, Int)
navigateAim (("forward", move):xs) hPos vPos aim = navigateAim xs (hPos+move) (vPos+(aim*move)) aim
navigateAim (("down", move):xs) hPos vPos aim = navigateAim xs hPos vPos (aim+move)
navigateAim (("up", move):xs) hPos vPos aim = navigateAim xs hPos vPos (aim-move)
navigateAim _ hPos vPos aim = (hPos, vPos)

day2_2 = do
    input <- getInput
    pure $ uncurry (*) $ navigateAim input 0 0 0
