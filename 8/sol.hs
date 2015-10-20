takeStep :: Int -> [a] -> [a]
takeStep _ [] = []
takeStep n (x:xs) = x : takeStep n (drop (n-1) xs)

slice :: Int -> [a] -> [a]
slice [] = [[]]
slice (x:xs) = [x:(take 7 xs)] ++ slice xs

-- from large list, produce sets which are each N long
getBestProd :: Int -> [Int] -> Int
getBestProd as = bestProduct $ slice as


bestProduct :: [[Int]] -> Int
bestProduct as = max $ map productSet as


productSet :: [Int] -> Int
productSet as = foldl (*) as
