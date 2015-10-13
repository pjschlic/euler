takeStep :: Int -> [a] -> [a]
takeStep _ [] = []
takeStep n (x:xs) = x : takeStep n (drop (n-1) xs)

slice :: Int -> Int -> [a] -> [a]
slice start stop = takeStep 1 . take (stop - start) . drop start

-- from large list, produce sets which are each N long
getBestProd :: Int -> [Int] -> Int
getBestProd n as = bestProduct $ [slice start (start+n) as | <- ]


bestProduct :: [[Int]] -> Int
bestProduct as = max $ map productSet as


productSet :: [Int] -> Int
productSet as = foldl (*) as
