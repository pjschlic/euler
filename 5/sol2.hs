
divByAll :: [Int] -> Int -> Bool
divByAll ys x = foldl (\acc z -> acc && z) True $ map (isADiv x) ys

-- divByAll x ys = foldl (isADiv x) ys

isADiv :: Int -> (Int -> Bool)
isADiv x = (\y -> mod x y == 0)

firstDivByAll :: [Int] -> Int
firstDivByAll xs = head $ filter (divByAll xs) [1..]
