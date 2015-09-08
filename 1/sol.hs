-- isMult :: Num -> Num -> Bool
isMult x y = (mod y x) == 0

-- isMult5 :: Num -> Bool
isMult5 = isMult 5
isMult3 = isMult 3

-- isMult5or3 :: Num -> Bool
isMult5or3 x = (isMult5 x) || (isMult3 x)

main = print $ sum $ filter isMult5or3 [1..999]
