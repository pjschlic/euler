
isPal :: Int -> Bool
isPal = isPal' . show

isPal' :: String -> Bool
isPal' x
  | lenX <= 1  = True
  | otherwise = (head x == last x) && isPal' (take (lenX - 2) $ tail x)
  where lenX = length x

justPals :: [Int] -> [Int]
justPals = filter isPal

maxPals :: [Int] -> Int
maxPals = maximum . justPals

crossProdSeq :: [Int] -> [Int] -> [Int]
crossProdSeq xs ys 
  | length xs == 0 || length ys == 0 = []
	| otherwise = multAll (head ys) xs ++ crossProdSeq (tail ys) xs

multAll :: Int -> [Int] -> [Int]
multAll y xs = map (* y) xs
