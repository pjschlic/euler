

createFilter :: Int -> ([Int] -> [Int])
createFilter x = filter (\y -> mod y x > 0)

getNthPrime :: Int -> Int
getNthPrime :: 

getNPrimes :: Int -> [Int]
getNPrimes x = take x $ genPrimes [2..]

-- genPrimes
-- take in current prime to filter out, and an array NOT YET filtering out that prime
-- then outputs an array which include
genPrimes :: [Int] -> [Int]
genPrimes stream = h : (genPrimes $ filterFirst stream)
  where h = head stream

filterFirst :: [Int] -> [Int]
filterFirst ys = (createFilter h) ys
  where h = head ys
