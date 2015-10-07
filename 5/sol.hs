import qualified Data.IntMap as IntMap

-- Find an integer square root of an int.
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- Factor an int into a list of the factors.
fact :: Int -> [Int]
fact x = fact' x 2 (isqrt x)

-- Factor int helper function, will try to find factors up to a max number, then assume it is fully factored.
fact' :: Int -> Int -> Int -> [Int]
fact' x cur max
  | cur > max = [x] -- cur over max factor suggests that we can stop looking for factors.
  | (mod x cur) == 0 = [cur] ++ fact nextCur -- cur is a factor, append to list of factors and start factoring quotient.
  | otherwise = fact' x (cur+1) max -- otherwise try next factor.
  where nextCur = quot x cur

-- From an array of ints, find an int which is the smallest common multiple of all elements in array.
allFactors :: [Int] -> Int
allFactors xs = foldl (*) 1 $ decodeTable $ joinTables $ map (encodeTable . fact) xs


-- Turn a table which contains factor mapping to count into an array of the factors.
decodeTable :: IntMap.IntMap Integer -> [Int]
decodeTable table = foldl (\acc (key, val) -> acc ++ (take (fromIntegral val) $ repeat key)) [] $ IntMap.toList table

-- Built a map of factor to the number of times it appears.
-- IE: factors of 8: [2,2,2] => an IntMap of [(2,3)]
encodeTable :: [Int] -> IntMap.IntMap Integer
encodeTable xs = IntMap.fromListWith (+) $ map (\x -> (x, 1)) xs

-- Join factored IntMaps tables together, using the max of each value.
joinTables :: [IntMap.IntMap Integer] -> IntMap.IntMap Integer
joinTables = foldl (IntMap.unionWith max) IntMap.empty

main = print $ allFactors [1..20]
