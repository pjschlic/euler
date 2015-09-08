import Data.HashTable (HashTable)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral


fact :: Int -> [Int]
fact x = fact' x 2 (isqrt x)

fact' :: Int -> Int -> Int -> [Int]
fact' x cur max
  | cur > max = [x]
	| (mod x cur) == 0 = [cur] ++ fact nextCur
	| otherwise = fact' x (cur+1) max
	where nextCur = quot x cur

allFactors :: [[Int]] -> [Int]
allFactors xs = decodeTable $

decodeTable :: HashTable Int Int -> [Int]
decodeTable table = foldl accumKeyVal [] $ HashTable.toList table

accumKeyVal :: [Int] -> (Int Int) -> [Int]
accumKeyVal acc (key val) = acc ++ take val $ repeat key

encodeTable :: [Int] -> HashTable Int Int
encodeTable xs = 

unions :: [Set Int] -> Set Int
unions xs = foldl (\acc x -> Set.union acc (Set.difference acc x)) xs
