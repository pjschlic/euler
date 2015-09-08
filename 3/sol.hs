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

lpf = maximum . fact
