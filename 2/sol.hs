

-- Get fib numbers less than N
fib :: Num -> [Num]
fib max = fibRest 0 1 max

-- fibRest :: Num -> Num -> Num -> [Num]
fibRest last cur max
  | nextCur >= max = []
	| otherwise = [nextCur] ++ (fibRest cur nextCur max)
		where nextCur = (last + cur)

-- main = print $ sum $ filter even $ fib 10
main = print $ sum $ filter even $ fib (4*10^6)
