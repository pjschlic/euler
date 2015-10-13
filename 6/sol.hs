
-- Sum square of all integers
sumSquares :: [Integer] -> Integer
sumSquares as = sum $ map (\a -> a*a) as

-- Square sum of all integers
squareSum :: [Integer] -> Integer
squareSum as = tot * tot
  where tot = sum as
