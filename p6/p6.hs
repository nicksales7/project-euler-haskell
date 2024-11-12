squareSum :: Int -> Int
squareSum x = ((x * (x + 1)) `div` 2)^2

sumSquares :: Int -> Int
sumSquares y = (y * (y + 1) * (2 * y + 1)) `div` 6

answer = squareSum 100 - sumSquares 100
