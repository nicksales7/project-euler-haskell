isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | n == 2    = True 
    | n `mod` 2 == 0 = False
    | otherwise = not (any (\x -> n `mod` x == 0) [3,5..floor (sqrt (fromIntegral n))]) -- fromIntegral converts n from an int to a float apparently xd

largestPrime :: Int -> Int
largestPrime x = maximum [y | y <- [3,5..floor (sqrt (fromIntegral x))], isPrime y && x `mod` y == 0]
