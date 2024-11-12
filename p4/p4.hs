reverseNum :: Int -> Int
reverseNum x = helper x 0
    where 
        helper 0 rev = rev
        helper n rev = helper (n `div` 10) (rev * 10 + n `mod` 10)

answer = maximum [x * y | x <- [100..999], y <- [100..999], x * y == reverseNum (x * y)]
