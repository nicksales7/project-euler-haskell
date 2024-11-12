genFib :: Int -> Int 
genFib 1 = 1
genFib 2 = 2
genFib n = genFib (n - 2) + genFib (n - 1)

fibList :: Int -> [Int]
fibList x = takeWhile (<= x) [genFib n | n <- [1..]]

answer = sum [y | y <- fibList 4000000, y `mod` 2 == 0]
