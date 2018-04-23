import Prelude hiding (rem)

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

sumFacs :: Int -> Int -> Int
sumFacs n m
    | n == m = 0
    | otherwise = fac n + sumFacs (n+1) m

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Int -> Int
fib' n = fibs' !! n

prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

prod' :: [Int] -> Int
prod' = prodWithAcc 1
    where
        prodWithAcc :: Int -> [Int] -> Int
        prodWithAcc acc [] = acc
        prodWithAcc acc (x:xs) = prodWithAcc (x*acc) xs

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rev' :: [Int] -> [Int]
rev' = revWithAcc []
    where
        revWithAcc acc [] = acc
        revWithAcc acc (x:xs) = revWithAcc (x:acc) xs

rem :: Int -> [Int] -> [Int]
rem _ [] = []
rem 0 (x:xs) = xs
rem n (x:xs) = x : rem (n-1) xs

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x] = True
isOrd (x:(x':xs))
    | x < x' = isOrd (x':xs)
    | otherwise = False

merge :: [Int] -> [Int] -> [Int]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y: merge (x:xs) ys

fibs :: [Int]
fibs = map fib [0..]

fibs' :: [Int]
fibs' = go 0 1
    where
        go n m = n : go m (n+m)

trees :: Int -> Int
trees 1 = 1
trees n = sum [ trees i * trees j | (i, j) <- zip [1..(n-1)] [(n-1),(n-2)..1] ]

catalan n = go 1 n
    where
        go _ 0 = 0 
        go n m = catalan n * catalan m + go (n+1) (m-1)