import Prelude hiding (unwords, words)

pack :: [Char] -> [[Char]]
pack [] = []
pack (x:xs) = let (cs, css) = span (== x) (x:xs) 
              in cs : pack css

encode :: [Char] -> [(Int, Char)]
encode = map (\ cs -> (length cs, head cs)) . pack

decode :: [(Int, Char)] -> [Char]
decode = foldl (++) [] . map (\ (n, c) -> replicate n c)

rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate xs i = go (i `mod` length xs) xs
    where
        go 0 xs = xs
        go i (x:xs) = go (i-1) (xs ++ [x])

unwords :: [String] -> String
unwords [] = []
unwords (s:ss) = foldl (\ acc word -> acc ++ (' ' : word)) s ss

words :: String -> [String]
words [] = []
words cs = let (word, (' ':cs')) = span (/= ' ') cs
           in word : words cs'

-- Beispiel: max_length [[1,2,3], [4], [5,6]] = 3
-- max_length [] = undefined

max_length :: [[Int]] -> Int
max_length = maximum . map length

data Queue = Queue [Int] [Int]

empty :: Queue
empty = Queue [] []

proper :: Queue -> Queue
proper (Queue [] ys) = Queue (reverse ys) []
proper q = q

isEmpty :: Queue -> Bool
isEmpty (Queue [] _) = True
isEmpty _ = False

enqueue :: Int -> Queue -> Queue
enqueue i (Queue xs ys) = proper $ Queue [] [i]

first :: Queue -> Int
first (Queue (x:xs) _) = x

rest :: Queue -> Queue
rest (Queue (x:xs) ys) = proper $ Queue xs ys