import Prelude hiding (foldl)

data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving Show

-- Beispiel:
-- Branch 0 (Branch 1 (Branch 8 (Leaf 10) (Leaf 12)) (Leaf 3)) (Branch 3 (Leaf 6) (Leaf 4))

depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch _ t1 t2) = min (depth t1) (depth t2)

paths :: Tree a -> Tree [a]
paths = go []
    where
        go path (Leaf x) = Leaf (reverse (x:path))
        go path (Branch x t1 t2) = let path' = x:path
                                   in Branch (reverse path') (go path' t1) (go path' t2)

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf a) = Leaf (f a)
tmap f (Branch a t1 t2) = Branch (f a) (tmap f t1) (tmap f t2)

f :: [Int] -> Int
f = foldr (*) 1 . map (^ 2) . filter ((== 0) . (`mod` 2))

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a [] = a
foldl f a (b:bs) = foldl f (f a b) bs

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl f a = foldr (flip f) a . reverse