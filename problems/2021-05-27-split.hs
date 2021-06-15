module Main where

n = 20

grid :: [[(Int, Int)]]
grid = [ [(x, y) | x <- [0..n] ] | y <- [0..n] ]

type Cursor = (Int, Int)

-- | Returns a unique quadrant id for a 0-based @n * n@-size grid split into @s * s@ quadrants
-- which separated by lines of zeros. May be useful as the key function for group/groupSort/groupSortOn.
-- https://stackoverflow.com/questions/2745074/fast-ceiling-of-an-integer-division-in-c-c
quadrant' :: Int -> Int -> Cursor -> Int
quadrant' n' s (x', y') = (l *) . (1 +) $ x `div` q + wrap * (y `div` q)
  where
    (x, y, q, n) = (x' + 2 , y' + 2, n `div` s + 1, n' + 2)
    l = if x `mod` q < 2 || y `mod` q < 2 then 0 else 1
    wrap = (n + q) `div` q -- wrap x = ceiling (n / q)

-- | Returns a unique quadrant id for a 0-based n-size grid split into s*s quadrants
-- which separated by lines of zeros. May be useful as the key function for groupSortOn.
-- https://stackoverflow.com/questions/2745074/fast-ceiling-of-an-integer-division-in-c-c
quadrant :: Int -> Int -> Int -> Cursor -> Int
quadrant n' sx sy (x', y') = (`mod` 10) . (l *) . (1 +) $ x `div` qx + wrap * (y `div` qy)
  where
    (x, y, qx, qy, n) = (x' + 2 , y' + 2, n `div` sx + 1, n `div` sy + 1, n' + 2)
    l = if x `mod` qx < 2 || y `mod` qy < 2 then 0 else 1
    wrap = (n + qy) `div` qy -- wrap x = ceiling (n / q)

main = traverse putStrLn . map show . map (map (quadrant n 2 4)) $ grid

{-

runhaskell trash/2021-05-27-split.hs

[1,1,1,1,1,1,1,1,1,1,0,0,2,2,2,2,2,2,2,2,2]
[1,1,1,1,1,1,1,1,1,1,0,0,2,2,2,2,2,2,2,2,2]
[1,1,1,1,1,1,1,1,1,1,0,0,2,2,2,2,2,2,2,2,2]
[1,1,1,1,1,1,1,1,1,1,0,0,2,2,2,2,2,2,2,2,2]
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
[5,5,5,5,5,5,5,5,5,5,0,0,6,6,6,6,6,6,6,6,6]
[5,5,5,5,5,5,5,5,5,5,0,0,6,6,6,6,6,6,6,6,6]
[5,5,5,5,5,5,5,5,5,5,0,0,6,6,6,6,6,6,6,6,6]
[5,5,5,5,5,5,5,5,5,5,0,0,6,6,6,6,6,6,6,6,6]
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
[9,9,9,9,9,9,9,9,9,9,0,0,0,0,0,0,0,0,0,0,0]
[9,9,9,9,9,9,9,9,9,9,0,0,0,0,0,0,0,0,0,0,0]
[9,9,9,9,9,9,9,9,9,9,0,0,0,0,0,0,0,0,0,0,0]
[9,9,9,9,9,9,9,9,9,9,0,0,0,0,0,0,0,0,0,0,0]
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
[3,3,3,3,3,3,3,3,3,3,0,0,4,4,4,4,4,4,4,4,4]
[3,3,3,3,3,3,3,3,3,3,0,0,4,4,4,4,4,4,4,4,4]
[3,3,3,3,3,3,3,3,3,3,0,0,4,4,4,4,4,4,4,4,4]

-}
