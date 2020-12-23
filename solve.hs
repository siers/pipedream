module Main where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Tuple (swap)

type Pix = [Int]
type Maze = [[Pix]]

type Cursor = (Int, Int)
type Rotation = (Cursor, Int)

charMapEntries =
  [ ('╹', [0])
  , ('┗', [0,1])
  , ('┣', [0,1,2])
  , ('╋', [0,1,2,3])
  , ('┻', [0,1,3])
  , ('┃', [0,2])
  , ('┫', [0,2,3])
  , ('┛', [0,3])
  , ('╺', [1])
  , ('┏', [1,2])
  , ('┳', [1,2,3])
  , ('━', [1,3])
  , ('╻', [2])
  , ('┓', [2,3])
  , ('╸', [3])
  ]

charMap :: Map Char Pix
charMap = Map.fromList charMapEntries

mapChar :: Map Pix Char
mapChar = Map.fromList $ map swap charMapEntries

parse :: String -> Maze
parse =
  filter (not . null)
  . map (map (charMap !))
  . lines

render :: Maze -> String
render = unlines . map (map (mapChar !))

--

rotate :: Int -> Pix -> Pix
rotate n = map ((`mod` 4) . (+ 1))

rotateC, rotateCW :: Pix -> Pix
rotateC = rotate 1
rotateCW = rotate (-1)

-- [Cursor] should be a set
sqSolutions :: Maze -> [Cursor] -> Int -> Int -> [Rotation]
sqSolutions maze visited x y = []

solve :: [[Pix]] -> [[Pix]]
solve = id

main = getContents >>= putStrLn . render . solve . parse
-- main = getContents >>= putStrLn . show . map (length) . solve . parse
