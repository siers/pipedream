module Main where

import Data.List (sort, isSubsequenceOf)
import Data.Map (Map, (!))
import Data.Matrix as Mx (ncols, nrows)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Matrix as Mx

t a = trace (show a) a

type Direction = Int -- top 0, right 1, bottom 2, left 3
type Pix = [Direction]
type Maze = Mx.Matrix Pix

type Cursor = (Int, Int)
type Rotation = Int

directions = [0, 1, 2, 3]

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
  Mx.fromLists
  . filter (not . null)
  . map (map (charMap !))
  . lines

render :: Maze -> String
render = unlines . map (map (mapChar !)) . Mx.toLists

verifyPixelModel = (pixs ==) . last $
  [ map (idLookup . rotateC) pixs
  , map (idLookup . rotateC . rotateC) pixs
  , map (idLookup . rotateC . rotateC . rotateC) pixs
  , map (idLookup . rotateC . rotateC . rotateC . rotateC) pixs
  ]
  where
    idLookup = (charMap !) . (mapChar !)
    pixs = map snd charMapEntries
    rotateC = rotate 1

--

-- C: n=1, CW: n=-1
rotateDir :: Int -> Direction -> Direction
rotateDir n = (`mod` 4) . (+ n)

flipDir = rotateDir 2

rotate :: Rotation -> Pix -> Pix
rotate n = sort . map (rotateDir n)

opposite = 2

implementRotate :: Cursor -> Rotation -> Maze -> Maze
implementRotate cur@(x, y) rot maze = Mx.setElem (rotate rot $ Mx.getElem x y maze) cur maze

-- [Cursor] should be a set
-- pixSolutions :: Maze -> -> Int -> Int -> [Rotation]
-- pixSolutions maze visited x y = []

-- given top and left pix is solved, verify this pix is valid after rotation
pixValid :: Maze -> Cursor -> Rotation -> Bool
pixValid maze cur@(x, y) rot = flip all directions $ \d ->
    trace (show (d, "//", filterD d thisRequires, "==", filterD d (thatHas d))) $
    filterD d thisRequires == filterD d (thatHas d)
  where
    filterD d = filter (flipDir d ==)
    thisRequires = (rot + opposite) `rotate` (Mx.getElem x y maze)

    curN :: Direction -> Cursor
    curN 0 = (x, y - 1)
    curN 1 = (x + 1, y)
    curN 2 = (x, y + 1)
    curN 3 = (x - 1, y)

    thatHas :: Direction -> Pix
    thatHas n =
      if (n == 1 || n == 2) && (x <= ncols maze || y <= Mx.nrows maze)
      then directions
      else [] `fromMaybe` uncurry Mx.safeGet (curN n) maze

    default_ :: Cursor -> Pix
    default_ (x, y) =
      if (x `min` y < 1) || x > ncols maze || y > Mx.nrows maze
      then []
      else directions

solve :: Maze -> [Maze]
solve input = do
  take 2 $ solve_ (1, 1) input
  where
    solve_ :: Cursor -> Maze -> [Maze]
    solve_ cur@(x, y) maze = do
      rotation <- directions

      if trace (show $ (x, y, rotation)) pixValid maze cur rotation
      then
        (if x == ncols maze && y == nrows maze
        then [nextMaze rotation]
        else nextCur cur maze >>= \cur -> solve_ cur (nextMaze rotation))
      else []

      where
        nextMaze rot = implementRotate cur rot maze

    nextCur (x, y) maze =
      [((x `mod` ncols maze) + 1, y + (if x == ncols maze then 1 else 0))]

-- Varbūt noderēs
-- solve = id

probe maze = pixValid maze (1, 1) 3

change maze = Mx.setElem (rotate (-1) $ Mx.getElem 1 1 maze) (1, 1) maze

main = do
  pure verifyPixelModel
  input <- getContents
  putStrLn "input"
  putStrLn . render . parse $ input
  putStrLn "solve"
  mapM_ putStrLn . map render . solve . parse $ input
