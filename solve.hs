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
render = unlines . map (map ((mapChar !) . sort)) . Mx.toLists

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
rotate n = map (rotateDir n)

opposite = 2

implementRotate :: Cursor -> Rotation -> Maze -> Maze
implementRotate cur@(x, y) rot maze = Mx.setElem (rotate rot $ Mx.getElem y x maze) (y, x) maze

-- given top and left pix is solved, verify this pix is valid after rotation
pixValid :: Maze -> Cursor -> Rotation -> Bool
pixValid maze cur@(x, y) rot = all validateDirection directions
  where
    validateDirection d = filter (flipDir d ==) thisRequires == filter (flipDir d ==) thatRequires
      where
        -- square in that direction does not have its final value yet
        directionUncertain = (d == 1 && x < ncols maze) || (d == 2 && y < nrows maze)

        thisRequires :: Pix
        thisRequires =
          if directionUncertain
          then []
          else (rot + opposite) `rotate` (Mx.getElem y x maze)

        thatRequires :: Pix
        thatRequires =
          if directionUncertain
          then []
          else [] `fromMaybe` uncurry Mx.safeGet (curN d) maze
          where
            curN :: Direction -> Cursor
            curN 0 = (y - 1, x)
            curN 1 = (y, x + 1)
            curN 2 = (y + 1, x)
            curN 3 = (y, x - 1)

solve :: Maze -> (Maze, [(Int, Int)])
solve input =
  (maze, reverse rotations >>= (\(x, y, r) -> take r (repeat (x, y))))
  where
    (maze, rotations) = head $ solve_ (1, 1) [] input

    solve_ :: Cursor -> [(Int, Int, Int)] -> Maze -> [(Maze, [(Int, Int, Int)])]
    solve_ cur@(x, y) path maze = do
      rotation <- directions

      if pixValid maze cur rotation
      -- if trace (show (x, y, rotation, nextCur cur maze)) pixValid maze cur rotation
      then
        (if x == ncols maze && y == nrows maze
        then [(nextMaze rotation, nextPath rotation)]
        else solve_ (nextCur cur maze) (nextPath rotation) (nextMaze rotation))
        -- else solve_ (nextCur cur maze) ((x,y,rotation) : path) (trace ("\x1b[H\x1b[2J" ++ (render (nextMaze rotation))) nextMaze rotation))
      else []

      where
        nextMaze rot = implementRotate cur rot maze
        nextPath rot = (x, y, rot) : path

    nextCur (x, y) maze = (x_, y_)
      where
        jump = x == 1 || y == nrows maze
        nthLine = x + y - 1
        x_overflow = ((nthLine + 1) `max` ncols maze) - ncols maze
        x_ = if jump then (nthLine + 1) `min` ncols maze else x - 1
        y_ = if jump then 1 + x_overflow else y + 1

printRot :: [Cursor] -> String
printRot = unlines . map (\(x, y) -> "rotate " ++ show (x - 1) ++ " " ++ show (y - 1))

main = do
  pure verifyPixelModel
  input <- getContents
  -- putStrLn "input"
  -- putStrLn . render . parse $ input
  -- putStrLn "solve"
  (solved, rotations) <- pure $ solve . parse $ input
  putStrLn . printRot $ rotations
  -- putStrLn . render $ solved
