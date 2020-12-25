module Main where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List (sort, isSubsequenceOf)
import Data.Map (Map, (!))
import Data.Matrix as Mx (ncols, nrows)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Debug.Trace
import qualified Data.HashMap.Strict as HS
import qualified Data.Map as Map
import qualified Data.Matrix as Mx

t a = trace (show a) a

type Direction = Int -- top 0, right 1, bottom 2, left 3
type Pix = [Direction]
type Maze = Mx.Matrix Char

type Cursor = (Int, Int)
type CursorRot = (Int, Int, Int)
type Rotation = Int

type PixCheck = (Char, Char, Char, Char, Char, Rotation) -- 0,1,2,3 – direction + 4 – current
type PixValidPrecomp = HashMap PixCheck Bool
type RotatePrecomp = HashMap (Char, Rotation) Char

(#!) :: (Eq k, Hashable k) => HashMap k v -> k -> v
(#!) = (HS.!)

directions = [0, 1, 2, 3]
rotations = directions -- nu tā sanāk

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

charsSpecial =
  [ ' ' -- wall
  , 'u' -- unsolved
  ]

(chars, pixs) = unzip charMapEntries
charsWithSpecial = chars ++ charsSpecial

charMap :: Map Char Pix
charMap = Map.fromList charMapEntries

pixMap :: Map Pix Char
pixMap = Map.fromList $ map swap charMapEntries

mapChar = (charMap !)
mapPix = (pixMap !) . sort

parse :: String -> Maze
parse =
  Mx.fromLists
  . filter (not . null)
  . lines

render :: Maze -> String
render = unlines . Mx.toLists

-- C: n=1, CW: n=-1
rotateDir :: Int -> Direction -> Direction
rotateDir n = (`mod` 4) . (+ n)

flipDir = rotateDir 2

rotate :: Rotation -> Pix -> Pix
rotate n = map (rotateDir n)

opposite = 2

verifyPixelModel = (pixs ==) . last $
  [ map (mapChar . mapPix . rotate 1) pixs
  , map (mapChar . mapPix . rotate 1 . rotate 1) pixs
  , map (mapChar . mapPix . rotate 1 . rotate 1 . rotate 1) pixs
  , map (mapChar . mapPix . rotate 1 . rotate 1 . rotate 1 . rotate 1) pixs
  ]

cursorNext :: Cursor -> Direction -> Cursor
cursorNext (x, y) 0 = (y - 1, x)
cursorNext (x, y) 1 = (y, x + 1)
cursorNext (x, y) 2 = (y + 1, x)
cursorNext (x, y) 3 = (y, x - 1)

implementRotate :: RotatePrecomp -> Cursor -> Rotation -> Maze -> Maze
implementRotate rotP cur@(x, y) rot maze = Mx.setElem rotated (y, x) maze
  where rotated = rotP #! (Mx.getElem y x maze, rot)

--

pixValid :: PixCheck -> Bool
pixValid (d0, d1, d2, d3, this, rot) =
  all validateDirection directions
  where
    validateDirection d = filter (flipDir d ==) thisRequires == filter (flipDir d ==) thatRequires
      where
        that :: Char
        that = [d0, d1, d2, d3] !! d

        thisRequires :: Pix
        thisRequires =
          if that == 'u'
          then []
          else (rot + opposite) `rotate` mapChar this

        thatRequires :: Pix
        thatRequires =
          if that == 'u' || that == ' '
          then []
          else mapChar that

-- given top and left pix is solved, verify this pix is valid after rotation
mazePixValid :: PixValidPrecomp -> Maze -> Cursor -> Char -> Rotation -> Bool
mazePixValid pixValidP maze cur@(x, y) this rotation =
  check (charN 0, charN 1, charN 2, charN 3, this, rotation)
  where
    check =
      if usePixValidPrecomputed
      then (pixValidP #!)
      else pixValid

    charN d =
      if directionUncertain
      then 'u'
      else ' ' `fromMaybe` uncurry Mx.safeGet (cursorNext cur d) maze
      where
        directionUncertain = (d == 1 && x < ncols maze) || (d == 2 && y < nrows maze)

solve :: PixValidPrecomp -> RotatePrecomp -> Maze -> (Maze, [CursorRot])
solve pixValidP rotP = last . take 4 . solve_ (1, 1) []
  where
    solve_ :: Cursor -> [(Int, Int, Int)] -> Maze -> [(Maze, [CursorRot])]
    solve_ cur@(x, y) path maze = do
      this <- pure $ Mx.getElem y x maze
      let rotations = mazePixValid pixValidP maze cur this `filter` chooseRotation this
      let canUseMutation = length rotations == 1
      rotation <- rotations

      (if x == ncols maze && y == nrows maze
      then [(nextMaze rotation, nextPath rotation)]
      else solve_ (nextCur cur maze) (nextPath rotation) (traceBoard $ nextMaze rotation))

      where
        nextMaze rot = implementRotate rotP cur rot maze
        nextPath rot = (x, y, rot) : path

    chooseRotation '╋' = [0]
    chooseRotation '┃' = [0,1]
    chooseRotation '━' = [0,1]
    chooseRotation _ = rotations

    traceBoard board =
      if 't' == 'f'
      then board
      else trace ("\x1b[H\x1b[2J" ++ (render board)) board

    nextCur (x, y) maze = (x_, y_)
      where
        jump = x == 1 || y == nrows maze
        nthLine = x + y - 1
        x_overflow = ((nthLine + 1) `max` ncols maze) - ncols maze
        x_ = if jump then (nthLine + 1) `min` ncols maze else x - 1
        y_ = if jump then 1 + x_overflow else y + 1

--

printRot :: [CursorRot] -> String
printRot =
  unlines
  . map (\(x, y) -> "rotate " ++ show (x - 1) ++ " " ++ show (y - 1))
  . (>>= (\(x, y, r) -> take r (repeat (x, y))))
  . reverse

rotatePrecomputed :: RotatePrecomp
rotatePrecomputed = HS.fromList $ list (\(c, r) -> mapPix . rotate r . mapChar $ c)
  where
    list f = (f >>= flip (,)) `map` all
    all = do
      p1 <- chars
      r <- rotations
      pure (p1, r)

pixValidPrecomputed :: PixValidPrecomp
pixValidPrecomputed = HS.fromList list
  where
    list = (pixValid >>= flip (,)) `map` all
    all = do
      p1 <- charsWithSpecial
      p2 <- charsWithSpecial
      p3 <- charsWithSpecial
      p4 <- charsWithSpecial
      p5 <- chars
      r <- rotations
      pure (p1, p2, p3, p4, p5, r)

usePixValidPrecomputed = False

main = do
  pure verifyPixelModel

  pixValidPrecomp <- pure pixValidPrecomputed
  rotatePrecomp <- pure rotatePrecomputed

  input <- getContents
  -- putStrLn "input"
  -- putStrLn . render . parse $ input
  -- putStrLn "solve"
  (solved, rotations) <- pure $ solve pixValidPrecomp rotatePrecomp . parse $ input
  -- putStrLn . printRot $ rotations
  putStrLn . render $ solved
