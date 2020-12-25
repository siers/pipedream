module Main where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List (sort, isSubsequenceOf, elemIndex, uncons, concat)
import Data.Map (Map, (!))
import Data.Matrix as Mx (Matrix, ncols, nrows)
import Data.Maybe (fromMaybe, fromJust, maybeToList)
import Data.Set (Set)
import Data.Tuple (swap)
import Debug.Trace
import qualified Data.HashMap.Strict as HS
import qualified Data.Map as Map
import qualified Data.Matrix as Mx
import qualified Data.Set as Set
import Text.Printf

t a = trace (show a) a

type Direction = Int -- top 0, right 1, bottom 2, left 3
type Pix = [Direction]
type Maze = Matrix Char

type Cursor = (Int, Int)
type CursorRot = (Int, Int, Int)
type Rotation = Int

type PixCheck = (Char, Char, Char, Char, Char, Rotation) -- 0,1,2,3 – direction + 4 – current
type PixValidPrecomp = HashMap PixCheck Bool
type RotatePrecomp = HashMap (Char, Rotation) Char

(#!) :: (Eq k, Hashable k) => HashMap k v -> k -> v
(#!) = (HS.!)

setHead :: Ord a => Set a -> Maybe (a, Set a)
setHead set = (\head -> (head, head `Set.delete` set)) <$> head'
  where head' = fst <$> uncons (Set.toList set)

nsize :: Matrix a -> Int
nsize m = nrows m * ncols m

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

renderWithPos :: Maze -> Cursor -> String
renderWithPos maze target =
  unlines
  . map concat
  . Mx.toLists
  . Mx.mapPos (\cur c -> printf (if target == cur then "\x1b[31m%s\x1b[39m" else "%s") (c : []))
  $ maze


-- C: n=1, CW: n=-1
rotateDir :: Int -> Direction -> Direction
rotateDir n = (`mod` 4) . (+ n)

flipDir = rotateDir 2

rotate :: Rotation -> Pix -> Pix
rotate r = map (rotateDir r)

rotateChar :: Rotation -> Char -> Char
rotateChar r = mapPix . rotate r .mapChar

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

solve :: PixValidPrecomp -> RotatePrecomp -> Maze -> [Maze]
solve pixValidP rotP = take 1 . solve_ (1, 1) Set.empty Set.empty
  where
    solve_ :: Cursor -> Set Cursor -> Set Cursor -> Maze -> [Maze]
    solve_ cur@(x, y) solved continue maze = do
      this <- pure $ Mx.getElem y x maze
      rotation <- mazePixValid pixValidP maze cur this `filter` chooseRotation this
      -- canUseMutation = length rotations == 1
      solveRotation rotation (rotP #! (Mx.getElem y x maze, rotation))

      where
        chooseRotation '╋' = [0]
        chooseRotation '┃' = [0,1]
        chooseRotation '━' = [0,1]
        chooseRotation _ = rotations

        solveRotation :: Rotation -> Char -> [Maze]
        solveRotation rotation rotated =
          if Set.size solved == nsize maze
          then [nextMaze]
          else do
            (nextCursor, nextContinue) <- maybeToList $ setHead nextContinue
            solve_ nextCursor nextSolved nextContinue (traceBoard nextMaze)

          where
            nextCursors = Set.fromList $ (cursorNext cur <$> mapChar rotated)
            nextSolved = cur `Set.insert` solved
            nextContinue = (nextCursors `Set.union` continue) `Set.difference` solved
            nextMaze = Mx.setElem rotated (y, x) maze

            traceBoard board =
              if 't' == 'f'
              then board
              else trace (show cur ++ "\n" ++ renderWithPos board cur) board
              -- else trace ("\x1b[H\x1b[2J" ++ (render board)) board

--

printRot :: [CursorRot] -> String
printRot =
  unlines
  . map (\(x, y) -> "rotate " ++ show (x - 1) ++ " " ++ show (y - 1))
  . (>>= (\(x, y, r) -> take r (repeat (x, y))))
  . reverse

computeRotations :: Maze -> Maze -> [CursorRot]
computeRotations input solved = Mx.toList . Mx.matrix (nrows input) (ncols input) $ cursorRot
  where
    cursorRot (y, x) = (x, y, get input `rotations` get solved)
      where
        get = Mx.getElem y x
        rotations from to = fromJust $ to `elemIndex` iterate (rotateChar 1) from

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

  input <- parse <$> getContents
  solveds <- pure . solve pixValidPrecomp rotatePrecomp $ input

  mapM_ (putStrLn . printRot . computeRotations input) $ solveds
  mapM_ (putStrLn . render) $ solveds
