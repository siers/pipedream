module Main where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List (sort, isSubsequenceOf, elemIndex, uncons, concat, find)
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

type PixCheck = (Char, Char, Rotation, Direction)
type PixValidPrecomp = HashMap PixCheck Bool
type RotatePrecomp = HashMap (Char, Rotation) Char

type CursorSet = Set Cursor

(#!) :: (Eq k, Hashable k) => HashMap k v -> k -> v
(#!) = (HS.!)

matrixSize :: Matrix a -> Int
matrixSize m = nrows m * ncols m

matrixBounded :: Matrix a -> Cursor -> Bool
matrixBounded m (x, y) = not $ (x < 1 || y < 1 || ncols m < x || nrows m < y)

--

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

renderWithPositions :: [(String, Set Cursor)] -> Maze -> String
renderWithPositions targets maze =
  unlines
  . map concat
  . Mx.toLists
  . Mx.mapPos (\cur c -> fmt cur (c : []))
  $ maze
  where
    color cur = fst <$> find (Set.member (swap cur) . snd) targets
    fmt cur s = printf $ fromMaybe s . fmap (\c -> printf "\x1b[%sm%s\x1b[39m" c s) $ color cur

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

cursorDelta :: Cursor -> Direction -> Cursor
cursorDelta (x, y) 0 = (x, y - 1)
cursorDelta (x, y) 1 = (x + 1, y)
cursorDelta (x, y) 2 = (x, y + 1)
cursorDelta (x, y) 3 = (x - 1, y)

cursorDeltaSafe :: Maze -> Cursor -> Direction -> [Cursor]
cursorDeltaSafe maze c d = matrixBounded maze `filter` [cursorDelta c d]

--

pixValid :: PixCheck -> Bool
pixValid (this, that, rotation, direction) =
  filter (flipDir direction ==) thisRequires == filter (flipDir direction ==) thatRequires
    where
      thisRequires :: Pix
      thisRequires = (rotation + opposite) `rotate` mapChar this

      thatRequires :: Pix
      thatRequires = if that == ' ' then [] else mapChar that

-- given top and left pix is solved, verify this pix is valid after rotation
mazePixValid :: PixValidPrecomp -> Maze -> CursorSet -> Cursor -> Char -> Rotation -> Bool
mazePixValid pixValidP maze solveds cur@(x, y) this rotation =
  all checkDirection directions
  where
    checkDirection d =
      if (not $ matrixBounded maze curDelta) || curDelta `Set.member` solveds
      then pixValidP #! (this, char, rotation, d)
      else True
        where
          curDelta = cursorDelta cur d
          char =
            if matrixBounded maze curDelta
            then uncurry Mx.getElem (swap curDelta) maze
            else ' '

chooseRotation :: Char -> Pix
chooseRotation '╋' = [0]
chooseRotation '┃' = [0,1]
chooseRotation '━' = [0,1]
chooseRotation _ = rotations

solve :: PixValidPrecomp -> RotatePrecomp -> Maze -> [Maze]
solve pixValidP rotP = take 1 . solve_ 2 (1, 1) Set.empty []
  where
    solve_ :: Direction -> Cursor -> CursorSet -> [(Cursor, Direction)] -> Maze -> [Maze]
    solve_ origin cur@(x, y) solveds continues maze = do
      this <- pure $ Mx.getElem y x maze
      rotation <- mazePixValid pixValidP maze solveds cur this `filter` (chooseRotation this)
      -- canUseMutation = length rotations == 1
      solveRotation rotation (rotP #! (this, rotation))

      where
        solveRotation :: Rotation -> Char -> [Maze]
        solveRotation rotation rotated =
          if Set.size solveds == matrixSize maze - 1
          then [maze']
          else do
            ((cursor, origin), continues') <- maybeToList $ uncons continues'
            solve_ origin cursor solveds' continues' (traceBoard maze')

          where
            cursors' = (matrixBounded maze . fst) `filter` ((cursorDelta cur >>= (,)) <$> mapChar rotated)
            solveds' = cur `Set.insert` solveds
            continues' = dropWhile ((`Set.member` solveds) . fst) (continues ++ cursors')
            maze' = Mx.setElem rotated (y, x) maze

            traceBoard board = if 't' == 'f' then board else trace traceStr board
              where
                clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
                traceStr = clear ++ show (cur, continues') ++ "\n" ++ renderWithPositions positions board
                -- traceStr = clear ++ render board -- cheap
                positions =
                  [ ("31", Set.singleton cur)
                  , ("34", solveds')
                  , ("32", Set.fromList $ fmap fst continues')
                  ]

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
      this <- chars
      that <- charsWithSpecial
      r <- rotations
      d <- directions
      pure (this, that, r, d)

main = do
  pure verifyPixelModel

  pixValidPrecomp <- pure pixValidPrecomputed
  rotatePrecomp <- pure rotatePrecomputed

  input <- parse <$> getContents
  solveds <- pure . solve pixValidPrecomp rotatePrecomp $ input

  mapM_ (putStrLn . printRot . computeRotations input) $ solveds
  mapM_ (putStrLn . render) $ solveds
