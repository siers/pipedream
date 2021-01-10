{-# LANGUAGE TupleSections, NamedFieldPuns #-}

module Main where

import Control.Monad (join, mplus)
import Data.Either.Extra (fromLeft, mapLeft)
import Data.Function (on)
import Data.List (sort, sortOn, elemIndex, find, (\\))
import Data.Map (Map, (!))
import Data.Matrix as Mx (Matrix, ncols, nrows)
import Data.Maybe (fromMaybe, fromJust)
import Data.Set (Set)
import Data.Tuple (swap)
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Matrix as Mx
import qualified Data.Set as Set
import Text.Printf (printf)

t a = trace (show a) a

type Direction = Int -- top 0, right 1, bottom 2, left 3
type Pix = [Direction]
type Maze = Matrix Char

type Cursor = (Int, Int)
type CursorRot = (Int, Int, Int)
type Rotation = Int

-- PartId distinguishes the connected graphs (partitions) by their smallest cursor (by def. ascending order)
type PartId = Cursor
type PartEquiv = Map PartId PartId
type Solveds = Map Cursor PartId
type CursorSet = Set Cursor

-- (# valid rotations, cursor, value, directly pointed)
data Continue = Continue
  { cursor :: Cursor
  , cchar :: Char
  , choices :: Int
  , direct :: Bool
  , origin :: PartId
  , created :: Int } -- created at iter

data Progress = Progress
  { iter :: Int
  , maze :: Maze
  , continues :: [Continue]
  , continuesSet :: CursorSet -- cached continues cursors
  , solveds :: Solveds
  , partEquiv :: PartEquiv } -- partition equivalence (if b connects to a, then add (b, a) to map)

type Solution = Either Maze [Progress]

instance Show Continue where
  show Continue{cursor, direct} =
    "Cursor " ++ show cursor ++ if direct then "d" else "u"

instance Show Progress where
  show ps@Progress{iter, continues} =
    "Progress" ++ show (iter, length continues)

matrixSize :: Matrix a -> Int
matrixSize m = nrows m * ncols m

matrixBounded :: Matrix a -> Cursor -> Bool
matrixBounded m (x, y) = x >= 0 && y >= 0 && ncols m > x && nrows m > y

matrixBoundaryIndices :: Matrix a -> [(Int, Int)]
matrixBoundaryIndices m = join . Mx.toList . Mx.matrix (nrows m) (ncols m) $ \(y, x) ->
  if x == 1 || y == 1 || x == ncols m || y == ncols m
  then [(x - 1, y - 1)]
  else []

to0Cursor (y, x) = (x - 1, y - 1)

matrixIndices :: Matrix a -> [Cursor]
matrixIndices m = Mx.toList $ Mx.matrix (nrows m) (ncols m) to0Cursor

mxGetElem :: Int -> Int -> Matrix a -> a
mxGetElem x y m = Mx.getElem (y + 1) (x + 1) m

mxGetElem' = uncurry mxGetElem

mxSetElem :: a -> (Int, Int) -> Matrix a -> Matrix a
mxSetElem v (x, y) m = Mx.setElem v (y + 1, x + 1) m

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- stops at first cycle
lookupConverge :: (Ord v, Eq v, Show v) => Map v v -> v -> v
lookupConverge m v = converge (\v' -> lookup v $ v') $ lookupDef v
  where
    lookup first val = fromJust . def (Just first) . fmap lookupDef . find (/= first) $ Just val
    lookupDef v = fromMaybe v . flip Map.lookup m $ v
    def val = flip mplus val

--

directions = [0, 1, 2, 3]
rotations = directions -- nu tā sanāk

edgePriority :: Map Char [Int]
edgePriority = Map.fromList
  [ ('╋', [])
  , ('┣', [1])
  , ('┻', [1])
  , ('┫', [1])
  , ('┳', [1])
  , ('┃', [1])
  , ('━', [1])
  , ('┛', [])
  , ('┏', [])
  , ('┓', [])
  , ('┗', [])
  , ('╺', [])
  , ('╻', [])
  , ('╸', [])
  , ('╹', [])
  ]

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

(chars, pixs) = unzip charMapEntries
charsWithSpecial = chars ++ [' '] -- ' ' == wall

charMap :: Map Char Pix
charMap = Map.fromList charMapEntries

pixMap :: Map Pix Char
pixMap = Map.fromList $ map swap charMapEntries

toPix = (charMap !)
toChar = (pixMap !) . sort

parse :: String -> Maze
parse =
  Mx.fromLists
  . filter (not . null)
  . lines

render :: Maze -> String
render = unlines . Mx.toLists

renderWithPositions :: Solveds -> PartEquiv -> [(String, Set Cursor)] -> Maze -> String
renderWithPositions solveds partEquiv coloredSets maze =
  unlines
  . map concat
  . Mx.toLists
  . Mx.mapPos (\cur1 c -> fmt (to0Cursor cur1) (c : []))
  $ maze
  where
    color256 = (printf "\x1b[38;5;%im" . ([24 :: Int, 27..231] !!)) . (`mod` 70) . colorHash :: Cursor -> String
    colorHash = (+15) . (\(x, y) -> x * 67 + y * 23)
    colorPart cur = color256 . lookupConverge partEquiv <$> Map.lookup cur solveds
    colorSet cur = printf "\x1b[%sm" . fst <$> find (Set.member cur . snd) coloredSets
    color cur = colorPart cur `mplus` colorSet cur

    fmt cur s = printf $ fromMaybe s . fmap (\c -> printf "%s%s\x1b[39m" c s) $ color cur

-- C: n=1, CW: n=-1
rotateDir :: Int -> Direction -> Direction
rotateDir n = (`mod` 4) . (+ n)

flipDir = rotateDir 2
flipPix = (directions \\)

rotate :: Rotation -> Pix -> Pix
rotate r = map (rotateDir r)

rotateChar :: Rotation -> Char -> Char
rotateChar r = toChar . rotate r . toPix

verifyPixelModel :: Bool
verifyPixelModel = (pixs ==) . last $
  [ map (toPix . toChar . rotate 1) pixs
  , map (toPix . toChar . rotate 1 . rotate 1) pixs
  , map (toPix . toChar . rotate 1 . rotate 1 . rotate 1) pixs
  , map (toPix . toChar . rotate 1 . rotate 1 . rotate 1 . rotate 1) pixs
  ]

cursorDelta :: Cursor -> Direction -> Cursor
cursorDelta (x, y) 0 = (x, y - 1)
cursorDelta (x, y) 1 = (x + 1, y)
cursorDelta (x, y) 2 = (x, y + 1)
cursorDelta (x, y) 3 = (x - 1, y)
cursorDelta _ _      = error "only defined for 4 directions"

-- just to be able to switch quickly to see if it's better
cursorDeltasSafe :: Matrix a -> Cursor -> Pix -> [(Cursor, Direction)]
cursorDeltasSafe m c p = filter (matrixBounded m . fst) $ (cursorDelta c >>= (,)) `map` p

--

pixValid :: (Char, Char, Rotation, Direction) -> Bool
pixValid (this, that, rotation, direction) = satisfied thisRequires thatRequires
  where
    satisfied = (==) `on` filter (flipDir direction ==) :: Pix -> Pix -> Bool
    thisRequires = (rotation + 2) `rotate` toPix this :: Pix
    thatRequires = if that == ' ' then [] else toPix that :: Pix

pixValidRotations :: Maze -> Solveds -> Cursor -> Pix
pixValidRotations maze solveds cur =
  (\r -> all (checkDirection r) directions) `filter` chooseRotation this
  where
    this = mxGetElem' cur maze :: Char

    chooseRotation :: Char -> Pix
    chooseRotation '╋' = [0]
    chooseRotation '┃' = [0,1]
    chooseRotation '━' = [0,1]
    chooseRotation _ = rotations

    checkDirection rotation d =
      if not bounded || curDelta `Map.member` solveds
      -- if not $ matrixBounded maze curDelta && curDelta `Set.notMember` solveds
      then pixValid (this, char, rotation, d)
      else True
        where
          bounded = matrixBounded maze curDelta
          curDelta = cursorDelta cur d
          char = if bounded then uncurry mxGetElem curDelta maze else ' '

cursorToContinue :: Maze -> Solveds -> Pix -> PartId -> (Cursor, Direction) -> Continue
cursorToContinue maze solveds pix origin (c@(x, y), o) = Continue c char (nRotations maze c) direct origin' 0
  where
    char = mxGetElem x y maze
    direct = o `elem` pix
    origin' = if direct then origin else c

    nRotations :: Maze -> Cursor -> Int
    nRotations maze c = length $ pixValidRotations maze solveds c

sortContinues :: Progress -> [Continue] -> [Continue]
sortContinues p cs = sortOn depth cs
  where
    depth :: Continue -> Int
    -- depth c = created c
    -- depth c = (\(x, y) -> x + y) $ cursor c
    depth c = created c + (choices c) * 5
    -- depth c = created c + (choices c) * 100

--

traceBoard :: Progress -> Progress
traceBoard progress@Progress{continues=[]} = progress
traceBoard progress@Progress{iter, maze, continues=(Continue{cursor=cur}: continues), solveds, partEquiv} =
  tracer iter progress
  where
    tracer iter -- reorder/comment out clauses to disable tracing
      | Map.size solveds == matrixSize maze - 1 = trace traceStr
      --  | True = id
      --  | True = trace traceStr
      | iter `mod` 50 == 0 = trace traceStr
      | iter `mod` 50 == 0 = trace solvedStr
      | True = id

    percentage = (fromIntegral $ Map.size solveds) / (fromIntegral $ matrixSize maze)
    solvedStr = ("\x1b[2Ksolved: " ++ show percentage ++ "%" ++ "\x1b[1A")
    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    -- traceStr = show progress ++ "\n" ++ renderWithPositions positions maze
    -- traceStr = show iter ++ "\n" ++ renderWithPositions positions maze
    traceStr = clear ++ renderWithPositions solveds partEquiv positions maze
    -- traceStr = renderWithPositions solveds partEquiv positions maze
    -- traceStr = clear ++ render maze -- cheap
    contFast = map cursor . filter ((== 1) . choices) $ continues
    contSlow = map cursor . filter ((>= 2) . choices) $ continues
    positions =
      [ ("31", Set.singleton cur) -- red
      , ("34", Set.fromList $ map cursor continues) -- green
      -- , ("34", Set.fromList contFast) -- green
      -- , ("32", Set.fromList contSlow) -- magenta
      ]

solveRotation :: Progress -> Continue -> Solution
solveRotation
  Progress{iter, maze, continues, solveds, continuesSet, partEquiv}
  Continue{cursor=cur, cchar=this, created, origin} =
    if dead
      then Right []
      else
        if Map.size solveds == matrixSize maze
        then Left maze
        else solve' . traceBoard $ progress

  where
    progress = progressRaw { continues = dropBad $ sortContinues progressRaw continues' }
    progressRaw = Progress (iter + 1) maze continues' continuesSet' solveds partEquiv'

    dropBad = dropWhile ((`Map.member` solveds) . cursor)
    continues' = (next ++ continues)
    continuesSet' = (continuesSet `Set.union` Set.fromList (map cursor next)) Set.\\ (Map.keysSet solveds)

    next :: [Continue]
    next =
      filter (\Continue{choices=c, direct=d} -> c < 2 || d)
      . map (\c -> c { created = created + 1 })
      . map (cursorToContinue maze solveds (toPix this) origin')
      . filter (not . (`Map.member` solveds) . fst)
      $ cursorDeltasSafe maze cur directions

    partEquate = lookupConverge partEquiv

    -- if null directed, then neighbours = [], so partEquiv = partEquiv'
    dead = null directed && not connected && null hope
      where
        directed = dropBad $ filter (\c -> direct c) next
        connected = (0, 0) == partEquate cur
        hope = dropBad $ filter ((partEquate cur ==) . partEquate . cursor) continues

    neighbours = map (partEquate . fst) $ cursorDeltasSafe maze cur (toPix this)
    (origin':neighbours') = sort $ neighbours ++ [origin]

    partEquiv' = foldr (uncurry Map.insert) partEquiv $ (, origin') <$> neighbours'

solve' :: Progress -> Solution
solve' Progress{continues=[]} = Right []
solve' progress@Progress{maze=maze, continues=(continue: continues), solveds=solveds, continuesSet} =
  fmap join . traverse (solveRotation' . flip rotateChar (cchar continue)) $
    pixValidRotations maze solveds (cursor continue)
  where
    solveRotation' rotated =
      solveRotation
        (progress
          { continues = continues
          , solveds = Map.insert (cursor continue) (origin continue) solveds
          , maze = mxSetElem rotated (cursor continue) maze })
        continue { cchar = rotated }

solve :: Maze -> [Maze]
solve maze =
  fromLeft [] . mapLeft pure . solve'
  $ Progress 0 maze [continue (0, 0)] Set.empty Map.empty Map.empty
  where continue c = Continue c (uncurry mxGetElem c maze) 0 True (0, 0) 0

--

printRot :: [CursorRot] -> String
printRot =
  unlines
  . map (\(x, y) -> "rotate " ++ show x ++ " " ++ show y)
  . (>>= (\(x, y, r) -> take r (repeat (x, y))))
  . reverse

computeRotations :: Maze -> Maze -> [CursorRot]
computeRotations input solved = Mx.toList . Mx.matrix (nrows input) (ncols input) $ cursorRot . to0Cursor
  where
    cursorRot (x, y) = (x, y, get input `rotations` get solved)
      where
        get = mxGetElem x y
        rotations from to = fromJust $ to `elemIndex` iterate (rotateChar 1) from

main :: IO ()
main = do
  input <- parse <$> getContents
  solveds <- pure . solve $ input

  -- mapM_ (putStrLn . printRot . computeRotations input) $ solveds
  -- mapM_ (putStrLn . render) $ solveds
  seq solveds (pure ())
