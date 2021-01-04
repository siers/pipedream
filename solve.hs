{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (join, foldM)
import Data.Bifunctor
import Data.Either.Extra (fromLeft, mapLeft)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List (sort, sortOn, elemIndex, uncons, find, (\\), partition)
import Data.Map (Map, (!))
import Data.Matrix as Mx (Matrix, ncols, nrows)
import Data.Maybe (fromMaybe, fromJust, listToMaybe)
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

type CursorSet = Set Cursor
-- (# valid rotations, cursor, value, directly pointed)
data Continue = Continue
  { cursor :: Cursor
  , cchar :: Char
  , choices :: Int
  , direct :: Bool }

data Progress = Progress
  { iter :: Int
  , maze :: Maze
  , continues :: [Continue]
  , continuesSet :: CursorSet -- cached continues cursors
  , solveds :: CursorSet }

type Solution = Either Maze [Progress]

data HashedFun = HashedFun
  { rotate' :: Rotation -> Char -> Char
  , pixValid' :: (Char, Char, Rotation, Direction) -> Bool }

instance Show Progress where
  show ps@Progress{iter=iter, continues=continues} =
    "Progress" ++ show (iter, length continues)

{-# INLINE (#!) #-}
(#!) :: (Eq k, Hashable k) => HashMap k v -> k -> v
(#!) = (HS.!)

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

unconsMay :: [a] -> (Maybe a, [a])
unconsMay a = (listToMaybe a, drop 1 a)

unconsList :: [a] -> ([a], [a])
unconsList = fromMaybe (([], [])) . fmap (bimap return id) . uncons

matrixCopy :: (Cursor -> Bool) -> Matrix a -> Matrix a -> Matrix a
matrixCopy match dst src = flip Mx.mapPos dst $ \(y, x) a ->
  if match (x - 1, y - 1)
  then mxGetElem (x - 1) (y - 1) src
  else a

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f = uncurry (foldM f) . fromJust . uncons

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
  . Mx.mapPos (\cur1 c -> fmt (to0Cursor cur1) (c : []))
  $ maze
  where
    color cur = fst <$> find (Set.member cur . snd) targets
    fmt cur s = printf $ fromMaybe s . fmap (\c -> printf "\x1b[%sm%s\x1b[39m" c s) $ color cur

-- C: n=1, CW: n=-1
rotateDir :: Int -> Direction -> Direction
rotateDir n = (`mod` 4) . (+ n)

flipDir = rotateDir 2
flipPix = (directions \\)

rotate :: Rotation -> Pix -> Pix
rotate r = map (rotateDir r)

rotateChar :: Rotation -> Char -> Char
rotateChar r = mapPix . rotate r . mapChar

verifyPixelModel :: Bool
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
cursorDelta _ _      = error "only defined for 4 directions"

cursorDeltaSafe :: Matrix a -> Cursor -> Direction -> [Cursor]
cursorDeltaSafe maze c d = matrixBounded maze `filter` [cursorDelta c d]

-- just to be able to switch quickly to see if it's better
cursorDeltasSafe :: Matrix a -> Cursor -> Pix -> [(Cursor, Direction)]
cursorDeltasSafe m c p = filter (matrixBounded m . fst) $ (cursorDelta c >>= (,)) `map` p

cursorShrink :: Int -> Cursor -> Cursor
cursorShrink scale (x, y) = (max 0 $ x `div` scale, max 0 $ y `div` scale)

withinRadius :: Double -> Cursor -> Bool
withinRadius r cur = r * r > x * x + y * y
  where (x, y) = bimap fromIntegral fromIntegral cur
  -- Mx.matrix 20 20 (withinRadius 5 . to0Cursor)

--

pixValid :: (Char, Char, Rotation, Direction) -> Bool
pixValid (this, that, rotation, direction) =
  filter (flipDir direction ==) thisRequires == filter (flipDir direction ==) thatRequires
    where
      thisRequires :: Pix
      thisRequires = (rotation + 2) `rotate` mapChar this

      thatRequires :: Pix
      thatRequires = if that == ' ' then [] else mapChar that

pixValidRotations :: HashedFun -> Maze -> CursorSet -> Cursor -> Char -> Pix
pixValidRotations HashedFun{pixValid'=pixValid'} maze solveds cur this =
  (\r -> all (checkDirection r) directions) `filter` chooseRotation this
  where
    chooseRotation :: Char -> Pix
    chooseRotation '╋' = [0]
    chooseRotation '┃' = [0,1]
    chooseRotation '━' = [0,1]
    chooseRotation _ = rotations

    checkDirection rotation d =
      if not bounded || curDelta `Set.member` solveds
      -- if not $ matrixBounded maze curDelta && curDelta `Set.notMember` solveds
      then pixValid' (this, char, rotation, d)
      else True
        where
          bounded = matrixBounded maze curDelta
          curDelta = cursorDelta cur d
          char = if bounded then uncurry mxGetElem curDelta maze else ' '

pixValidRotations' :: HashedFun -> Maze -> CursorSet -> Cursor -> Pix
pixValidRotations' h maze solveds cur =
  pixValidRotations h maze solveds cur (mxGetElem' cur maze)

cursorToContinue :: HashedFun -> Maze -> CursorSet -> Pix -> (Cursor, Direction) -> Continue
cursorToContinue h maze solveds pix (c@(x, y), o) = Continue c char (nRotations maze c) direct
  where
    char = mxGetElem x y maze
    direct = o `elem` pix

    nRotations :: Maze -> Cursor -> Int
    nRotations maze c = length $ pixValidRotations' h maze solveds c

sortContinues :: Progress -> [Continue] -> [Continue]
sortContinues p cs = sortOn (\cont -> (snd <$> find (any (cursor cont ==) . fst) islands, depth cont)) cs
  where
    depth :: Continue -> Int
    depth c = (\(x, y) -> x + y) $ cursor c

    islands :: [([Cursor], Int)]
    islands = sortOn (length . fst) $ cursorPartitions p . map cursor $ cs

-- continue cursors reachable from c, found from Int cursor visits
cursorsReachable :: Progress -> Cursor -> ([Cursor], Int)
cursorsReachable p@Progress{maze=maze, continuesSet=continuesSet, solveds=solveds} c =
  (\(x, y, _) -> (x, y)) . isolated Set.empty $ ([], 0, [c])
  where
    isolated :: CursorSet -> ([Cursor], Int, [Cursor]) -> ([Cursor], Int, [Cursor])
    isolated _ s@(_, _, []) = s
    isolated visited (found, size, (n:next)) =
      let (found', next') = isolated' visited' n
      in isolated visited' (found ++ found', size + length next', clean $ next' ++ next)
      where
        visited' = (n `Set.insert` visited)
        clean = dropWhile (`Set.member` visited')

    isolated' :: CursorSet -> Cursor -> ([Cursor], [Cursor])
    isolated' visited c =
      let
        deltas = map fst $ cursorDeltasSafe maze c directions
        next = unvisited [visited, solveds] `filter` deltas
      in
        ((`Set.member` continuesSet) `filter` next, next)

    unvisited :: [CursorSet] -> Cursor -> Bool
    unvisited visited c = all (not . Set.member c) visited

cursorPartitions :: Progress -> [Cursor] -> [([Cursor], Int)]
cursorPartitions _ [] = []
cursorPartitions p (c:cs) =
  let
    (neighbours, count) = cursorsReachable p c
    neighboursC = c : filter (\c -> any (c ==) neighbours) cs
  in (neighboursC, count) : cursorPartitions p (filter (\c -> all (c /=) (neighbours)) cs)

-- continuePartitions :: Progress -> [Continue] -> [([Continue], Int)]
-- continuePartitions _ [] = []
-- continuePartitions p (c:cs) =
--   let
--     (neighbours, count) = cursorsReachable p (sel2 c)
--     neighboursC = c : filterContinueCur (\c -> any (c ==) neighbours) cs
--     filterContinueCur f cs = filter (\c -> f (sel2 c)) cs
--   in (neighboursC, count) : continuePartitions p (filterContinueCur (\c -> all (c /=) (neighbours)) cs)

--

-- edge pieces with unambiguous rotations
initialSet :: Maze -> [Cursor]
initialSet maze =
  map (\(cur, _, _) -> cur)
  . sortOn (\(_, _, p) -> p)
  $ onCur =<< (matrixBoundaryIndices maze)
  where
    onCur :: Cursor -> [(Cursor, Char, Int)]
    onCur cur = (\p -> (cur, elem, p)) <$> (edgePriority ! elem)
      where elem = uncurry mxGetElem cur maze

solve :: HashedFun -> Maze -> [Maze]
solve h@HashedFun{rotate'=rotate'} maze =
  fromLeft [] . mapLeft pure . solve' $ [simplestPSolution]
  where
    initialContinue :: Cursor -> Continue
    initialContinue c = Continue c (uncurry mxGetElem c maze) 0 True

    simplestPSolution = Progress 0 maze [(initialContinue (0, 0))] Set.empty Set.empty

    -- pure backtracking
    solve' :: [Progress] -> Solution
    solve' [] = Right []
    solve' ps = pure ps >>= traverse (solve'' (-1)) >>= solve' . join

    solve'' :: Int -> Progress -> Solution
    solve'' _ Progress{continues=[]} = Right []
    solve'' lifespan progress'@Progress{maze=maze', continues=(Continue{cursor=cur, cchar=this}: continues'), solveds=solveds', continuesSet=cset} =
      iterGuard $ do
        let rotations = pixValidRotations h maze' solveds' cur this
        fmap join . traverse (solveRotation . flip rotate' this) $ rotations

      where
        iterGuard compute =
          if lifespan == 0
          then Right [progress']
          else compute

        solveRotation :: Char -> Solution
        solveRotation rotated =
          if Set.size solveds == matrixSize maze
          then Left maze
          else
            if all (not . null . fst . cursorsReachable progressRaw . cursor) islands
            then solve'' (lifespan - 1) . traceBoard $ progress
            else Right []

          where
            progress = progressRaw { continues = sortContinues progressRaw continues }
            progressRaw = Progress (iter progress' + 1) maze continues continuesSetNext solveds

            maze = mxSetElem rotated cur maze'

            continues = ((`Set.member` solveds) . cursor) `dropWhile` (next ++ continues')
            continuesSetNext = (cset `Set.union` Set.fromList (map cursor next)) Set.\\ solveds
            solveds = cur `Set.insert` solveds'

            islands, next :: [Continue]
            (islands, next) =
              bimap id (filter (\Continue{choices=c, direct=d} -> c < 1 || d))
              . partition (\Continue{cursor=cur, choices=c, direct=d} -> c > 0 && not d && not (cur `Set.member` cset))
              . map (cursorToContinue h maze solveds (mapChar rotated))
              . filter (not . (`Set.member` solveds) . fst)
              $ cursorDeltasSafe maze cur directions

traceBoard :: Progress -> Progress
traceBoard progress@Progress{continues=[]} = progress
traceBoard progress@Progress{iter=iter, maze=maze, continues=(Continue{cursor=cur}: continues), solveds=solveds} =
  tracer iter progress
  where
    tracer iter -- reorder clauses to disable tracing
      --  | True = id
      --  | True = trace traceStr
      | iter `mod` 20 == 0 = trace traceStr
      | iter `mod` 200 == 0 = trace solvedStr
      | True = id

    percentage = (fromIntegral $ Set.size solveds) / (fromIntegral $ matrixSize maze)
    solvedStr = ("\x1b[2Ksolved: " ++ show percentage ++ "%" ++ "\x1b[1A")
    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    -- traceStr = show progress ++ "\n" ++ renderWithPositions positions maze
    -- traceStr = clear ++ renderWithPositions positions maze
    traceStr = renderWithPositions positions maze
    -- traceStr = clear ++ render maze -- cheap
    contFast = map cursor . filter ((== 1) . choices) $ continues
    contSlow = map cursor . filter ((>= 2) . choices) $ continues
    positions =
      [ ("33", Set.singleton cur) -- yellow
      , ("34", solveds) -- blue
      , ("32", Set.fromList contFast) -- green
      , ("35", Set.fromList contSlow) -- magenta
      ]

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

rotatePrecomputed :: Rotation -> Char -> Char
rotatePrecomputed = curry (HS.fromList entries #!)
  where
    entries = list (uncurry rotateChar)
    list f = (f >>= flip (,)) `map` all
    all = do
      r <- rotations
      p1 <- chars
      pure (r, p1)

pixValidPrecomputed :: (Char, Char, Rotation, Direction) -> Bool
pixValidPrecomputed = (HS.fromList list #!)
  where
    list = (pixValid >>= flip (,)) `map` all
    all = do
      this <- chars
      that <- charsWithSpecial
      r <- rotations
      d <- directions
      pure (this, that, r, d)

main :: IO ()
main = do
  -- hashedFun <- pure $ HashedFun rotateChar pixValid
  hashedFun <- pure $ HashedFun rotatePrecomputed pixValidPrecomputed

  input <- parse <$> getContents
  solveds <- pure . solve hashedFun $ input

  -- mapM_ (putStrLn . printRot . computeRotations input) $ solveds
  mapM_ (putStrLn . render) $ solveds
  -- mapM_ (const (pure ()) . render) $ solveds
