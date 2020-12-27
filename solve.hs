{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (join, mplus)
import Data.Bifunctor
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List (sort, sortOn, isSubsequenceOf, elemIndex, uncons, concat, find, (\\), partition)
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

matrixBoundaryIndices :: Matrix a -> [(Int, Int)]
matrixBoundaryIndices m = join . Mx.toList . Mx.matrix (nrows m) (ncols m) $ \cur@(y, x) ->
  if x == 1 || y == 1 || x == ncols m || y == ncols m
  then [swap cur]
  else []

mxGetElem :: Int -> Int -> Matrix a -> a
mxGetElem x y m = Mx.getElem y x m

mxGetElem' = uncurry mxGetElem

mxSetElem :: a -> (Int, Int) -> Matrix a -> Matrix a
mxSetElem v (x, y) m = Mx.setElem v (y, x) m

--

directions = [0, 1, 2, 3]
rotations = directions -- nu tā sanāk

edgePriority :: Map Char [Int]
edgePriority = Map.fromList
  [ ('╋', [5])
  , ('┣', [4])
  , ('┻', [4])
  , ('┫', [4])
  , ('┳', [4])
  , ('┃', [3])
  , ('━', [3])
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
flipPix = (directions \\)

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

cursorDeltaSafe :: Matrix a -> Cursor -> Direction -> [Cursor]
cursorDeltaSafe maze c d = matrixBounded maze `filter` [cursorDelta c d]

-- points to cursor's closest side to maze
-- any offsets should be adjusted via dbgm output in ghci
cursorMagnet :: Matrix a -> Cursor -> Direction
cursorMagnet maze (x', y') =
  if x > (h - y)
  then if x > y then 1 else 2
  else if x > y then 0 else 3
  where
    x = (fromIntegral x') * scale
    y = fromIntegral y'
    w = fromIntegral $ Mx.ncols maze
    h = fromIntegral $ Mx.nrows maze
    scale = h / w

-- order continue' by going to sides first
-- more efficent implementation: recurse and put flipDir as last
deltaOrder :: Matrix a -> Cursor -> Pix -> Pix
deltaOrder maze cur pix = order magnet \\ flipPix pix
  where
    magnet = cursorMagnet maze cur
    order :: Direction -> Pix
    order 0 = [2, 3, 1, 0]
    order 1 = [3, 0, 2, 1]
    order 2 = [0, 1, 3, 2]
    order 3 = [1, 2, 0, 3]

dbgm :: String
dbgm = render $ Mx.matrix (nrows maze) (ncols maze) $ \cur ->
  mapPix $ deltaOrder maze cur (flipPix [flipDir . cursorMagnet maze . swap $ cur])
  -- (cursorMagnet maze . swap $ cur)
  where
    maze = Mx.matrix 20 15 (const 0 :: a -> Int)

-- choose next directions at cursor for piece pointing at p directions
cursorDeltasSafeOrdered :: Matrix a -> Cursor -> Pix -> [(Cursor, Direction)]
cursorDeltasSafeOrdered m c p = filter (matrixBounded m . fst) $
  (cursorDelta c >>= (,)) `map` deltaOrder m c p

-- just to be able to switch quickly to see if it's better
cursorDeltasSafe :: Matrix a -> Cursor -> Pix -> [(Cursor, Direction)]
cursorDeltasSafe m c p = filter (matrixBounded m . fst) $ (cursorDelta c >>= (,)) `map` p

--

pixValid :: PixCheck -> Bool
pixValid (this, that, rotation, direction) =
  filter (flipDir direction ==) thisRequires == filter (flipDir direction ==) thatRequires
    where
      thisRequires :: Pix
      thisRequires = (rotation + opposite) `rotate` mapChar this

      thatRequires :: Pix
      thatRequires = if that == ' ' then [] else mapChar that

pixValidRotations :: PixValidPrecomp -> Maze -> CursorSet -> Cursor -> Char -> Pix
pixValidRotations pixValidP maze solveds cur@(x, y) this =
  (\r -> all (checkDirection r) directions) `filter` chooseRotation this
  where
    chooseRotation :: Char -> Pix
    chooseRotation '╋' = [0]
    chooseRotation '┃' = [0,1]
    chooseRotation '━' = [0,1]
    chooseRotation _ = rotations

    checkDirection rotation d =
      if (not $ matrixBounded maze curDelta) || curDelta `Set.member` solveds
      then pixValidP #! (this, char, rotation, d)
      else True
        where
          curDelta = cursorDelta cur d
          char =
            if matrixBounded maze curDelta
            then uncurry mxGetElem curDelta maze
            else ' '

pixValidRotations' :: PixValidPrecomp -> Maze -> CursorSet -> Cursor -> Pix
pixValidRotations' pvp maze solveds cur =
  pixValidRotations pvp maze solveds cur (mxGetElem' cur maze)

initialSet :: Maze -> [Cursor]
initialSet maze =
  map (\(cur, _, _) -> cur)
  . sortOn (\(_, _, p) -> p)
  $ onCur =<< (matrixBoundaryIndices maze)
  where
    onCur :: Cursor -> [(Cursor, Char, Int)]
    onCur cur = (\p -> (cur, elem, p)) <$> (edgePriority ! elem)
      where elem = uncurry mxGetElem cur maze

solve :: PixValidPrecomp -> RotatePrecomp -> Maze -> [Maze]
solve pixValidP rotP maze =
  take 1
  . join
  . maybeToList
  . fmap (\(head, initial) -> solve_ 0 head initial Set.empty [] maze)
  $ uncons (initialSet maze)
  where
    solve_ :: Direction -> Cursor -> [Cursor] -> CursorSet -> [(Cursor, Direction)] -> Maze -> [Maze]
    solve_ origin cur@(x, y) initial solveds continues maze = do
      this <- pure $ mxGetElem x y maze
      rotation <- pixValidRotations pixValidP maze solveds cur this
      -- canUseMutation = length rotations == 1
      solveRotation rotation (rotP #! (this, rotation))

      where
        solveRotation :: Rotation -> Char -> [Maze]
        solveRotation rotation rotated =
          if Set.size solveds == matrixSize maze - 1
          then [maze']
          else do
            ((continue, origin), continues'') <- maybeToList $ uncons continues'
            solve_ origin continue [] solveds' continues'' (traceBoard maze')

          where
            nRotations = length . (pixValidRotations' pixValidP maze solveds') . fst
            withRotations = nRotations >>= (,)

            (nextFast, (next, bad)) =
              bimap (map snd) (partition (flip elem (mapChar rotated) . snd) . map snd)
              . partition ((1 >=) . fst)
              . sortOn fst .  map withRotations
              . ((map (, 0) initial) ++)
              $ cursorDeltasSafe maze cur directions

            (continueHead, continuesTail) = fromMaybe (([], [])) . fmap (bimap return id) $ uncons continues
            continues' = dropWhile ((`Set.member` solveds) . fst) $
              nextFast ++ continueHead ++ next ++ continuesTail
              -- nextFast ++ next ++ continues
              -- nextFast ++ continues ++ next

            solveds' = cur `Set.insert` solveds
            maze' = Mx.setElem rotated (y, x) maze

            traceBoard board =
              if 't' == 'f'
              then if 't' == 'f' && Set.size solveds `mod` 50 == 0
                then trace solvedStr board
                else board
              else trace traceStr board

              where
                percentage = (fromIntegral $ Set.size solveds) / (fromIntegral $ matrixSize maze)
                solvedStr = ("\x1b[2Ksolved: " ++ show percentage ++ "%" ++ "\x1b[1A")
                clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
                -- traceStr = show (nextFast, continues) ++ "\n" ++ renderWithPositions positions board
                traceStr = clear ++ renderWithPositions positions board
                -- traceStr = clear ++ render board -- cheap
                positions =
                  [ ("31", Set.singleton cur)
                  , ("34", solveds')
                  , ("35", Set.fromList $ map fst nextFast)
                  -- , ("33", Set.fromList $ map fst bad)
                  , ("32", Set.fromList $ map fst continues')
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
        get = mxGetElem x y
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
