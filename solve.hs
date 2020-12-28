{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (join, mplus)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Bifunctor
import Data.Either (lefts, rights, partitionEithers)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List.Extra (groupSortOn)
import Data.Either.Extra (eitherToMaybe)
import Data.List (sort, sortOn, isSubsequenceOf, elemIndex, uncons, concat, find, (\\), partition, groupBy)
import Data.Map (Map, (!))
import Data.Matrix as Mx (Matrix, ncols, nrows)
import Data.Maybe (fromMaybe, isJust, fromJust, maybeToList, listToMaybe)
import Data.Set (Set)
import Data.Tuple.Select
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
-- (# valid rotations, cursor, origin, value, directly pointed)
type Continue = (Int, Cursor, Direction, Char, Bool)
-- scale, cursor (essentially a quadrant if you shrink cursor by scale
type Constraint = (Int, Cursor)
data PartialSolution = PartialSolution
  { iter :: Int
  , maze :: Maze
  , continues :: [Continue]
  , solveds :: CursorSet
  , constraints :: [Constraint] }

{-# INLINE (#!) #-}
(#!) :: (Eq k, Hashable k) => HashMap k v -> k -> v
(#!) = (HS.!)

matrixSize :: Matrix a -> Int
matrixSize m = nrows m * ncols m

matrixBounded :: Matrix a -> Cursor -> Bool
matrixBounded m (x, y) = x >= 1 && y >= 1 && ncols m >= x && nrows m >= y

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

unconsMay :: [a] -> (Maybe a, [a])
unconsMay a = (listToMaybe a, drop 1 a)

unconsList :: [a] -> ([a], [a])
unconsList = fromMaybe (([], [])) . fmap (bimap return id) . uncons

matrixCopy :: (Cursor -> Bool) -> Matrix a -> Matrix a -> Matrix a
matrixCopy copy dst src = flip Mx.mapPos dst $ \cur@(x, y) a ->
  if copy cur
  then mxGetElem x y src
  else a

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

canonicalRotations :: Map Char (Char, Int)
canonicalRotations = Map.fromList
  [ ('╋', ('╋', 0)) -- x
  , ('┣', ('┣', 0))
  , ('┻', ('┣', 1)) -- t
  , ('┫', ('┣', 2))
  , ('┳', ('┣', 3))
  , ('┃', ('┃', 0))
  , ('━', ('┃', 2)) -- I
  , ('┗', ('┗', 0)) -- l
  , ('┛', ('┗', 1))
  , ('┓', ('┗', 2))
  , ('┏', ('┗', 3))
  , ('╹', ('╹', 0)) -- i
  , ('╸', ('╹', 1))
  , ('╻', ('╹', 2))
  , ('╺', ('╹', 0))
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

-- cursor in the middle is zero 0, the closer to the edge, the bigger the value
cursorDepth :: Cursor -> Cursor -> Int
cursorDepth from@(i, j) (x, y) = abs $ (p $ x - i) * (p $ y - j)
  where p x = x + if x == 0 then 1 else 0

cursorShrink :: Int -> Cursor -> Cursor
cursorShrink scale (x, y) = ((x - 1) `div` scale, (y - 1) `div` scale)

cursorQuadrant :: Int -> Cursor -> Constraint
cursorQuadrant shrink c = (shrink, cursorShrink shrink c)

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
      if not bounded || curDelta `Set.member` solveds
      -- if not $ matrixBounded maze curDelta && curDelta `Set.notMember` solveds
      then pixValidP #! (this, char, rotation, d)
      else True
        where
          bounded = matrixBounded maze curDelta
          curDelta = cursorDelta cur d
          char = if bounded then uncurry mxGetElem curDelta maze else ' '

pixValidRotations' :: PixValidPrecomp -> Maze -> CursorSet -> Cursor -> Pix
pixValidRotations' pvp maze solveds cur =
  pixValidRotations pvp maze solveds cur (mxGetElem' cur maze)

constraintMet :: [Constraint] -> Cursor -> Bool
constraintMet constr c = all ((\(scale, quad) -> quad == cursorShrink scale c)) constr

joinSolutions :: PartialSolution -> PartialSolution -> PartialSolution
joinSolutions a b =
  if constraints a == constraints b
  then error . show $ ("bad:", constraints a, constraints b)
  else PartialSolution
    (iter a + iter b)
    (matrixCopy (constraintMet (constraints b)) (maze a) (maze b))
    (continues a ++ continues b)
    (solveds a `Set.intersection` solveds b)
    (constraints a ++ constraints b)

initialSet :: Maze -> [Cursor]
initialSet maze =
  map (\(cur, _, _) -> cur)
  . sortOn (\(_, _, p) -> p)
  $ onCur =<< (matrixBoundaryIndices maze)
  where
    onCur :: Cursor -> [(Cursor, Char, Int)]
    onCur cur = (\p -> (cur, elem, p)) <$> (edgePriority ! elem)
      where elem = uncurry mxGetElem cur maze

initialShrink = 4

solve :: PixValidPrecomp -> RotatePrecomp -> Maze -> [Maze]
solve pixValidP rotP maze =
  take 1 . lefts . solve' initialShrink $ quadrantSolutions
  where
    initialContinue :: Cursor -> Continue
    initialContinue edge@(x, y) = (0, edge, flipDir $ cursorMagnet maze edge, elem, True)
      where elem = mxGetElem x y maze

    quadrantSolutions =
      map (\cursors@(head:_) ->
        PartialSolution 0 maze (initialContinue `map` cursors) Set.empty [cursorQuadrant initialShrink head])
      . groupSortOn (cursorShrink initialShrink >>= (,))
      $ initialSet maze

    solve' :: Int -> [PartialSolution] -> [Either Maze PartialSolution]
    solve' _ [] = []
    solve' shrink psolutions =
      uncurry (++)
      . bimap (map Left) (solve' shrink) . partitionEithers
      . psolves
      -- . foldr combinePsolves
      -- . groupSortOn (constraints)
      $ psolutions
      where
        -- combinePsolves :: [PartialSolution] -> [PartialSolution] -> [PartialSolution]
        -- combinePsolves a b = a >>= (, b) >>=

        psolves :: [PartialSolution] -> [Either Maze PartialSolution]
        psolves ps = ps >>= solve'' 2000

    solve'' :: Int -> PartialSolution -> [Either Maze PartialSolution]
    solve'' _ PartialSolution{continues=[]} = []
    solve'' lifespan progress@PartialSolution{maze=maze', continues=((_, cur@(x, y), origin, this, _): continues), solveds=solveds', constraints=constraints} =
      iterGuard $ do
        let rotations = pixValidRotations pixValidP maze' solveds' cur this
        join . parMap rpar (\r -> solveRotation (rotP #! (this, r)) r) $ rotations

      where
        constraintViolated = not . constraintMet constraints

        iterGuard compute =
          if lifespan == 0 || constraintViolated cur
          then [Right progress]
          else compute

        solveRotation :: Char -> Rotation -> [Either Maze PartialSolution]
        solveRotation rotated rotation =
          if Set.size solveds == matrixSize maze
          then [Left maze]
          else solve'' (lifespan - 1) nextSolution

          where
            nextSolution :: PartialSolution
            nextSolution =
              PartialSolution (iter progress + 1) (traceBoard maze) continues' solveds constraints

            nRotations :: Maze -> Cursor -> Char -> Bool -> Bool -> Int
            nRotations maze c p direct ambig =
              if not ambig
              then 0
              else length $ pixValidRotations' pixValidP maze solveds c

            cursorToContinue :: Pix -> Maze -> (Cursor, Direction) -> Continue
            cursorToContinue pix maze (c@(x, y), o) = (nRotations maze c char direct True, c, o, char, direct)
              where
                char = mxGetElem x y maze
                direct = o `elem` pix

            next :: [Continue]
            next =
              filter (\(choices, c, o, p, d) -> choices < 2 || d)
              . map (cursorToContinue (mapChar rotated) maze)
              . filter (not . (`Set.member` solveds) . fst)
              $ cursorDeltasSafe maze cur directions

            continues' = ((`Set.member` solveds) . sel2) `dropWhile` (sortContinues $ next ++ continues)
            -- sortContinues = sortOn sel1 -- fastest
            sortContinues = sortOn (\c -> (constraintViolated (sel2 c), sel1 c))
            -- sortContinues = sortOn (\c -> (sel1 c))
            -- sortContinues = sortOn (\c -> (sel1 c, cursorDepth (sel2 c) cur))
            -- sortContinues = sortOn (\c -> (sel1 c, cursorMagnet maze (sel2 c) == sel3 c))

            solveds = cur `Set.insert` solveds'
            maze = Mx.setElem rotated (y, x) maze'

            traceBoard board =
              if 1 == 0
              then
                if 1 == 1 && iter progress `mod` 200 == 0
                then trace solvedStr board
                else board
              else
                if 1 == 1 -- && iter progress `mod` 200 == 0
                then trace traceStr board
                else board

              where
                percentage = (fromIntegral $ Set.size solveds) / (fromIntegral $ matrixSize maze)
                solvedStr = ("\x1b[2Ksolved: " ++ show percentage ++ "%" ++ "\x1b[1A")
                clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
                -- traceStr = (rotated:[]) ++ show (rotation, continues') ++ "\n" ++ renderWithPositions positions board
                traceStr = clear ++ renderWithPositions positions board
                -- traceStr = renderWithPositions positions board
                -- traceStr = clear ++ render board -- cheap
                contFast = map sel2 . filter ((== 1) . sel1) $ continues'
                contSlow = map sel2 . filter ((>= 2) . sel1) $ continues'
                positions =
                  [ ("33", Set.singleton cur) -- yellow
                  , ("34", solveds') -- blue
                  , ("32", Set.fromList contFast) -- green
                  , ("35", Set.fromList contSlow) -- magenta
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

  -- mapM_ (putStrLn . printRot . computeRotations input) $ solveds
  mapM_ (putStrLn . render) $ solveds
  -- mapM_ (const (pure ()) . render) $ solveds
