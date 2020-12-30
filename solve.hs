{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (join, foldM)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Bifunctor
import Data.Either.Extra (fromLeft, mapLeft)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List.Extra (groupSortOn)
import Data.List (sort, sortOn, elemIndex, uncons, concat, find, (\\), nub)
import Data.Map (Map, (!))
import Data.Matrix as Mx (Matrix, ncols, nrows)
import Data.Maybe (fromMaybe, fromJust, listToMaybe)
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
-- (# valid rotations, cursor, value, directly pointed)
type Continue = (Int, Cursor, Char, Bool)
-- scale, cursor (essentially a quadrant if you shrink cursor by scale)
type Constraint = (Int, Cursor)

data PartialSolution = PartialSolution
  { iter :: Int
  , maze :: Maze
  , continues :: [Continue]
  , solveds :: CursorSet
  , constraints :: [Constraint] }

instance Show PartialSolution where
  show ps@PartialSolution{iter=iter, continues=continues, constraints=constraints} =
    "PartialSolution" ++ show (iter, if constrained ps then "dead" else "active", length continues, constraints)

constrained :: PartialSolution -> Bool
constrained ps@PartialSolution{continues=continues, constraints=constraints} =
  all (not . constraintBorderMet (maze ps) constraints . sel2) $ continues

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
  . Mx.mapPos (\(y, x) c -> fmt (x - 1, y - 1) (c : []))
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
rotateChar r = mapPix . rotate r .mapChar

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

quadrantShrink :: Int -> Constraint -> Constraint
quadrantShrink scale (s, cur) = (s * scale, cursorShrink scale cur)

constraintMet :: [Constraint] -> Cursor -> Bool
constraintMet constr (x, y) = flip all constr $ \(scale, quad) -> quad == cursorShrink scale (x, y)
  -- Mx.matrix 20 20 (constraintMet [(2, (3,2))] . (\(y, x) -> (x - 1, y - 1)))
  -- Mx.matrix 20 20 (constraintMet [(4, (0,0))] . (\(y, x) -> (x - 1, y - 1)))

-- this computes constraints by quadrants for divide and conquer excluding borders unless joined
-- this is way too complicated, but it works and isn't the reason the solver can't solve
-- see matrix outputs in comments under body to figure out what it does
constraintBorderMet :: Matrix a -> [Constraint] -> Cursor -> Bool
constraintBorderMet m constrs (x, y) = flip any constrs $ \constr@(scale, quad) ->
  let
    borderX = ((x + overflowX) `mod` scale < 2) && (not $ x == 0 || x + 1 == ncols m)
    borderY = ((y + overflowY) `mod` scale < 2) && (not $ y == 0 || y + 1 == nrows m)
    friendlyAt (dx, dy) = constraintBorderMet m (constrs \\ [constr]) (x + dx, y + dy)
    overflowX = if friendlyAt (3, 0) then 0 else if friendlyAt (-3, 0) then -2 else 1
    overflowY = if friendlyAt (0, 3) then 0 else if friendlyAt (0, -3) then -2 else 1
  in
    (quad == cursorShrink scale (x, y)) && (not borderX && not borderY)
  -- m = Mx.matrix 20 20 (const 0)
  -- Mx.matrix 20 20 (constraintBorderMet m [(4, (0,0)), (4, (2,0)), (4, (3,1)), (4, (2,1))] . (\(y, x) -> (x - 1, y - 1)))

--

pixValid :: PixCheck -> Bool
pixValid (this, that, rotation, direction) =
  filter (flipDir direction ==) thisRequires == filter (flipDir direction ==) thatRequires
    where
      thisRequires :: Pix
      thisRequires = (rotation + 2) `rotate` mapChar this

      thatRequires :: Pix
      thatRequires = if that == ' ' then [] else mapChar that

pixValidRotations :: PixValidPrecomp -> Maze -> CursorSet -> Cursor -> Char -> Pix
pixValidRotations pixValidP maze solveds cur this =
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

--

sortContinues :: Maze -> [Constraint] -> [Continue] -> [Continue]
sortContinues maze constraints = sortOn (\c -> (not . constraintBorderMet maze constraints $ sel2 c, sel1 c))

joinSolutions :: PartialSolution -> PartialSolution -> PartialSolution
joinSolutions a b =
  PartialSolution
    (iter a + iter b)
    (matrixCopy (constraintMet (constraints b)) (maze a) (maze b))
    (sortContinues (maze a) constraints' $ continues a ++ continues b)
    (solveds a `Set.union` solveds b)
    constraints'
  where
    constraints' = (constraints a ++ constraints b)

widenSolution :: Int -> PartialSolution -> PartialSolution
widenSolution scale ps = ps { constraints = nub . map (quadrantShrink scale) $ constraints ps }

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

solve :: PixValidPrecomp -> RotatePrecomp -> Maze -> [Maze]
solve pixValidP rotP maze = fromLeft [] . mapLeft pure . solve' $ [simplestPSolution]
  where
    initialContinue :: Cursor -> Continue
    initialContinue c = (0, c, uncurry mxGetElem c maze, True)

    simplestPSolution = PartialSolution 0 maze [(initialContinue (1, 1))] Set.empty [(4, (1, 1))]

    quadrantSolutions =
      filter (not . constrained)
      . map (\cursors@(head:_) ->
        PartialSolution 0 maze (initialContinue `map` cursors) Set.empty [quadrantShrink 4 (1, head)])
      . groupSortOn (cursorShrink 4 >>= (,))
      $ initialSet maze

    solve' = solveDC
    -- solve' = solveBT

    -- divide and conquer by combining quadrants + backtracking
    solveDC :: [PartialSolution] -> Either Maze [PartialSolution]
    solveDC [] = Right []
    solveDC psolutions = do
      solved <- sequence $ psolutions >>= solve'' (-1)
      combined <- (map (widenSolution 2) <$>) . combine $ group solved
      solve' combined
      where
        group :: [PartialSolution] -> [[PartialSolution]]
        group = groupSortOn (map (quadrantShrink 2) . constraints)

        combine :: [[PartialSolution]] -> Either Maze [PartialSolution]
        combine = (fmap join . ) . traverse $
          foldM1 combinePsolves
          . (\x -> trace (show (map length x)) x)
          . groupSortOn constraints

        combinePsolves :: [PartialSolution] -> [PartialSolution] -> Either Maze [PartialSolution]
        combinePsolves as bs = sequence $ do
          a <- as
          b <- bs
          solve'' (-1) $ joinSolutions a b

    -- pure backtracking
    solveBT :: [PartialSolution] -> Either Maze [PartialSolution]
    solveBT [] = Right []
    solveBT ps = do
      ps <- sequence $ (map traceBoard ps) >>= solve'' (-1)
      solveBT . map (widenSolution 2) $ ps

    solve'' :: Int -> PartialSolution -> [Either Maze PartialSolution]
    solve'' _ PartialSolution{continues=[]} = []
    solve'' lifespan progress@PartialSolution{maze=maze', continues=((_, cur, this, _): continues), solveds=solveds', constraints=constraints} =
      iterGuard $ do
        let rotations = pixValidRotations pixValidP maze' solveds' cur this
        join . parMap rpar (\r -> solveRotation (rotP #! (this, r))) $ rotations

      where
        constraintViolated maze cur = not . constraintBorderMet maze constraints $ cur

        iterGuard compute =
          if constraintViolated maze cur
          then [Right progress]
          else if lifespan == 0
            then [Right progress]
            else compute

        solveRotation :: Char -> [Either Maze PartialSolution]
        solveRotation rotated =
          if Set.size solveds == matrixSize maze
          then [Left maze]
          else solve'' (lifespan - 1) nextSolution

          where
            nextSolution :: PartialSolution
            nextSolution = traceBoard $
              PartialSolution (iter progress + 1) maze continues' solveds constraints

            nRotations :: Maze -> Cursor -> Bool -> Int
            nRotations maze c ambig =
              if not ambig
              then 0
              else length $ pixValidRotations' pixValidP maze solveds c

            cursorToContinue :: Pix -> Maze -> (Cursor, Direction) -> Continue
            cursorToContinue pix maze (c@(x, y), o) = (nRotations maze c True, c, char, direct)
              where
                char = mxGetElem x y maze
                direct = o `elem` pix

            next :: [Continue]
            next =
              filter (\(choices, _, _, d) -> choices < 2 || d)
              . map (cursorToContinue (mapChar rotated) maze)
              . filter (not . (`Set.member` solveds) . fst)
              $ cursorDeltasSafe maze cur directions

            continues' = ((`Set.member` solveds) . sel2) `dropWhile` (sortContinues maze constraints $ next ++ continues)
            solveds = cur `Set.insert` solveds'
            maze = mxSetElem rotated cur maze'

traceBoard :: PartialSolution -> PartialSolution
traceBoard progress@PartialSolution{continues=[]} = progress
traceBoard progress@PartialSolution{iter=iter, maze=maze, continues=((_, cur, _, _): continues), solveds=solveds} =
  tracer iter progress
  where
    tracer iter -- reorder clauses to disable tracing
      -- | True = trace traceStr
      | iter `mod` 200 == 0 = trace traceStr
      | iter `mod` 200 == 0 = trace solvedStr
      | True = id

    percentage = (fromIntegral $ Set.size solveds) / (fromIntegral $ matrixSize maze)
    solvedStr = ("\x1b[2Ksolved: " ++ show percentage ++ "%" ++ "\x1b[1A")
    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    -- traceStr = show progress ++ "\n" ++ renderWithPositions positions maze
    traceStr = clear ++ renderWithPositions positions maze
    -- traceStr = renderWithPositions positions maze
    -- traceStr = clear ++ render maze -- cheap
    contFast = map sel2 . filter ((== 1) . sel1) $ continues
    contSlow = map sel2 . filter ((>= 2) . sel1) $ continues
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

main :: IO ()
main = do
  pixValidPrecomp <- pure pixValidPrecomputed
  rotatePrecomp <- pure rotatePrecomputed

  input <- parse <$> getContents
  solveds <- pure . solve pixValidPrecomp rotatePrecomp $ input

  -- mapM_ (putStrLn . printRot . computeRotations input) $ solveds
  mapM_ (putStrLn . render) $ solveds
  -- mapM_ (const (pure ()) . render) $ solveds
