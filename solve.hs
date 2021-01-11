{-# LANGUAGE TupleSections, NamedFieldPuns #-}

module Main where

-- solver
import Control.Monad (join, mplus)
import Data.Either.Extra (fromLeft, mapLeft)
import Data.Function (on)
import Data.List (sort, sortOn, elemIndex, find)
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

-- IO
import Data.Text (Text)
import Network.Socket (withSocketsDo)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Environment
import Data.Foldable (traverse_)

t a = trace (show a) a

type Direction = Int -- top 0, right 1, bottom 2, left 3
type Pix = [Direction]
type Maze = Matrix Char

type Cursor = (Int, Int)
type Rotation = Int

-- PartId distinguishes the connected graphs (partitions) by their smallest cursor (by def. ascending order)
type PartId = Cursor
type PartEquiv = Map PartId PartId
type Solveds = Map Cursor (Char, PartId)

-- (# valid rotations, cursor, value, directly pointed)
data Continue = Continue
  { cursor :: Cursor
  , cchar :: Char
  , choices :: Int
  , direct :: Bool
  , origin :: PartId
  , created :: Int } deriving Eq -- created at iter

data Progress = Progress
  { iter :: Int
  , maze :: Maze
  , continues :: [Continue]
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

to0Cursor (y, x) = (x - 1, y - 1)

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
  . Mx.mapPos (\cur1 c -> fmt (to0Cursor cur1) ((getMazeElem maze solveds (to0Cursor cur1)) : []))
  $ maze
  where
    color256 = (printf "\x1b[38;5;%im" . ([24 :: Int, 27..231] !!)) . (`mod` 70) . colorHash :: Cursor -> String
    colorHash = (+15) . (\(x, y) -> x * 67 + y * 23)
    colorPart cur = color256 . lookupConverge partEquiv . snd <$> Map.lookup cur solveds

    colorSet cur = printf "\x1b[%sm" . fst <$> find (Set.member cur . snd) coloredSets

    color cur = colorSet cur `mplus` colorPart cur
    fmt cur s = printf $ fromMaybe s . fmap (\c -> printf "%s%s\x1b[39m" c s) $ color cur

-- C: n=1, CW: n=-1
rotateDir :: Int -> Direction -> Direction
rotateDir n = (`mod` 4) . (+ n)

flipDir = rotateDir 2

rotate :: Rotation -> Pix -> Pix
rotate r = map (rotateDir r)

rotateChar :: Rotation -> Char -> Char
rotateChar r = toChar . rotate r . toPix

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

-- get diff from Solveds otherwise from Maze
getMazeElem :: Maze -> Solveds -> Cursor -> Char
getMazeElem maze solveds cur = mxGetElem' cur maze `fromMaybe` (fst <$> Map.lookup cur solveds)

setMazeElems :: Maze -> Solveds -> Maze
setMazeElems maze solveds = Mx.mapPos ((getMazeElem maze solveds . ) . (const . to0Cursor)) maze

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
    this = getMazeElem maze solveds cur

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
          char = if bounded then getMazeElem maze solveds curDelta else ' '

cursorToContinue :: Maze -> Solveds -> Pix -> PartId -> (Cursor, Direction) -> Continue
cursorToContinue maze solveds pix origin (c@(x, y), o) = Continue c char (nRotations maze c) direct origin' 0
  where
    char = getMazeElem maze solveds c
    direct = o `elem` pix
    origin' = if direct then origin else c

    nRotations :: Maze -> Cursor -> Int
    nRotations maze c = length $ pixValidRotations maze solveds c

sortContinues :: Progress -> [Continue] -> [Continue]
sortContinues p cs = sortOn score cs
  where
    score c = created c + (choices c) * 5
    -- score c = created c + (choices c) * 5 -- + (if direct c then 1 else 0)

--

traceBoard :: Progress -> Progress
traceBoard progress@Progress{continues=[]} = progress
traceBoard progress@Progress{iter, maze, continues=(Continue{cursor=cur}: continues), solveds, partEquiv} =
  tracer iter progress
  where
    freq = (matrixSize maze) `div` 10
    tracer iter -- reorder/comment out clauses to disable tracing
      --  | Map.size solveds == matrixSize maze - 1 = trace traceStr
      --  | True = trace traceStr
      | iter `mod` freq == 0 = trace solvedStr
      | iter `mod` freq == 0 = trace traceStr
      | True = id

    percentage = (fromIntegral $ Map.size solveds) / (fromIntegral $ matrixSize maze)
    solvedStr = ("\x1b[2Ksolved: " ++ show (percentage * 100) ++ "%" ++ "\x1b[1A")
    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    traceStr = clear ++ renderWithPositions solveds partEquiv positions maze
    -- traceStr = renderWithPositions solveds partEquiv positions maze
    -- traceStr = clear ++ render maze -- cheap
    positions =
      [ ("31", Set.singleton cur) -- red
      , ("31", Set.fromList $ map cursor continues) -- green
      ]

solveRotation :: Progress -> Continue -> Solution
solveRotation
  Progress{iter, maze, continues, solveds, partEquiv}
  Continue{cursor=cur, cchar=this, created, origin} =
    if Map.size solveds == matrixSize maze
    then Left (setMazeElems maze solveds)
    else
      if dead
        then Right []
        else solve' . traceBoard $ progress

  where
    progress = progressRaw { continues = dropBad $ sortContinues progressRaw continues' }
    progressRaw = Progress (iter + 1) maze continues' solveds partEquiv'

    dropBad = dropWhile ((`Map.member` solveds) . cursor)
    continues' = next ++ continues

    next :: [Continue]
    next =
      map (\c -> c { created = created + 1 })
      . map (cursorToContinue maze solveds (toPix this) origin')
      . filter (not . (`Map.member` solveds) . fst)
      $ cursorDeltasSafe maze cur directions

    partEquate = lookupConverge partEquiv

    dead = null directed && null hope
      where
        directed = dropBad $ filter (\c -> direct c) next
        hope = dropBad $ filter ((partEquate cur ==) . partEquate . cursor) continues

    neighbours = map (partEquate . fst) $ cursorDeltasSafe maze cur (toPix this)
    (origin':neighbours') = sort $ neighbours ++ [origin]

    partEquiv' = foldr (uncurry Map.insert) partEquiv $ (, origin') <$> neighbours'

solve' :: Progress -> Solution
solve' Progress{continues=[]} = Right []
solve' progress@Progress{maze=maze, continues=(continue: continues), solveds=solveds} =
  fmap join . traverse (solveRotation' . flip rotateChar (cchar continue)) $
    pixValidRotations maze solveds (cursor continue)
  where
    solveRotation' rotated =
      solveRotation
        progress
          { continues = continues
          , solveds = Map.insert (cursor continue) (rotated, origin continue) solveds
          }
        continue { cchar = rotated }

solve :: Maze -> [Maze]
solve maze =
  fromLeft [] . mapLeft pure . solve'
  $ Progress 0 maze [continue (0, 0)] Map.empty Map.empty
  where continue c = Continue c (uncurry mxGetElem c maze) 0 True (0, 0) 0

--

rotateStr :: Maze -> Maze -> Text
rotateStr = (concatenate .) . rotations
  where
    concatenate :: [(Rotation, Cursor)] -> Text
    concatenate =
      (T.pack "rotate " <>)
      . T.intercalate (T.pack "\n")
      . map (\(x, y) -> T.pack $ show x ++ " " ++ show y)
      . (>>= (\(r, (x, y)) -> take r (repeat (x, y))))

    rotations :: Maze -> Maze -> [(Rotation, Cursor)]
    rotations input solved = Mx.toList . Mx.matrix (nrows input) (ncols input) $ (cursorRot >>= (,)) . to0Cursor
      where
        cursorRot cur = (rotations `on` mxGetElem' cur) input solved
        rotations from to = fromJust $ to `elemIndex` iterate (rotateChar 1) from

pļāpātArWebsocketu :: WS.ClientApp ()
pļāpātArWebsocketu conn = traverse_ solveLevel [1..6]
  where
    send = WS.sendTextData conn
    recv = T.unpack <$> WS.receiveData conn

    solveLevel level = do
      send (T.pack $ "new " ++ show level)
      recv

      send (T.pack "map")
      maze <- parse . T.unpack . T.drop 5 <$> WS.receiveData conn
      send (rotateStr maze (head $ solve maze))
      recv

      send (T.pack "verify")
      putStrLn =<< recv

main :: IO ()
main = do
  websocket <- (== "1") . fromMaybe "0" . lookup "websocket" <$> getEnvironment

  if websocket
  then withSocketsDo $ WS.runClient "maze.server.host" 80 "/game-pipes/" pļāpātArWebsocketu
  else (`seq` (pure ())) . solve . parse =<< getContents
