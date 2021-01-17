{-# LANGUAGE TupleSections, NamedFieldPuns, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- solver
import Control.Lens (view, _1, _2, _3)
import Control.Monad.Extra (allM)
import Control.Monad (join, mplus, filterM)
import Control.Monad.Primitive (RealWorld)
import Data.Either.Extra (fromLeft, mapLeft)
import Data.Function (on)
import Data.List (sort, sortOn, elemIndex, find, uncons)
import Data.Map (Map, (!))
import Data.Matrix as Mx (Matrix)
import Data.Maybe (fromMaybe, fromJust)
import Data.Tuple (swap)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Vector.Unboxed.Mutable (MVector)
import Debug.Trace (trace)
import qualified Data.Map as Map
import qualified Data.Matrix as Mx
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
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
type Rotation = Int
type Pix = [Direction]
type Cursor = (Int, Int)

-- PartId distinguishes the connected graphs (partitions) by their
-- smallest cursor (unique by def. ascending order)
type PartId = Cursor

type MazeDelta = (Piece, Cursor, Direction)

data Continue = Continue
  { cursor :: Cursor
  , choices :: Int -- # of valid rotatonis
  , direct :: Bool -- directly pointed to from previous continue
  , origin :: PartId
  , created :: Int -- created at depth
  , score :: Int
  } deriving Eq

data Piece = Piece
  { pipe :: Char
  , solved :: Bool
  , partId :: PartId -- meaningless if not solved
  }

data MMaze = MMaze -- Mutable Maze
  { board :: MVector RealWorld Piece
  , width :: Int
  , height :: Int
  , locked :: MVector RealWorld Cursor -- stack with head at depth
  , depth :: Int -- recursion depth
  -- , choices -- [[Cursor]] -- language of choices left to explore -- maybe for fairness later on
  }

data Progress = Progress
  { iter :: Int -- the total number of backtracking iterations (incl. failed ones)
  , continues :: [Continue]
  , maze :: MMaze
  }

type Solution = Either MMaze [Progress]

derivingUnbox "Piece"
  [t| Piece -> (Char, Bool, PartId) |]
  [| \Piece{pipe, solved, partId} -> (pipe, solved, partId) |]
  [| \(c, s, p) -> Piece c s p |]

instance Show Continue where
  show Continue{cursor, direct} =
    "Cursor " ++ show cursor ++ if direct then "d" else "u"

instance Show Progress where
  show ps@Progress{iter, continues} =
    "Progress" ++ show (iter, length continues)

{- MMaze and Matrix operations -}

matrixBounded :: MMaze -> Cursor -> Bool
matrixBounded m (x, y) = x >= 0 && y >= 0 && width m > x && height m > y

mazeSize :: MMaze -> Int
mazeSize MMaze{width, height} = width * height

mazeLists :: Int -> Int -> V.Vector a -> [[a]]
mazeLists width height board = [ [ board V.! (x + y * width) | x <- [0..width - 1] ] | y <- [0..height - 1] ]

mazeCursor :: MMaze -> Int -> Cursor
mazeCursor MMaze{width} = swap . flip quotRem width

parse :: String -> IO MMaze
parse input = do
  locked <- MV.new . length . join $ rect
  chars <- UV.thaw . UV.fromList . map (\c -> Piece c False (0, 0)) . join $ rect
  pure $ MMaze chars (length (head rect)) (length rect) locked 0
  where rect = filter (not . null) $ lines input

renderWithPositions :: Progress -> IO String
renderWithPositions Progress{maze=maze@MMaze{board, width, height}, continues} =
  unlines . map concat . mazeLists width height . V.imap fmt . UV.convert <$> UV.freeze board
  where
    color256 = (printf "\x1b[38;5;%im" . ([24 :: Int, 27..231] !!)) . (`mod` 70) . colorHash
    colorHash = (+15) . (\(x, y) -> x * 67 + y * 23)
    colorPart cur Piece{solved, partId} = if solved then Just (color256 partId) else Nothing
      -- then color256 . lookupConverge partEquiv . snd <$> Map.lookup cur solveds

    colorSet cur = printf "\x1b[%sm" . fst <$> find (Set.member cur . snd)
      [ ("31", Set.fromList . map cursor $ (((:[]) . fst) `foldMap` uncons continues)) -- red
      , ("32", Set.fromList . map cursor $ continues) -- green
      ]

    color :: Cursor -> Piece -> Maybe String
    color cur piece = colorSet cur `mplus` colorPart cur piece

    fmt idx piece@Piece{pipe} =
      printf $ fromMaybe (pipe : []) . fmap (\c -> printf "%s%c\x1b[39m" c pipe) $ color (mazeCursor maze idx) piece

traceBoard :: Progress -> IO Progress
traceBoard progress@Progress{iter, maze=maze@MMaze{board, depth}} =
  tracer iter *> pure progress
  where
    freq = (mazeSize maze) `div` 10
    tracer iter -- reorder/comment out clauses to disable tracing
      | True = traceStr >>= putStrLn
      | depth == mazeSize maze - 1 = traceStr >>= putStrLn
      | iter `mod` freq == 0 = traceStr >>= putStrLn
      | iter `mod` freq == 0 = putStrLn solvedStr
      | True = pure ()

    percentage = (fromIntegral $ depth) / (fromIntegral $ mazeSize maze)
    solvedStr = ("\x1b[2Ksolved: " ++ show (percentage * 100) ++ "%" ++ "\x1b[1A")

    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    -- traceStr = (clear ++) <$> renderWithPositions progress
    traceStr = renderWithPositions progress

mazeSolve :: MMaze -> Cursor -> Piece -> IO MMaze
mazeSolve m@MMaze{board, width, locked, depth} cursor@(x, y) p = do
  board <- MV.write board (x + y * width) p
  locked <- MV.write locked (x + y * width) cursor
  pure $ m { depth = depth + 1 }

-- mazePop :: MMaze -> IO Maze
-- mazePop m@MMaze{board, depth} = do
--   locked <- MV.read (depth - 1)
--   board <

mazeEquate :: MMaze -> Cursor -> Cursor -> IO ()
mazeEquate MMaze{board, width} partId (x, y) =
  MV.modify board (\p -> p {partId}) (x + y * width)

mazeRead :: MMaze -> Cursor -> IO Piece
mazeRead MMaze{board, width, height} (x, y) = MV.read board (x + y * width)

-- lookup fixed point (ish) in maze by PartId lookup, stops at first cycle
partEquate :: MMaze -> PartId -> IO PartId
partEquate maze@MMaze{board} v = loop' =<< find v
  where
    find v = (\Piece{solved, partId} -> if solved then partId else v) <$> mazeRead maze v
    loop' v' = (\found -> if v' == v || v' == found then pure v' else loop' found) =<< find v

{- Generic functions -}

-- | Monadic 'dropWhile' from monad-loops package
dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ []     = return []
dropWhileM p (x:xs) = p x >>= \q -> if q then dropWhileM p xs else return (x:xs)

{- Model -}

directions = [0, 1, 2, 3]
rotations = directions

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

rotateDir :: Int -> Direction -> Direction
rotateDir n = (`mod` 4) . (+ n) -- C: n=1, CW: n=-1

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
cursorDeltaPieces :: MMaze -> Cursor -> IO [(Piece, Cursor, Direction)]
cursorDeltaPieces m c =
  traverse (_1 (mazeRead m))
  . filter (matrixBounded m . (view _1))
  $ (\d -> (cursorDelta c d, cursorDelta c d, d)) `map` directions

{- Solver bits -}

pixValid :: (Char, Char, Rotation, Direction) -> Bool
pixValid (this, that, rotation, direction) = satisfied thisRequires thatRequires
  where
    satisfied = (==) `on` filter (rotateDir 2 direction ==) :: Pix -> Pix -> Bool
    thisRequires = (rotation + 2) `rotate` toPix this :: Pix
    thatRequires = if that == ' ' then [] else toPix that :: Pix

pixValidRotations :: MMaze -> Cursor -> IO [Char]
pixValidRotations maze@MMaze{board} cur = do
  Piece{pipe=this} <- mazeRead maze cur
  rotation <- filterM (\r -> allM (checkDirection this r) directions) (chooseRotation this)
  pure $ map (flip rotateChar this) rotations
  where
    chooseRotation :: Char -> Pix
    chooseRotation '╋' = [0]
    chooseRotation '┃' = [0,1]
    chooseRotation '━' = [0,1]
    chooseRotation _ = rotations

    checkDirection this rotation direction = do
      -- TODO: refactor condition to ifless
      Piece{pipe=that, solved} <- if bounded then mazeRead maze delta else pure (Piece ' ' False (0, 0))
      pure $
        if not bounded || solved
        then pixValid (this, that, rotation, direction)
        else True
        where
          bounded = matrixBounded maze delta
          delta = cursorDelta cur direction

cursorToContinue :: MMaze -> Pix -> PartId -> Int -> (Piece, Cursor, Direction) -> IO Continue
cursorToContinue maze pix origin created (_, c@(x, y), o) = do
  choices <- length <$> pixValidRotations maze c
  let direct = o `elem` pix
  let origin' = if direct then origin else c
  let created' = created + 1
  pure $ Continue c choices direct origin' created' (created' + choices * 5)

{- Solver -}

solveRotation :: Progress -> Continue -> IO Solution
solveRotation
  Progress{iter, maze=maze@MMaze{board, depth}, continues}
  Continue{cursor=cur, created, origin} =
    if depth == mazeSize maze
    then pure (Left maze)
    else do
      Piece{pipe=this} <- mazeRead maze cur
      deltas <- cursorDeltaPieces maze cur
      directDeltas <- pure . filter ((`elem` toPix this) . view _3) $ deltas
      dead <- dead directDeltas

      if dead
      then pure (Right [])
      else progress this deltas directDeltas >>= traceBoard >>= solve'

  where
    solvedM = fmap solved . mazeRead maze

    dead :: [MazeDelta] -> IO Bool
    dead deltas = do
      -- continued = other unsolved continues exist
      continued <- (`allM` (map cursor continues)) $ \c -> do
        connected <- (==) <$> partEquate maze cur <*> partEquate maze c
        solved <- solvedM c
        pure $ not solved && connected
      let stuck = all (solved . (view _1)) $ deltas
      pure $ stuck && not continued -- refactor allM to enable shortcircuit

    progress :: Char -> [MazeDelta] -> [MazeDelta] -> IO Progress
    progress this deltas directDeltas = do
      (origin':neighbours) <- sort . (++ [origin]) <$> traverse (partEquate maze . (view _2)) directDeltas
      mazeEquate maze origin' `traverse_` neighbours
      continues' <- continues' origin'
      pure $ Progress (iter + 1) continues' maze
      where
        continues' origin = do
          let deltas' = filter (not . solved . (view _1)) deltas
          next <- traverse (cursorToContinue maze (toPix this) origin created) deltas'
          dropWhileM (solvedM . cursor) . sortOn score $ next ++ continues

solve' :: Progress -> IO Solution
solve' Progress{continues=[]} = pure (Right [])
solve' progress@Progress{iter, maze, continues=(continue: continues)} =
  fmap (fmap join . sequence) . traverse solve =<< pixValidRotations maze (cursor continue)
  where
    solve this = do
      maze <- mazeSolve maze (cursor continue) (Piece this True (origin continue))
      solveRotation progress { continues, maze } continue

solve :: MMaze -> IO [MMaze]
solve maze = do
  let progress = Progress 0 [Continue (0, 0) 0 True (0, 0) 0 0]
  return . fromLeft [] . mapLeft pure =<< solve' (progress maze)

{- Main -}

rotateStr = undefined
-- rotateStr' :: Maze -> Maze -> Text
-- rotateStr' = (concatenate .) . rotations
--   where
--     concatenate :: [(Rotation, Cursor)] -> Text
--     concatenate =
--       (T.pack "rotate " <>)
--       . T.intercalate (T.pack "\n")
--       . map (\(x, y) -> T.pack $ show x ++ " " ++ show y)
--       . (>>= (\(r, (x, y)) -> take r (repeat (x, y))))

--     rotations :: Maze -> Maze -> [(Rotation, Cursor)]
--     rotations input solved = Mx.toList . Mx.matrix (Mx.nrows input) (Mx.ncols input) $ (cursorRot >>= (,)) . undefined
--       where
--         cursorRot cur = (rotations `on` mxGetElem' cur) input solved
--         rotations from to = fromJust $ to `elemIndex` iterate (rotateChar 1) from

pļāpātArWebsocketu :: WS.ClientApp ()
pļāpātArWebsocketu conn = traverse_ solveLevel [1..6]
  where
    send = WS.sendTextData conn
    recv = T.unpack <$> WS.receiveData conn

    solveLevel level = do
      send (T.pack $ "new " ++ show level); recv

      send (T.pack "map")
      maze <- parse . T.unpack . T.drop 5 =<< WS.receiveData conn
      (solved: _) <- solve maze

      send (rotateStr maze solved); recv

      send (T.pack "verify")
      putStrLn =<< recv

main :: IO ()
main = do
  websocket <- (== "1") . fromMaybe "0" . lookup "websocket" <$> getEnvironment

  if websocket
  then withSocketsDo $ WS.runClient "maze.server.host" 80 "/game-pipes/" pļāpātArWebsocketu
  else fmap (const ()) . solve =<< parse =<< getContents
