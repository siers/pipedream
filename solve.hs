{-# LANGUAGE TupleSections, NamedFieldPuns, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- solver
import Control.Lens (view, _1, _2, _3)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import Control.Monad.Extra (allM)
import Control.Monad (join, mplus, filterM, void, mfilter)
import Control.Monad.Primitive (RealWorld)
import Data.Either.Extra (fromLeft, mapLeft)
import Data.Function (on)
import Data.List (sort, sortOn, find, uncons)
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Vector.Unboxed.Mutable (MVector)
import Debug.Trace (trace)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Printf (printf)
import System.IO.Unsafe

-- IO
import Network.Socket (withSocketsDo)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Environment
import Data.Foldable (traverse_)

t a = seq (trace (show a) a) a
t' l a = seq (trace (show l ++ ": " ++ show a) a) a

type Direction = Int -- top 0, right 1, bottom 2, left 3
type Rotation = Int
type Pix = [Direction]
type Cursor = (Int, Int)

-- PartId distinguishes the connected graphs (partitions) by their
-- smallest cursor (unique by def. ascending order)
type PartId = Cursor

-- Continue represents splits that are saved for later
-- Continues in progress may turn out to be already solved, which must be checked
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
  } deriving Show

type PieceDelta = (Piece, Cursor, Direction)

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
  show Continue{cursor, direct, origin} =
    "Continue " ++ show cursor ++ (if direct then "d" else "u") ++ " from " ++ show origin

instance Show Progress where
  show ps@Progress{iter, continues} =
    "Progress" ++ show (iter, length continues)

instance Show MMaze where
  show MMaze{depth} = "MMaze" ++ show (depth, ())

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
    colorHash = (+15) . (\(x, y) -> x * 67 + y * 23)
    color256 = (printf "\x1b[38;5;%im" . ([24 :: Int, 27..231] !!)) . (`mod` 70) . colorHash
    colorPart cur Piece{solved, partId} = if solved then Just (color256 $ partEquateUnsafe maze partId) else Nothing

    headSafe l = fst <$> uncons l
    colorHead cur = printf "\x1b[31m" <$ ((cur ==) `mfilter` (cursor <$> headSafe continues))
    colorContinues cur = printf "\x1b[32m" <$ find ((cur ==) . cursor) continues

    color :: Cursor -> Piece -> Maybe String
    color cur piece = colorHead cur `mplus` colorPart cur piece `mplus` colorContinues cur

    fmt idx piece@Piece{pipe} =
      printf $ fromMaybe (pipe : []) . fmap (\c -> printf "%s%c\x1b[39m" c pipe) $ color (mazeCursor maze idx) piece

render :: MMaze -> IO ()
render = (putStrLn =<<) . renderWithPositions . Progress 0 []

traceBoard :: Progress -> IO Progress
traceBoard progress@Progress{iter, maze=maze@MMaze{board, depth}} = do
  mode <- fromMaybe "" . lookup "trace" <$> getEnvironment
  tracer mode *> pure progress
  where
    freq = (mazeSize maze) `div` 10
    now = True -- iter `mod` freq == 0
    tracer mode -- reorder/comment out clauses to disable tracing
      | now && mode == "board" = traceStr >>= putStrLn
      | now && mode == "perc" = solvedStr >>= putStrLn
      | True = pure ()

    percentage = (fromIntegral $ depth) / (fromIntegral $ mazeSize maze)
    solvedStr = pure $ ("\x1b[2Ksolved: " ++ show (percentage * 100) ++ "%" ++ "\x1b[1A")

    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    -- traceStr = (clear ++) <$> renderWithPositions progress
    traceStr = renderWithPositions progress

mazeSolve :: MMaze -> Cursor -> Char -> IO MMaze
mazeSolve m@MMaze{board, width, locked, depth} cursor@(x, y) pipe = do
  board <- MV.modify board (\p -> p { pipe, solved = True }) (x + y * width)
  locked <- MV.write locked depth cursor
  pure $ m { depth = depth + 1 }

mazePop :: MMaze -> IO ()
mazePop m@MMaze{board, width, locked, depth} = do
  (x, y) <- MV.read locked depth
  void $ MV.modify board (\p -> p { solved = False }) (x + y * width)

mazeEquate :: MMaze -> Cursor -> Cursor -> IO ()
mazeEquate MMaze{board, width} partId (x, y) =
  MV.modify board (\p -> p {partId}) (x + y * width)

mazeRead :: MMaze -> Cursor -> IO Piece
mazeRead MMaze{board, width} (x, y) = MV.read board (x + y * width)

-- lookup fixed point (ish) in maze by PartId lookup, stops at first cycle
partEquate :: MMaze -> PartId -> IO PartId
partEquate maze@MMaze{board} v = loop' =<< find v
  where
    find v = (\Piece{solved, partId} -> if solved then partId else v) <$> mazeRead maze v
    loop' v' = (\found -> if v' == v || v' == found then pure v' else loop' found) =<< find v

partEquateUnsafe :: MMaze -> PartId -> PartId
partEquateUnsafe m p = unsafePerformIO $ partEquate m p

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

mazeDeltas :: MMaze -> Cursor -> IO [PieceDelta]
mazeDeltas m c =
  traverse (_1 (mazeRead m))
  . filter (matrixBounded m . (view _1))
  $ (\d -> (cursorDelta c d, cursorDelta c d, d)) `map` directions

mazeDeltasWalls :: MMaze -> Cursor -> IO [PieceDelta]
mazeDeltasWalls m c =
  traverse fetch $ (\d -> (cursorDelta c d, d)) `map` directions
  where
    fetch (c, d) =
      if matrixBounded m c
      then (, c, d) <$> mazeRead m c
      else pure (Piece ' ' True (0, 0), c, d)

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
  deltas <- mazeDeltasWalls maze cur
  pure $
    map (flip rotateChar this)
    . filter (flip all deltas . validateDirection this)
    $ chooseRotation this
  where
    chooseRotation :: Char -> Pix
    chooseRotation '╋' = [0]
    chooseRotation '┃' = [0,1]
    chooseRotation '━' = [0,1]
    chooseRotation _ = rotations

    validateDirection this rotation (Piece{pipe=that, solved}, _, direction) = do
      not solved || pixValid (this, that, rotation, direction)

cursorToContinue :: MMaze -> Pix -> PartId -> Int -> PieceDelta -> IO Continue
cursorToContinue maze pix origin created (_, c@(x, y), o) = do
  choices <- length <$> pixValidRotations maze c
  let direct = o `elem` pix
  let origin' = if direct then origin else c
  let created' = created + 1
  pure $ Continue c choices direct origin' created' (created' + choices * 5)

{- Solver -}

solveContinue :: Progress -> Continue -> IO Solution
solveContinue
  p@Progress{iter, maze=maze@MMaze{board, depth}, continues}
  c@Continue{cursor=cur, created, origin} =
    if depth == mazeSize maze
    then pure (Left maze)
    else do
      piece@Piece{pipe=this} <- mazeRead maze cur
      deltas <- mazeDeltas maze cur
      directDeltas <- pure . filter ((`elem` toPix this) . view _3) $ deltas
      dead <- dead piece directDeltas

      if dead
      then pure (Right [])
      else progress this deltas directDeltas >>= solve'

  where
    solvedM = fmap solved . mazeRead maze

    dead :: Piece -> [PieceDelta] -> IO Bool
    dead Piece{partId=thisPart} deltas = do
      allM id $ stuck : map discontinued continues
      where
        stuck = pure $ not . any (not . solved . (view _1)) $ deltas
        discontinued Continue{cursor=c, origin} =
          (\thatPart solved -> thisPart /= thatPart || solved) <$> partEquate maze c <*> solvedM c

    progress :: Char -> [PieceDelta] -> [PieceDelta] -> IO Progress
    progress this deltas directDeltas = do
      (origin':neighbours) <- sort . (++ [origin]) <$> traverse (partEquate maze . (view _2)) directDeltas
      mazeEquate maze origin' `traverse_` neighbours
      continues' <- continues' origin'
      pure $ Progress (iter + 1) continues' maze
      where
        continues' origin = t <$> do
          let deltas' = filter (not . solved . (view _1)) deltas
          next <- traverse (cursorToContinue maze (toPix this) origin created) deltas'
          dropWhileM (solvedM . cursor) . sortOn score $ next ++ continues

solve' :: Progress -> IO Solution
solve' Progress{continues=[]} = pure (Right [])
solve' progress@Progress{iter, maze, continues=(continue: continues)} = do
  rotateds <- pixValidRotations maze (cursor continue)
  solutions <- runExceptT . fmap join $ traverse solveCursor rotateds
  pure $ solutions
  where
    unwind = (<* ExceptT (return <$> mazePop maze))
    solveCursor this = unwind . ExceptT $ do
      maze <- mazeSolve maze (cursor continue) this
      traceBoard progress
      solveContinue progress { continues, maze } continue

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
  else mapM_ render =<< solve =<< parse =<< getContents
