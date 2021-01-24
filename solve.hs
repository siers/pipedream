{-# LANGUAGE TupleSections, NamedFieldPuns, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- solver

import Control.Lens ((&), (%~), (.~), set, over, view, _1, _2, _3)
import Control.Lens.TH
import Control.Monad.Extra (allM)
import Control.Monad (join, mplus, mfilter, filterM)
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import Data.Either.Extra (fromLeft, mapLeft)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (sort, sortOn, find, uncons, elemIndex)
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe, fromJust)
import Data.Tuple (swap)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Vector.Unboxed.Mutable (MVector)
import Debug.Trace (trace)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import System.IO.Unsafe
import Text.Printf (printf)

-- IO

import Data.Foldable (traverse_)
import Data.Text (Text)
import Network.Socket (withSocketsDo)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Environment

t a = seq (trace (show a) a) a
t' l a = seq (trace (show l ++ ": " ++ show a) a) a

type Direction = Int -- top 0, right 1, bottom 2, left 3
type Rotation = Int
type Pix = [Direction]
type Cursor = (Int, Int)

-- PartId distinguishes the connected graphs (partitions) by their
-- smallest cursor (unique by def. ascending order)
-- They're marked ahead of solveds, so Continue{direct=false} may already be connected.
type PartId = Cursor

-- Continue represents the piece that should be solved next
-- Continues in Progress may turn out to be already solved, which must be checked
data Continue = Continue
  { cursor :: Cursor
  , char :: Char -- defaulted to ' ', but set when guessing
  , choices :: Int -- # of valid rotations
  , direct :: Bool -- directly pointed to from previous continue
  , origin :: PartId
  , created :: Int -- created at depth
  , score :: Int
  } deriving Eq

data Piece = Piece
  { pipe :: Char
  , solved :: Bool
  , partId :: PartId -- meaningless if not connected
  , connected :: Bool -- connected when pointed
  } deriving Show

type PieceDelta = (Piece, Cursor, Direction)

data MMaze = MMaze -- Mutable Maze
  { board :: MVector RealWorld Piece
  , width :: Int
  , height :: Int
  , depth :: Int -- recursion depth
  }

type Unwind = (Cursor, [Cursor])

data Progress = Progress
  { iter :: Int -- the total number of backtracking iterations (incl. failed ones)
  , continues :: [Continue]
  , space :: [[(Continue, [Continue])]] -- unexplored solution space. enables parallelization. item per choice, per cursor.
  , unwinds :: [Unwind] -- history, essentially. an item per a solve. pop when (last space == [])
  , maze :: MMaze
  }

derivingUnbox "Piece"
  [t| Piece -> (Char, Bool, PartId, Bool) |]
  [| \Piece{pipe, solved, partId, connected} -> (pipe, solved, partId, connected) |]
  [| \(c, s, p, ct) -> Piece c s p ct |]

makeLensesFor ((\n -> (n, n ++ "L")) <$> ["cursor", "char", "choices", "direct", "origin", "created", "score"]) ''Continue
makeLensesFor ((\n -> (n, n ++ "L")) <$> ["iter", "continues", "space", "unwinds", "maze"]) ''Progress

instance Show Continue where
  show Continue{cursor, char, direct} =
    fold ["Continue ", [char], show cursor, (if direct then "d" else "u")]
    -- "Continue " ++ show cursor ++ (if direct then "d" else "u") ++ " from " ++ show origin

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
  chars <- UV.thaw . UV.fromList . map (\c -> Piece c False (0, 0) False) . join $ rect
  pure $ MMaze chars (length (head rect)) (length rect) 0
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
render = (putStrLn =<<) . renderWithPositions . Progress 0 [] [] []

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

mazeModify :: MMaze -> (Piece -> Piece) -> Cursor -> IO ()
mazeModify m@MMaze{board, width} f (x, y) = MV.modify board f (x + y * width)

mazeRead :: MMaze -> Cursor -> IO Piece
mazeRead MMaze{board, width} (x, y) = MV.read board (x + y * width)

cursorSolved :: MMaze -> Cursor -> IO Bool
cursorSolved maze = fmap solved . mazeRead maze

mazeSolve :: MMaze -> Continue -> IO Piece
mazeSolve m Continue{char, cursor} = do
  mazeModify m (\p -> p { pipe = char, solved = True, connected = True }) cursor
  mazeRead m cursor

mazeEquate :: MMaze -> Cursor -> Cursor -> IO ()
mazeEquate m partId = mazeModify m (\p -> p {partId})

mazePop :: MMaze -> Unwind -> IO ()
mazePop m@MMaze{board, width} (cursor, neighbours) = do
  mazeModify m (\p -> p { solved = False }) cursor
  (mazeModify m(\p -> p { connected = False })) `traverse_` neighbours

-- lookup fixed point (ish) in maze by PartId lookup, stops at first cycle
partEquate :: MMaze -> PartId -> IO PartId
partEquate maze@MMaze{board} v = loop' =<< find v
  where
    find v = (\Piece{connected, partId} -> if connected then partId else v) <$> mazeRead maze v
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
  traverse fetch . map (\d -> (cursorDelta c d, d)) $ directions
  where
    fetch (c, d) =
      if matrixBounded m c
      then (, c, d) <$> mazeRead m c
      else pure (Piece ' ' True (0, 0) True, c, d)

{- Solver bits -}

pixValid :: (Char, Char, Rotation, Direction) -> Bool
pixValid (this, that, rotation, direction) = satisfied thisRequires thatRequires
  where
    satisfied = (==) `on` filter (rotateDir 2 direction ==) :: Pix -> Pix -> Bool
    thisRequires = (rotation + 2) `rotate` toPix this :: Pix
    thatRequires = if that == ' ' then [] else toPix that :: Pix

pieceRotations :: MMaze -> Cursor -> IO [Char]
pieceRotations maze@MMaze{board} cur = do
  Piece{pipe=this} <- mazeRead maze cur
  deltas <- mazeDeltasWalls maze cur
  pure $ map (flip rotateChar this) . filter (validateRotation this deltas) $ directionMap this
  where
    directionMap '╋' = [0]
    directionMap '┃' = [0,1]
    directionMap '━' = [0,1]
    directionMap _ = rotations

    validateRotation this deltas = flip all deltas . validateDirection this

    validateDirection this rotation (Piece{pipe=that, solved}, _, direction) = do
      not solved || pixValid (this, that, rotation, direction)

cursorToContinue :: MMaze -> Continue -> PieceDelta -> IO Continue
cursorToContinue maze Continue{char, origin, created} (_, c@(x, y), direction) = do
  choices <- length <$> pieceRotations maze c
  let direct = direction `elem` toPix char
  let origin' = if direct then origin else c
  let created' = created + 1
  pure $ Continue c ' ' choices direct origin' created' (created' + choices * 5)

progressPop :: Progress -> IO Progress
progressPop p@Progress{maze, unwinds=(unwind:unwinds)} =
  p { unwinds } <$ mazePop maze unwind

pieceDead :: MMaze -> (Continue, [Continue]) -> IO Bool
pieceDead maze@MMaze{board, depth} (continue@Continue{cursor=cur, char=this}, continues) = do
  directDeltas <- filter ((`elem` toPix this) . view _3) <$> mazeDeltas maze cur
  (mazeSolve maze continue >>= dead directDeltas) <* mazePop maze (cur, [])
  where
    dead :: [PieceDelta] -> Piece -> IO Bool
    dead directDeltas Piece{partId=thisPart} = do
      allM id $ stuck : map discontinued continues
      where
        stuck = pure $ not . any (not . solved . (view _1)) $ directDeltas
        discontinued Continue{cursor=c, origin} =
          (\thatPart solved -> thisPart /= thatPart || solved) <$> partEquate maze c <*> cursorSolved maze c

{- Solver -}

-- Solves a piece; if valid, (mutates the maze; sets unwind) else (return progress without mutations)
solveContinue :: Progress -> Continue -> IO Progress
solveContinue
  progress@Progress{iter, maze=maze@MMaze{board, depth}, continues}
  continue@Continue{cursor=cur, char=this, created, origin} = do
    piece <- mazeSolve maze continue <* traceBoard progress
    deltas <- mazeDeltas maze cur
    directDeltas <- pure . filter ((`elem` toPix this) . view _3) $ deltas

    advance this deltas directDeltas
  where
    advance :: Char -> [PieceDelta] -> [PieceDelta] -> IO Progress
    advance this deltas directDeltas = do
      (origin':neighbours) <- sort . (++ [origin]) <$> traverse (partEquate maze . (view _2)) directDeltas
      mazeEquate maze origin' `traverse_` neighbours
      continuesNext <- continuesNextSorted origin'

      pure $ progress
        & iterL %~ (+1)
        & continuesL .~ continuesNext
        & unwindsL %~ ((cur, neighbours) :)
      where
        continuesNextSorted origin = do
          next <- traverse (cursorToContinue maze continue) . filter (not . solved . view _1) $ deltas
          dropWhileM (cursorSolved maze . cursor) . sortOn score $ next ++ continues

-- Solves pieces by backtracking, stops when maze is solved.
solve' :: Progress -> IO Progress
solve' Progress{continues=[]} = error "unlikely"
solve' progress@Progress{iter, maze, continues=(continue: continues)} = do
  rotations <- pieceRotations maze (cursor continue)
  guesses <- filterM (fmap not . pieceDead maze) . map ((, continues) . ($ continue) . set charL) $ rotations
  print (guesses, continues)
  progress' <- uncurry solveContinue =<< backtrack . (spaceL %~ (guesses :)) =<< pure progress
  if depth maze == mazeSize maze - 1
  then pure progress'
  else solve' progress'
  where
    backtrack :: Progress -> IO (Progress, Continue)
    backtrack Progress{space=[]} = error "unlikely" -- for solvable mazes
    backtrack p@Progress{space=([]:space)} = progressPop p { space } >>= backtrack
    backtrack p@Progress{space=(((continue, continues):guesses):space)} =
      pure (p { continues = continues, space = guesses : space }, continue)

solve :: MMaze -> IO [MMaze]
solve mmaze = do
  let progress = Progress 0 [Continue (0, 0) ' ' 0 True (0, 0) 0 0] [] []
  return . maze <$> solve' (progress mmaze)

{- Main -}

rotateStr :: MMaze -> MMaze -> IO Text
rotateStr input solved = concatenate <$> rotations input solved
  where
    concatenate :: [(Cursor, Rotation)] -> Text
    concatenate =
      (T.pack "rotate " <>)
      . T.intercalate (T.pack "\n")
      . map (\(x, y) -> T.pack $ show x ++ " " ++ show y)
      . (>>= (\((x, y), r) -> take r (repeat (x, y))))

    rotations :: MMaze -> MMaze -> IO [(Cursor, Rotation)]
    rotations maze@MMaze{board=input, width, height} MMaze{board=solved} = do
      zipped <- (UV.zip <$> UV.freeze input <*> UV.freeze solved)
      pure $ UV.toList . UV.imap (\i (Piece{pipe=p}, Piece{pipe=q}) -> (mazeCursor maze i, rotations p q)) $ zipped
      where
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
      maze <- parse . T.unpack . T.drop 5 =<< WS.receiveData conn

      send =<< rotateStr maze . head =<< solve maze
      recv

      send (T.pack "verify")
      putStrLn =<< recv

main :: IO ()
main = do
  websocket <- (== "1") . fromMaybe "0" . lookup "websocket" <$> getEnvironment

  if websocket
  then withSocketsDo $ WS.runClient "maze.server.host" 80 "/game-pipes/" pļāpātArWebsocketu
  else mapM_ render =<< solve =<< parse =<< getContents
