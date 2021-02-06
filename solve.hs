{-# LANGUAGE TupleSections, NamedFieldPuns, TemplateHaskell, BinaryLiterals, StrictData, BangPatterns #-}

module Main where

-- solver

import Control.Lens ((&), (%~), (.~), set, view, _1, _2)
import Control.Lens.TH
import Control.Monad.Extra (allM, andM, ifM, whenM)
import Control.Monad (join, mplus, mfilter, filterM, void)
import Control.Monad.Primitive (RealWorld)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (find, elemIndex, foldl')
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.Set (Set)
import Data.Tuple (swap)
import Data.Vector.Mutable (MVector)
import Data.Word (Word8)
import Debug.Trace (trace)
import qualified Data.Bits as Bit
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.Environment (getEnv, lookupEnv, setEnv, getArgs)
import System.IO.Unsafe
import Text.Printf (printf)

-- IO

import Data.Foldable (traverse_)
import Data.Text (Text)
import Network.Socket (withSocketsDo)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

t a = seq (trace (show a) a) a
t' l a = seq (trace (show l ++ ": " ++ show a) a) a
tM :: (Functor f, Show a) => f a -> f a
tM = fmap t

type Direction = Int -- top 0, right 1, bottom 2, left 3
type Rotation = Int
type Pix = Word8 -- directions as exponents of 2, packed in a byte, mirrored in (shiftL 4) to help bit rotation
type Cursor = (Int, Int)

-- PartId distinguishes the connected graphs (partitions) by their
-- smallest cursor (unique by def. ascending order)
-- They're marked ahead of solveds, so Continue{direct=false} may already be connected.
type PartId = Cursor

data Piece = Piece
  { pipe :: Pix
  , solved :: Bool
  , partId :: PartId -- meaningless if not connected
  , connected :: Bool -- connected when pointed
  } deriving Show

type PieceDelta = (Piece, Cursor, Direction)

data MMaze = MMaze -- Mutable Maze
  { board :: MVector RealWorld Piece
  , width :: Int
  , height :: Int
  }

-- Continue represents the piece that should be solved next
-- Continues in Progress may turn out to be already solved, which must be checked
data Continue = Continue
  { cursor :: Cursor
  , char :: Pix -- defaulted to ' ', but set when guessing
  , origin :: PartId
  , choices :: Int --- number of possible rotations without clashes the surrounding solved pieces
  , score :: Int
  }

type Unwind1 = (Cursor, Piece)
type Unwind = [Unwind1]

data Progress = Progress
  { iter :: Int -- the total number of backtracking iterations (incl. failed ones)
  , depth :: Int -- number of solves, so also the length of unwinds/space
  , continues :: Set Continue -- priority queue for next guesses
  , space :: [[(Continue, Set Continue)]] -- unexplored solution space. enables parallelization. item per choice, per cursor.
  , unwinds :: [Unwind] -- history, essentially. an item per a solve. pop when (last space == [])
  , maze :: MMaze
  }

makeLensesFor ((\n -> (n, n ++ "L")) <$> ["cursor", "char", "choices", "origin", "score"]) ''Continue
makeLensesFor ((\n -> (n, n ++ "L")) <$> ["iter", "depth", "continues", "space", "unwinds", "maze"]) ''Progress

instance Eq Continue where
  Continue{cursor=ca, score=sa} == Continue{cursor=cb, score=sb} = ca == cb && sa == sb

instance Ord Continue where
  Continue{cursor=ca, score=sa} <= Continue{cursor=cb, score=sb} = sa <= sb --  || ca == cb

instance Show Continue where
  show Continue{cursor, char, origin} =
    fold ["Continue ", filter (/= ' ') [toChar char], show cursor, " of ", show origin]

instance Show Progress where
  show ps@Progress{iter, continues} =
    "Progress" ++ show (iter, length continues)

instance Show MMaze where
  show _ = "MMaze"

{--- Generic functions ---}

-- | Monadic 'dropWhile' from monad-loops package
dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ []     = return []
dropWhileM p (x:xs) = p x >>= \q -> if q then dropWhileM p xs else return (x:xs)

{--- MMaze and Matrix operations ---}

parse :: String -> IO MMaze
parse input = do
  board <- V.thaw . V.fromList . map (\c -> Piece c False (0, 0) False) . join $ rect
  maze <- pure (MMaze board (length (head rect)) (length rect))
  maze <$ mazeMap maze (\c p -> p { partId = c })
  where rect = filter (not . null) . map (map toPix) . lines $ input

mazeStore :: MMaze -> String -> IO ()
mazeStore m label = writeFile label =<< renderStr m

mazeBounded :: MMaze -> Cursor -> Bool
mazeBounded MMaze{width, height} (!x, !y) = x >= 0 && y >= 0 && width > x && height > y

{-# INLINE mazeSize #-}
mazeSize :: MMaze -> Int
mazeSize MMaze{width, height} = width * height

vectorLists :: Int -> Int -> V.Vector a -> [[a]]
vectorLists width height board = [ [ board V.! (x + y * width) | x <- [0..width - 1] ] | y <- [0..height - 1] ]

mazeLists :: MMaze -> IO [[Piece]]
mazeLists MMaze{board, width, height} = vectorLists width height . V.convert <$> V.freeze board

mazeCursor :: MMaze -> Int -> Cursor
mazeCursor MMaze{width} = swap . flip quotRem width

{-# INLINE mazeRead #-}
mazeRead :: MMaze -> Cursor -> IO Piece
mazeRead MMaze{board, width} (x, y) = MV.unsafeRead board (x + y * width)

{-# INLINE mazeCursorSolved #-}
mazeCursorSolved :: MMaze -> Cursor -> IO Bool
mazeCursorSolved MMaze{board, width} (x, y) = solved <$> MV.unsafeRead board (x + y * width)

mazeModify :: MMaze -> (Piece -> Piece) -> Cursor -> IO ()
mazeModify m@MMaze{board, width} f (x, y) = MV.unsafeModify board f (x + y * width)

mazeMap :: MMaze -> (Cursor -> Piece -> Piece) -> IO ()
mazeMap m@MMaze{board, width, height} f =
  void . traverse (\cursor -> mazeModify m (f cursor) cursor) . join $
    [ [ (x, y) | x <- [0..width - 1] ] | y <- [0..height - 1] ]

mazeClone :: MMaze -> IO MMaze
mazeClone m@MMaze{board} = (\board -> m { board }) <$> MV.clone board

mazeSolve :: MMaze -> Continue -> IO Unwind1
mazeSolve m Continue{char, cursor} =
  (cursor, ) <$> mazeRead m cursor <* mazeModify m (\p -> p { pipe = char, solved = True }) cursor

mazeEquate :: MMaze -> PartId -> [Cursor] -> IO Unwind
mazeEquate m partId cursors =
  traverse (\c -> (c, ) <$> mazeRead m c) cursors
  <* traverse (mazeModify m (\p -> p { partId, connected = True })) cursors

mazePop :: MMaze -> Unwind -> IO ()
mazePop m@MMaze{board, width} unwind = do
  uncurry (flip (mazeModify m . const)) `traverse_` unwind

-- lookup fixed point (ish) in maze by PartId lookup, stops at first cycle
partEquate :: MMaze -> PartId -> IO PartId
partEquate maze@MMaze{board} v = loop' =<< find v
  where
    find f = (\Piece{connected, partId} -> if connected then partId else f) <$> mazeRead maze f
    loop' v' = (\found -> if v' == v || v' == found then pure v' else loop' found) =<< find v'

partEquateUnsafe :: MMaze -> PartId -> PartId
partEquateUnsafe m p = unsafePerformIO $ partEquate m p

{--- Rendering, tracing ---}

renderWithPositions :: Maybe Continue -> Progress -> IO String
renderWithPositions current Progress{depth, maze=maze@MMaze{board, width, height}, continues} =
  unlines . map concat . vectorLists width height . V.imap fmt . V.convert <$> V.freeze board
  where
    colorHash = (+15) . (\(x, y) -> x * 67 + y * 23)

    color256 = (printf "\x1b[38;5;%im" . ([24 :: Int, 27..231] !!)) . (`mod` 70) . colorHash :: Cursor -> String
    colorPart cur Piece{solved, partId} = if solved then Just (color256 $ partEquateUnsafe maze partId) else Nothing

    -- color256 = (printf "\x1b[38;5;%im" . (\c -> if c == 15 then 8 :: Int else 9)) . colorHash :: Cursor -> String
    -- colorPart cur Piece{solved, partId} = if solved then Just (color256 $ partId) else Nothing

    colorHead cur = printf "\x1b[31m" <$ ((cur ==) `mfilter` (cursor <$> current))
    colorContinues cur = printf "\x1b[32m" <$ find ((cur ==) . cursor) continues
    -- colorContinues cur = color256 . origin <$> find ((cur ==) . cursor) continues

    color :: Cursor -> Piece -> Maybe String
    -- color cur piece = colorHead cur `mplus` colorContinues cur
    color cur piece = colorHead cur `mplus` colorPart cur piece `mplus` colorContinues cur

    fmt idx piece@Piece{pipe} =
      printf $ fromMaybe (toChar pipe : []) . fmap (\c -> printf "%s%c\x1b[39m" c (toChar pipe)) $ color (mazeCursor maze idx) piece

render :: MMaze -> IO ()
render = (putStrLn =<<) . renderWithPositions Nothing . Progress 0 0 S.empty [] []

renderStr :: MMaze -> IO String
renderStr MMaze{board, width, height} = do
  unlines . map concat . vectorLists width height . V.map (return . toChar . pipe) . V.convert
  <$> V.freeze board

traceBoard :: Continue -> Progress -> IO Progress
traceBoard current progress@Progress{iter, depth, maze=maze@MMaze{board}} = do
  mode <- getEnv "trace"
  freq <- (read :: String -> Int) <$> getEnv "freq"
  tracer mode freq *> pure progress
  where
    tracer mode freq -- reorder/comment out clauses to disable tracing
      | iter `mod` freq == 0 && mode == "board" = traceStr >>= putStrLn
      | iter `mod` freq == 0 && mode == "perc" = solvedStr >>= putStrLn
      | True = pure ()

    percentage = (fromIntegral $ depth) / (fromIntegral $ mazeSize maze)
    solvedStr = pure $ ("\x1b[2Ksolved: " ++ show (percentage * 100) ++ "%" ++ "\x1b[1A")

    stats = (show iter ++ "/" ++ show depth ++ " at " ++ show (cursor current) ++ "\n")
    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    traceStr = (clear ++) <$> renderWithPositions (Just current) progress
    -- traceStr = (stats ++) <$> renderWithPositions (Just current) progress
    -- traceStr = renderWithPositions (Just current) progress
    -- traceStr = renderWithPositions progress

-- faster, because no env lookups, but just by a little
traceProgress :: Progress -> IO Progress
traceProgress p@Progress{iter, depth, maze} = do
  if iter `mod` 1000 == 0 then putStrLn solvedStr else pure ()
  pure p
  where
    percentage = (fromIntegral $ depth) / (fromIntegral $ mazeSize maze)
    solvedStr = (printf "\x1b[2Ksolved: %0.2f%%\x1b[1A" (percentage * 100 :: Double))

{--- Model ---}

directions = [0, 1, 2, 3]
rotations = directions

(pixWall, pixUnset) = (0, 0)
charMapEntries = map (_2 %~ (byteify . bitify))
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
  , (' ', []) -- chars outside the map
  ]
  where
    bitify = foldr (\exp sum -> 2 ^ exp + sum) 0 . map fromIntegral :: [Direction] -> Pix
    byteify = (\bitified -> bitified + Bit.shiftL bitified 4) :: Pix -> Pix

charMap :: Map Char Pix
charMap = Map.fromList charMapEntries

pixMap :: Map Pix Char
pixMap = Map.fromList $ map swap charMapEntries

pixRotations :: Pix -> [Rotation]
pixRotations 0b11111111 = [0]
pixRotations 0b10101010 = [0,1]
pixRotations 0b01010101 = [0,1]
pixRotations _ = rotations

pixDirections :: Pix -> [Direction]
pixDirections n = fold
  [ if n `Bit.testBit` 0 then [0] else []
  , if n `Bit.testBit` 1 then [1] else []
  , if n `Bit.testBit` 2 then [2] else []
  , if n `Bit.testBit` 3 then [3] else []
  ]

toPix = (charMap !) :: Char -> Pix
toChar = (pixMap !) :: Pix -> Char

rotate :: Rotation -> Pix -> Pix
rotate = flip Bit.rotateL

cursorDelta :: Cursor -> Direction -> Cursor
cursorDelta (x, y) 0 = (x, y - 1)
cursorDelta (x, y) 1 = (x + 1, y)
cursorDelta (x, y) 2 = (x, y + 1)
cursorDelta (x, y) 3 = (x - 1, y)
cursorDelta _ _      = error "only defined for 4 directions"

mazeDeltas :: MMaze -> Cursor -> [Direction] -> IO [PieceDelta]
mazeDeltas m c directions =
  traverse (_1 (mazeRead m))
  . filter (mazeBounded m . (view _1))
  $ (\d -> (cursorDelta c d, cursorDelta c d, d)) `map` directions

mazeDeltasWalls :: MMaze -> Cursor -> IO [(Piece, Direction)]
mazeDeltasWalls m c = traverse (mazeDeltaWall m c) directions

{-# INLINE mazeDeltaWall #-}
mazeDeltaWall :: MMaze -> Cursor -> Direction -> IO (Piece, Direction)
mazeDeltaWall m !c !dir =
  if mazeBounded m delta
  then (, dir) <$> mazeRead m delta
  else pure (Piece 0 True (0, 0) True, dir)
  where
    delta = cursorDelta c dir

{--- Solver bits ---}

-- given current pixel at rotation, does it match the pixel at direction from it?
pixValid :: (Pix, Pix, Rotation, Direction) -> Bool
pixValid (!this, !that, !rotation, !direction) =
  not $ (rotate rotation this `Bit.xor` rotate 2 that) `Bit.testBit` direction

{-# INLINE validateDirection #-}
validateDirection :: Pix -> Rotation -> (Piece, Direction) -> Bool
validateDirection this rotation (Piece{pipe=that, solved}, direction) = do
  not solved || pixValid (this, that, rotation, direction)

validateRotation :: Pix -> [(Piece, Direction)] -> Rotation -> Bool
validateRotation this deltas rotation = all (validateDirection this rotation) deltas

{-# INLINE validateRotationM #-}
validateRotationM :: MMaze -> Cursor -> Pix -> Rotation -> IO Bool
validateRotationM maze cursor this rotation =
  (fmap (validateDirection this rotation) . mazeDeltaWall maze cursor) `allM` directions

pieceRotations :: MMaze -> Cursor -> IO [Pix]
pieceRotations maze@MMaze{board} cur = do
  Piece{pipe=this} <- mazeRead maze cur
  fmap (map (flip rotate this)) . filterM (validateRotationM maze cur this) $ pixRotations this

pieceRotationCount :: MMaze -> Cursor -> IO Int
pieceRotationCount maze@MMaze{board} cur = do
  Piece{pipe=this} <- mazeRead maze cur
  fmap length . filterM (validateRotationM maze cur this) $ pixRotations this

choiceScoreCoefficient = 10

cursorToContinue :: MMaze -> Continue -> PieceDelta -> IO Continue
cursorToContinue maze Continue{char, origin} (_, !c@(x, y), !direction) = do
  choices <- pieceRotationCount maze c
  let
    direct = char `Bit.testBit` direction
    origin' = if direct then origin else c
    score = (x - y + choices * choiceScoreCoefficient)
  pure $ Continue c pixUnset origin' choices score

addNextContinues :: MMaze -> Continue -> PartId -> Set Continue -> IO (Set Continue)
addNextContinues maze continue@Continue{cursor=cur} origin continues = do
  deltas <- filter (not . solved . view _1) <$> mazeDeltas maze cur directions
  next <- traverse (cursorToContinue maze continue { origin }) deltas
  pure $ foldl' (flip S.insert) continues next

progressPop :: Progress -> IO Progress
progressPop p@Progress{depth, maze, unwinds=(unwind:unwinds)} = do
  (depthL %~ (subtract 1) $ p { unwinds }) <$ mazePop maze unwind

pieceDead :: MMaze -> (Continue, Set Continue) -> IO Bool
pieceDead maze@MMaze{board} (continue@Continue{cursor=cur, char=this}, continues) = do
  thisPart <- partEquate maze . partId =<< mazeRead maze cur
  andM $ stuck : map (discontinued thisPart) (S.toList continues)
  where
    stuck = allM (fmap solved . mazeRead maze . cursorDelta cur) (pixDirections this)
    discontinued thisPart Continue{cursor=c} =
      if cur == c then pure True else
        ifM (mazeCursorSolved maze c) (pure True) ((thisPart /=) <$> partEquate maze c)

{--- Solver ---}

-- Solves a valid piece, mutates the maze and sets unwind
solveContinue :: Progress -> Continue -> IO Progress
solveContinue
  progress@Progress{iter, depth, maze=maze@MMaze{board}, continues}
  continue@Continue{cursor=cur, char=this, origin} = do
    unwindThis <- mazeSolve maze continue
    neighbours <- partEquate maze `traverse` (origin : map (cursorDelta cur) (pixDirections this))
    origin' <- pure . minimum $ origin : neighbours
    unwindEquate <- mazeEquate maze origin' neighbours
    continuesNext <- addNextContinues maze continue origin' continues

    traceProgress $ progress
    -- traceBoard continue $ progress
      & iterL %~ (+1)
      & depthL %~ (+1)
      & continuesL .~ continuesNext
      & unwindsL %~ ((unwindEquate ++ [unwindThis]) :)

findContinue :: Progress -> IO (Progress, Continue)
findContinue p@Progress{maze, continues} =
  ifM (mazeCursorSolved maze cursor)
    (findContinue p { continues = continues' })
    (pure (p { continues = continues' }, continue))
  where
    (continue@Continue{cursor}, continues') = S.deleteFindMin continues

-- Solves pieces by backtracking, stops when maze is solved.
solve' :: Progress -> IO Progress
-- solve' Progress{continues=[]} = error "unlikely"
solve' progressInit@Progress{iter, depth, maze} = do
  (progress@Progress{continues}, continue) <- findContinue progressInit

  rotations <- pieceRotations maze (cursor continue)
  guesses <- removeDead . map ((, continues) . ($ continue) . set charL) $ rotations
  progress' <- uncurry solveContinue =<< backtrack . (spaceL %~ (guesses :)) =<< pure progress

  (if last then pure else solve') progress'
  where
    last = depth == mazeSize maze - 1
    removeDead = if last then pure else filterM (fmap not . pieceDead maze)

    backtrack :: Progress -> IO (Progress, Continue)
    backtrack Progress{space=[]} = error "unlikely" -- for solvable mazes
    backtrack p@Progress{space=([]:space)} = progressPop p { space } >>= backtrack
    backtrack p@Progress{space=(((continue, continues):guesses):space)} =
      pure (p { continues, space = guesses : space }, continue)

solve :: MMaze -> IO MMaze
solve m = do
  solved@Progress{iter, depth} <- solve' $
    Progress 0 0 (S.singleton (Continue (0, 0) pixUnset (0, 0) 2 0)) [] [] m
  putStrLn (printf "%i/%i, ratio: %0.5f" iter depth (fromIntegral iter / fromIntegral depth :: Double))
  pure (maze solved)

verify :: MMaze -> IO Bool
verify maze = do
  (mazeSize maze ==) <$> partFollow maze S.empty [(0, 0)]
  where
    partFollow :: MMaze -> Set Cursor -> [Cursor] -> IO Int
    partFollow maze visited [] = pure (S.size visited)
    partFollow maze visited (cursor:next) = do
      this <- pipe <$> mazeRead maze cursor
      valid <- validateRotation this <$> mazeDeltasWalls maze cursor <*> pure 0
      if not valid
      then pure (S.size visited)
      else do
        deltas <- mazeDeltas maze cursor (pixDirections this)
        deltas' <- filter (not . flip S.member visited) . map (view _2) <$> pure deltas
        partFollow maze (S.insert cursor visited) (deltas' ++ next)

storeBad :: Int -> MMaze -> MMaze -> IO MMaze
storeBad level original solved = do
  whenM (fmap not . verify $ solved) $ do
    putStrLn (printf "storing bad level %i solve" level)
    mazeStore original ("samples/bad-" ++ show level)
  pure solved

{--- Main ---}

rotateStr :: MMaze -> MMaze -> IO Text
rotateStr input solved = concatenate <$> rotations input solved
  where
    concatenate :: [(Cursor, Rotation)] -> Text
    concatenate =
      (T.pack "rotate " <>) . T.intercalate (T.pack "\n")
      . (>>= (\((x, y), r) -> replicate r (T.pack (printf "%i %i" x y))))

    rotations :: MMaze -> MMaze -> IO [(Cursor, Rotation)]
    rotations maze@MMaze{board=input, width, height} MMaze{board=solved} = do
      V.toList . V.imap (\i -> (mazeCursor maze i, ) . uncurry (rotations `on` pipe))
      <$> (V.zip <$> V.freeze input <*> V.freeze solved)
      where
        rotations from to = fromJust $ to `elemIndex` iterate (rotate 1) from

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

      send =<< rotateStr maze =<< storeBad level maze =<< solve =<< mazeClone maze
      recv

      send (T.pack "verify")
      putStrLn =<< recv

solveFiles :: String -> IO ()
solveFiles file = do
  solve <- solve =<< parse =<< readFile file
  whenM (not <$> verify solve) (putStrLn "solution invalid")

main :: IO ()
main = do
  whenM (isNothing <$> lookupEnv "trace") $ setEnv "trace" "-"
  whenM (isNothing <$> lookupEnv "freq") $ setEnv "freq" "1"

  websocket <- (== "1") . fromMaybe "0" <$> lookupEnv "websocket"

  if websocket
  then withSocketsDo $ WS.runClient "maze.server.host" 80 "/game-pipes/" pļāpātArWebsocketu
  else traverse_ solveFiles . (\args -> if null args then ["/dev/stdin"] else args) =<< getArgs
