{-# LANGUAGE TupleSections, NamedFieldPuns, BinaryLiterals, TemplateHaskell, CPP, ScopedTypeVariables, NumericUnderscores #-}

{-# LANGUAGE StrictData, BangPatterns #-}
{-# OPTIONS_GHC -O #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-unused-do-bind -Wno-type-defaults -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- complains about lens :/

-- json
{-# LANGUAGE DeriveGeneric #-}

#ifndef TRACE
#define TRACE 1
#endif

#ifndef TRACESUFFIX
#define TRACESUFFIX ""
#endif

#ifndef FREQ
#define FREQ 3_000
#endif

module Main (main, render) where

-- solver

import Control.Lens.Internal.FieldTH (makeFieldOptics, LensRules(..))
import Language.Haskell.TH.Syntax (mkName, nameBase)
import Control.Lens.TH (DefName(..), lensRules)

import Control.Lens ((&), (%~), set, view, _1, _2)
import Control.Monad.Extra (allM, whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join, filterM, void, when, mfilter)
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.State.Strict (StateT(..))
import Data.Bifunctor (bimap)
import Data.Foldable (fold, traverse_, for_, foldrM)
import Data.Function (on)
import Data.List (elemIndex, foldl')
import Data.List.Extra (nubOrd)
import Data.Map.Strict (Map, (!))
import Data.IntMap.Strict (IntMap)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Monoid (Sum(..))
import Data.Set (Set)
import Data.Tuple (swap)
import Data.Vector.Mutable (MVector)
import Data.Word (Word8)
-- import Debug.Trace (trace)
import Graphics.Image.Interface (thaw, MImage, freeze, write)
import Graphics.Image (writeImage, makeImageR, Pixel(..), toPixelRGB, VU(..), RGB)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bits as Bit
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.Environment (lookupEnv, getArgs)
import Text.Printf (printf)

-- json for debug outputs
import Data.Aeson (ToJSON(..))
-- import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Generics (Generic)

-- IO

import Data.Text (Text)
import Network.Socket (withSocketsDo)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

-- t a = seq (trace (show a) a) a
-- t' l a = seq (trace (show l ++ ": " ++ show a) a) a
-- tM :: (Functor f, Show a) => f a -> f a
-- tM = fmap t

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
  , wasIsland :: Bool -- was this solved by an island Continue?
  } deriving (Generic, Show)

type PieceDelta = (Piece, Cursor, Direction)

data MMaze = MMaze -- Mutable Maze
  { board :: MVector RealWorld Piece
  , width :: Int
  , height :: Int
  , size :: Int
  , sizeLen :: Int -- leading char count for printf %0ni format
  , level :: Int
  }

-- Continue represents the piece that should be solved next
-- Continues in Progress may turn out to be already solved, which must be checked
data Continue = Continue
  { cursor :: Cursor
  , char :: Pix -- defaulted to 0 (i.e. ' '), but set when guessing
  , origin :: PartId
  , score :: Int
  , created :: Int -- unique ID for Ord (total order), taken from Progress.iter
  , island :: Int -- 0 if not an island or not active, otherwise 1
  , area :: Int -- island area, 0 if not an island
  , choices :: Int -- number of valid rotations
  } deriving (Generic, Show)

type Score = (Int, Int) -- score, created

type Priority = IntMap Cursor
type Continues = Map Cursor Continue
data Components = Components (Map PartId Int) | Components' (Map PartId (Set Cursor)) deriving (Show, Generic)

type Unwind1 = (Cursor, Piece)
type Unwind = [Unwind1]

-- Generic piece for the "fill" (think ms paint) operation
type FillNext s = MMaze -> Cursor -> Piece -> [(Piece, Direction)] -> StateT s IO [Cursor]

data Progress = Progress
  { iter :: Int -- the total number of backtracking iterations (incl. failed ones)
  , depth :: Int -- number of solves, so also the length of unwinds/space
  , priority :: Priority -- priority queue for next guesses (not a heap, because of reprioritizing)
  , continues :: Continues -- for finding if continue exists already in Priority
  , components :: Components -- unconnected network continue count
  , space :: [[(Continue, Progress)]] -- unexplored solution space. enables parallelization. item per choice, per cursor.
  , unwinds :: [Unwind] -- history, essentially. an item per a solve. pop when (last space == [])
  , maze :: MMaze
  }

makeFieldOptics lensRules { _fieldToDef = (\_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase) } ''Continue
makeFieldOptics lensRules { _fieldToDef = (\_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase) } ''Progress

instance Show Progress where
  show Progress{iter, priority} =
    "Progress" ++ show (iter, length priority)

instance Show MMaze where
  show _ = "MMaze"

instance ToJSON Piece
instance ToJSON Continue
instance ToJSON Components

{--- MMaze and Matrix operations ---}

parse :: String -> IO MMaze
parse input = do
  board <- V.thaw . V.fromList . map (\c -> Piece c False (0, 0) False False) . join $ rect
  maze <- pure (MMaze board width height size zeros level)
  maze <$ mazeMap maze (\c p -> p { partId = c })
  where
    rect = filter (not . null) . map (map toPix) . lines $ input
    (width, height) = (length (head rect), (length rect))
    size = width * height
    zeros = floor (logBase 10 (fromIntegral size) + 1.5)
    level = fromMaybe 0 (lookup width [(8,1), (25,2), (50,3), (200,4), (400,5), (1000,6)])

mazeStore :: MMaze -> String -> IO ()
mazeStore m label = writeFile label =<< renderStr m

mazeBounded :: MMaze -> Cursor -> Bool
mazeBounded MMaze{width, height} (!x, !y) = x >= 0 && y >= 0 && width > x && height > y

vectorLists :: Int -> Int -> V.Vector a -> [[a]]
vectorLists width height board = [ [ board V.! (x + y * width) | x <- [0..width - 1] ] | y <- [0..height - 1] ]

mazeCursor :: MMaze -> Int -> Cursor
mazeCursor MMaze{width} = swap . flip quotRem width

{-# INLINE mazeRead #-}
mazeRead :: MMaze -> Cursor -> IO Piece
mazeRead MMaze{board, width} (x, y) = MV.unsafeRead board (x + y * width)

mazeModify :: MMaze -> (Piece -> Piece) -> Cursor -> IO ()
mazeModify MMaze{board, width} f (x, y) = MV.unsafeModify board f (x + y * width)

mazeMap :: MMaze -> (Cursor -> Piece -> Piece) -> IO ()
mazeMap m@MMaze{width, height} f =
  void . traverse (\cursor -> mazeModify m (f cursor) cursor) . join $
    [ [ (x, y) | x <- [0..width - 1] ] | y <- [0..height - 1] ]

mazeClone :: MMaze -> IO MMaze
mazeClone m@MMaze{board} = (\board -> m { board }) <$> MV.clone board

mazeSolve :: MMaze -> Continue -> IO Unwind1
mazeSolve m Continue{char, cursor, island} =
  (cursor, ) <$> mazeRead m cursor <* mazeModify m modify cursor
  where modify p = p { pipe = char, solved = True, wasIsland = if island > 0 then True else False }

mazeEquate :: MMaze -> PartId -> [Cursor] -> IO Unwind
mazeEquate m partId cursors =
  traverse (\c -> (c, ) <$> mazeRead m c) cursors
  <* traverse (mazeModify m (\p -> p { partId, connected = True })) cursors

mazePop :: MMaze -> Unwind -> IO ()
mazePop m = traverse_ (uncurry (flip (mazeModify m . const)))

-- lookup fixed point (ish) in maze by PartId lookup, stops at first cycle
partEquate :: MMaze -> PartId -> IO PartId
partEquate maze v = loop' =<< find v
  where
    find f = (\Piece{connected, partId} -> if connected then partId else f) <$> mazeRead maze f
    loop' v' = (\found -> if v' == v || v' == found then pure v' else loop' found) =<< find v'

{--- Rendering, tracing ---}

renderImage :: String -> MMaze -> Continues -> IO ()
renderImage fn maze@MMaze{width, height} continues = seq continues $ do
  mcanvas <- thaw canvas :: IO (MImage RealWorld VU RGB Double)
  traverse (drawPiece mcanvas) grid
  writeImage fn =<< freeze mcanvas
  where
    (pw, ph) = (3, 3)
    canvas = makeImageR VU (width * pw, height * ph) $ const (PixelRGB 0 0 0)
    grid = (,) <$> [0..width - 1] <*> [0..height - 1]

    colorHash :: Cursor -> Double
    colorHash (x, y) =
      let
        n = ((83 * fromIntegral x) / (37 * fromIntegral (y + 2))) + 0.5
        unfloor m = m - fromIntegral (floor m)
      in unfloor $ (unfloor n) / 4 - 0.15

    drawPiece :: MImage RealWorld VU RGB Double -> PartId -> IO ()
    drawPiece image cur@(x, y) = do
      Piece{pipe, partId, solved} <- mazeRead maze cur
      ch <- colorHash <$> partEquate maze partId
      let cont = Map.lookup cur continues
      let colo = maybe ch (\c -> if island c == 2 then 0 else (if island c == 1 then 0.5 else 0.7)) cont
      let satu = if solved then 0.3 else (if isJust cont then 1 else 0)
      let inte = if isJust cont then 1 else 0.5
      let fill = toPixelRGB $ PixelHSI colo satu inte
      write image (x * pw + 1, y * ph + 1) fill
      for_ (pixDirections pipe) $ \d ->
        when (Bit.testBit pipe d) $ write image (cursorDelta (x * pw + 1, y * ph + 1) d) fill

renderImage' :: String -> Progress -> IO Progress
renderImage' name p@Progress{maze=maze@MMaze{sizeLen, level}, iter, continues} =
  p <$ renderImage (printf ("images/lvl%i-%s-%0*i.png") level name sizeLen iter) maze continues

renderWithPositions :: Maybe Continue -> Progress -> IO String
renderWithPositions _ Progress{maze=maze@MMaze{board, width, height}} =
  pure . unlines . map concat . vectorLists width height =<< V.imapM fmt . V.convert =<< V.freeze board
  where
    colorHash = (`mod` 70) . (+15) . (\(x, y) -> x * 67 + y * 23)
    fmt _ Piece{pipe, partId, solved} = do
      color <- mfilter (\_ -> solved) . Just . colorHash <$> partEquate maze partId
      pure $ case color of
        Just color -> printf "\x1b[38;5;%im%c\x1b[39m" ([24 :: Int, 27..231] !! color) (toChar pipe)
        _ -> (toChar pipe) : []

render :: MMaze -> IO ()
render = (putStrLn =<<) . renderWithPositions Nothing . Progress 0 0 IntMap.empty Map.empty (Components Map.empty) [] []

renderStr :: MMaze -> IO String
renderStr MMaze{board, width, height} = do
  unlines . map concat . vectorLists width height . V.map (return . toChar . pipe) . V.convert
  <$> V.freeze board

islandCounts :: Progress -> (Int, Int)
islandCounts Progress{continues} = bimap getSum getSum . foldMap count $ Map.toList continues
  where
    icount i n = Sum (fromEnum (i == n))
    count (_, Continue{island=i}) = (icount i 2, icount i 1)

traceBoard :: Continue -> Progress -> IO Progress
traceBoard current progress@Progress{iter, depth, maze=MMaze{size}} = do
  let (mode, freq, suffix) = (TRACE :: Int, FREQ :: Int, TRACESUFFIX :: String)
  tracer mode freq suffix *> pure progress
  where
    tracer mode freq s -- reorder/comment out clauses to disable tracing
      | iter `mod` freq == 0 && mode == 1 = pure solvedStr >>= putStrLn
      | iter `mod` freq == 0 && mode == 2 = ((clear ++) <$> traceStr) >>= putStrLn
      | iter `mod` freq == 0 && mode == 3 = traceStr >>= putStrLn
      | iter `mod` freq == 0 && mode == 4 = tracer 1 freq s >> void (renderImage' ("trace-" ++ s ++ show iter) progress)
      | True = pure ()

    perc = (fromIntegral $ depth) / (fromIntegral size) * 100 :: Double
    ratio = (fromIntegral iter / fromIntegral depth :: Double)
    (i, is) = islandCounts progress
    solvedStr = printf "\x1b[2Kislands: %2i/%2i, solved: %02.2f%%, ratio: %0.2f\x1b[1A" i is perc ratio

    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    traceStr = renderWithPositions (Just current) progress

{--- Model ---}

directions = [0, 1, 2, 3]
rotations = directions

charMapEntries :: [(Char, Pix)]
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
  else pure (Piece 0 True (0, 0) True False, dir)
  where delta = cursorDelta c dir

{--- Solver bits: per-pixel stuff ---}

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
pieceRotations maze cur = do
  Piece{pipe=this} <- mazeRead maze cur
  fmap (map (flip rotate this)) . filterM (validateRotationM maze cur this) $ pixRotations this

pieceRotationCount :: MMaze -> Cursor -> IO Int
pieceRotationCount maze cur = do
  Piece{pipe=this} <- mazeRead maze cur
  fmap length . filterM (validateRotationM maze cur this) $ pixRotations this

{--- Solver bits: components ---}

compInsert :: Continue -> Components -> Components
compInsert Continue{origin} (Components c) = Components (Map.insertWith (+) origin 1 c)
compInsert Continue{origin, cursor} (Components' c) = Components' (Map.insertWith (Set.union) origin (Set.singleton cursor) c)

compRemove :: PartId -> Cursor -> Components -> Components
compRemove origin _cursor (Components c) = Components (Map.update (Just . (subtract 1)) origin c)
compRemove origin cursor (Components' c) = Components' (Map.update (Just . Set.delete cursor) origin c)

compEquate :: PartId -> [PartId] -> Components -> Components
compEquate hub connections c = equate c
  where
    equate (Components c) = Components $ equate' Sum getSum c
    equate (Components' c) = Components' $ equate' id id c

    equate' :: Monoid m => (a -> m) -> (m -> a) -> Map PartId a -> Map PartId a
    equate' lift drop c = Map.insertWith (\a b -> drop (lift a <> lift b)) hub (drop sum) removed
      where (sum, removed) = (foldl' (extract lift) (mempty, c) connections)

    extract lift (sum, m) part = Map.alterF ((, Nothing) . mappend sum . foldMap lift) part m

compAlive :: PartId -> Components -> Bool
compAlive k (Components c) = (Just 1 ==) $ Map.lookup k c
compAlive k (Components' c) = (Just 1 ==) . fmap Set.size $ Map.lookup k c

compConnected :: PartId -> Components -> [Cursor]
compConnected k (Components' c) = foldMap Set.toList (Map.lookup k c)
compConnected _ _ = []

{--- Solver bits: continues ---}

cursorToContinue :: MMaze -> Int -> Continue -> (PieceDelta, Int) -> IO Continue
cursorToContinue maze iter Continue{char, origin, island, area} ((_, c, !direction), index) = do
  let origin' = if char `Bit.testBit` direction then origin else c
  let island' = min 2 (island * 2)
  Continue c 0 origin' 0 (iter * 4 + index) island' area <$> pieceRotationCount maze c

prioritizeDeltas :: Progress -> Continue -> IO Progress
prioritizeDeltas progress@Progress{iter, maze} continue@Continue{cursor=cur} = do
  deltas <- filter (not . solved . view _1) <$> mazeDeltas maze cur directions
  prioritizeContinues progress <$> traverse (cursorToContinue maze iter continue) (zip deltas [0..])

prioritizeIslands :: [Cursor] -> Progress -> IO Progress
prioritizeIslands _ p@Progress{components = (Components _)} = pure p
prioritizeIslands directDeltas p@Progress{maze, components = (Components' _)} = do
  shores <- filterM (fmap solvedSea . mazeRead maze) directDeltas
  foldr prioritizeIsland p <$> traverse (partEquate maze) shores
  where
    solvedSea Piece{solved, wasIsland} = solved && not wasIsland

    prioritizeIsland :: PartId -> Progress -> Progress
    prioritizeIsland k p@Progress{continues} =
      prioritizeContinues p . map (\c -> (continues Map.! c) { island = 1 }) $ compConnected k (components p)

prioritizeContinues :: Progress -> [Continue] -> Progress
prioritizeContinues progress@Progress{priority, continues, components} next =
  (\((priority, components), continues) -> progress { priority, components, continues }) $
    foldl' foldContinue ((priority, components), continues) $ map rescore next
  where
    rescore c@Continue{cursor=(x, y), choices, island, created} =
      c { score = (x + y + choices * 2^15 - island * 2^17) * 2^32 + created }

    foldContinue (acc, continues) c@Continue{cursor} = Map.alterF (insertContinue acc c) cursor continues

    insertContinue :: (Priority, Components) -> Continue -> Maybe Continue -> ((Priority, Components), Maybe Continue)
    insertContinue (p, c) new@Continue{cursor} Nothing =
      ((IntMap.insert (score new) cursor p, compInsert new c), Just new)
    insertContinue (p, c) new (Just (exists@Continue{cursor, created})) =
      ((contModify p, c), Just putback)
      where
        (putback, contModify) =
          if score new < score exists
          then (new { created }, IntMap.insert (score new) cursor . IntMap.delete (score exists))
          else (exists, id)

pieceDead :: MMaze -> Components -> (Continue, Priority) -> IO Bool
pieceDead maze components (Continue{cursor=cur, char=this}, _) = do
  thisPart <- partEquate maze . partId =<< mazeRead maze cur
  (compAlive thisPart components &&) <$> stuck
  where stuck = allM (fmap solved . mazeRead maze . cursorDelta cur) (pixDirections this)

-- assumptions: no solved or duplicate continues (per cursor) in priority
findContinue :: Progress -> IO (Progress, Continue)
findContinue p@Progress{priority, continues} = do
  pure (p { priority = priority', continues = continues' }, continue)
  where
    ((_, cursor), priority') = IntMap.deleteFindMin priority
    (continue, continues') = Map.alterF ((, Nothing) . fromJust) cursor continues

-- flood fill with the generic function fillNext as "paint"
-- assumptions: current piece is considered valid by fillNext
flood :: Monoid s => FillNext s -> MMaze -> Cursor -> IO (Set Cursor, s)
flood n m = flip runStateT mempty . flood' n m Set.empty . return
  where
  flood' :: FillNext s -> MMaze -> Set Cursor -> [Cursor] -> StateT s IO (Set Cursor)
  flood' _ _ visited [] = pure visited
  flood' fillNext maze visited (cursor:next) = do
    this <- liftIO (mazeRead maze cursor)
    more <- fillNext maze cursor this =<< liftIO (mazeDeltasWalls maze cursor)
    let next' = filter (not . flip Set.member visited) more ++ next
    flood' fillNext maze (Set.insert cursor visited) next'

islandize :: Progress -> IO Progress
islandize p@Progress{maze, continues} = do
  (_, _) <- foldIsland perIsland (map (cursor . snd) . Map.toList $ continues)
  let p' = maybe p (prioritizeContinues p . return) ((\c -> (continues Map.! c) { island = 1 }) . snd <$> IntMap.lookupMin (priority p))
  pure p'
  where
    borderContinues :: Continues -> Set Cursor -> Bool -> [Continue]
    borderContinues continues borders active =
      map (\c -> (continues Map.! c) { island = if active then 1 else 0, area = Set.size borders }) $ Set.toList borders

    foldIsland perIsland continues =
      (\acc -> (\(_, b, c) -> (b, c)) <$> foldrM acc (Set.empty, p, (0, Set.empty)) continues)
        (\cursor acc@(visited, _, _) -> if (cursor `Set.member` visited) then pure acc else perIsland cursor acc)

    -- border = island's border by continues
    perIsland :: Cursor -> (Set Cursor, Progress, (Int, Set Cursor)) -> IO (Set Cursor, Progress, (Int, Set Cursor))
    perIsland cursor (visited, p@Progress{continues}, max@(maxSize, _)) = do
      (_all, borders :: Set Cursor) <- flood (fillNextSolved continues) maze cursor
      let max' = if Set.size borders > maxSize then (Set.size borders, borders) else max
      pure (visited `Set.union` borders, prioritizeContinues p (borderContinues continues borders False), max')

    fillNextSolved :: Continues -> FillNext (Set Cursor)
    fillNextSolved continues _ cur _ deltasWall = do
      when (cur `Map.member` continues) $ State.modify (Set.insert cur)
      pure . map (cursorDelta cur . snd) . filter (\(Piece{pipe, solved}, _) -> pipe /= 0 && not solved) $ deltasWall

-- Progress.components = Components -> Components'
reconnectComponents :: Progress -> IO Progress
reconnectComponents p@Progress{maze, continues} = do
  (\c -> p { components = c }) . Components' . foldr1 (Map.unionWith Set.union) <$> traverse component (Map.toList continues)
  where component (_, Continue{origin, cursor}) = Map.singleton <$> partEquate maze origin <*> pure (Set.singleton cursor)

{--- Solver ---}

-- Solves a valid piece, mutates the maze and sets unwind
-- inefficient access: partEquate reads the same data as islands reads
solveContinue :: Progress -> Continue -> IO Progress
solveContinue
  progress@Progress{maze, components = components_}
  continue@Continue{cursor, char, origin = origin_} = do
    unwindThis <- mazeSolve maze continue
    thisPart <- partEquate maze origin_
    let directDeltas = map (cursorDelta cursor) $ pixDirections char
    neighbours <- fmap (nubOrd . (thisPart :)) . traverse (partEquate maze) $ directDeltas
    let origin = minimum neighbours
    let components = compEquate origin (filter (/= origin) (neighbours)) (compRemove thisPart cursor components_)
    unwindEquate <- mazeEquate maze origin neighbours
    --
    progress' <- prioritizeIslands directDeltas =<< prioritizeDeltas progress { components } continue { origin }

    traceBoard continue $ progress' & iterL %~ (+1) & depthL %~ (+1) & unwindsL %~ ((unwindEquate ++ [unwindThis]) :)

-- Solves pieces by backtracking, stops when maze is solved.
solve' :: Int -> Bool -> Progress -> IO Progress
-- solve' _ _ p@Progress{priority} | Map.null priority = pure p
solve' lifespan first progressInit@Progress{iter, depth, maze=maze@MMaze{size}} = do
  (progress@Progress{components}, continue) <- findContinue progressInit

  rotations <- pieceRotations maze (cursor continue)
  guesses <- removeDead components . map ((, progress) . ($ continue) . set charL) $ rotations
  progress' <- uncurry solveContinue =<< backtrack . (spaceL %~ (guesses :)) =<< pure progress

  let islandish = iter > 2 && length guesses /= 1
  let stop = last || lifespan == 0 || (islandish && first)
  (if stop then pure else solve' (lifespan - 1) first) progress'
  where
    last = depth == size - 1
    removeDead components = if last then pure else filterM (fmap not . pieceDead maze components . (_2 %~ priority))

    backtrack :: Progress -> IO (Progress, Continue)
    backtrack Progress{space=[]} = error "unsolvable"
    backtrack Progress{space=([]:_), unwinds=[]} = error "unlikely"
    backtrack p@Progress{space=([]:space), unwinds=(unwind:unwinds)} = mazePop maze unwind >> backtrack p { space, unwinds }
    backtrack p@Progress{space=(((continue, Progress{depth, priority, continues, components}):guesses):space)} =
      pure (p { depth, priority, continues, components, space = guesses : space }, continue)

solve :: MMaze -> IO MMaze
solve m = do
  let init = Continue (0, 0) 0 (0, 0) 0 0 0 0 0
  let components = Components (Map.fromList [((0, 0), 1)])
  let p = Progress 0 0 (IntMap.singleton 0 (0, 0)) (Map.singleton (0, 0) init) components [] [] m
  p <- solve' (-1) False =<< reconnectComponents =<< islandize =<< solve' (-1) True p
  -- p <- foldrM id p . replicate 30 $ (solve' 100 False =<<) . renderImage' "di0405"
  let solved@Progress{iter, depth, maze} = p

  putStrLn (printf "\x1b[2K%i/%i, ratio: %0.5f" iter depth (fromIntegral iter / fromIntegral depth :: Double))
  maze <$ renderImage' "done" solved

{--- Main ---}

verify :: MMaze -> IO Bool
verify maze@MMaze{size} = do
  (size ==) . Set.size . fst <$> flood fillNextValid maze (0, 0)
  where
    fillNextValid :: FillNext ()
    fillNextValid maze cur Piece{pipe=this} deltasWalls = pure $
      if validateRotation this deltasWalls 0
      then filter (mazeBounded maze) . map (cursorDelta cur) $ pixDirections this
      else []

storeBad :: Int -> MMaze -> MMaze -> IO MMaze
storeBad level original solved = do
  whenM (fmap not . verify $ solved) $ do
    putStrLn (printf "storing bad level %i solve" level)
    mazeStore original ("samples/bad-" ++ show level)
  pure solved

rotateStr :: MMaze -> MMaze -> IO Text
rotateStr input solved = concatenate <$> rotations input solved
  where
    concatenate :: [(Cursor, Rotation)] -> Text
    concatenate =
      (T.pack "rotate " <>) . T.intercalate (T.pack "\n")
      . (>>= (\((x, y), r) -> replicate r (T.pack (printf "%i %i" x y))))

    rotations :: MMaze -> MMaze -> IO [(Cursor, Rotation)]
    rotations maze@MMaze{board=input} MMaze{board=solved} = do
      V.toList . V.imap (\i -> (mazeCursor maze i, ) . uncurry (rotations `on` pipe))
      <$> (V.zip <$> V.freeze input <*> V.freeze solved)
      where
        rotations from to = fromJust $ to `elemIndex` iterate (rotate 1) from

pļāpātArWebsocketu :: WS.ClientApp ()
pļāpātArWebsocketu conn = for_ [1..6] solveLevel
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
  solved <- solve =<< parse =<< readFile file
  whenM (not <$> verify solved) (putStrLn "solution invalid")

main :: IO ()
main = do
  websocket <- (== "1") . fromMaybe "0" <$> lookupEnv "websocket"

  if websocket
  then withSocketsDo $ WS.runClient "maze.server.host" 80 "/game-pipes/" pļāpātArWebsocketu
  else traverse_ solveFiles . (\args -> if null args then ["/dev/stdin"] else args) =<< getArgs
