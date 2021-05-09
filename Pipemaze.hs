{-# LANGUAGE TupleSections, NamedFieldPuns, BinaryLiterals, TemplateHaskell, CPP, ScopedTypeVariables, NumericUnderscores, FlexibleInstances #-}

{-# LANGUAGE StrictData, BangPatterns #-}
{-# OPTIONS_GHC -O #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-unused-do-bind -Wno-type-defaults -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- complains about lens :/

-- json
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Pipemaze
Description : solves pipe mazes
Copyright   : (c) siers
License     : GPL-3
Maintainer  : wimuan@email.com
Stability   : experimental
Portability : POSIX

A dynamic solver of pipe mazes with an internal 'priority' queue based on scores, position and priority creation order.

The goal of the solver is to find the rotation of each piece in the grid to create a connected graph.
The number of valid rotations of a piece are encoded in 'Choices'.
Each cursor has four nearby cursors – top/right/bottom/left, which wil be refered to as 'directions' or 'deltas'.
The 'Cursor's in one square's distance in a 'direction' with respect to some cursor @c@ may also be called 'deltas'.
Disconnected graphs that will have to be connected
to the other graphs later are being refered to as /components/. <https://en.wikipedia.org/wiki/Component_(graph_theory)>

Solving each piece gives you hints about surrounding pieces,
so solving them in our order('rescoreContinue') is more effective than solving in an arbitrary order. If the connectivity of the pieces is efficiently
computed ('PartId' \/ 'partEquate'), the "open ends" ('Continues') have a good prioritization and the partial solutions on disconnected components
are immediately discarded ('pieceDead' \/ 'compAlive'), the /visited piece/ ratio to /maze size/ is rather small (<4)
for these specific mazes from the maze server.

This combined lets you solve around 98% of the level 6 maze determinstically, but the priority after that (many unsolved islands) is not yet optimal.
-}

#ifndef TRACE
#define TRACE 1
#endif

#define FREQ_DEF 3_000
#ifndef FREQ
#define FREQ FREQ_DEF
#endif

module Pipemaze (
  -- * Types
  Direction, Rotation, Pix, Cursor, Fursor, MMaze(..), Piece(..), Choices, PartId, Continue(..)
  , Priority, Continues, Components(..), Unwind, Progress(..), Island(..)
  -- * Maze operations
  , parse, mazeStore, mazeBounded, mazeRead, mazeSolve, mazeDelta, mazeFDelta, mazeEquate, mazePop, partEquate
  -- * Tracing and rendering
  , renderImage'
  , traceBoard
  -- * Pixel model
  , directions, rotations, charMap, pixMap, pixRotations, pixDirections, directionsPix, toPix, toChar, rotate
  -- * Pixel solving
  , pixValid, validateDirection, pieceChoices
  -- * Component indexing
  , compInsert, compRemove, compEquate, compAlive, compConnected
  -- * Continue operations
  , deltaContinue, prioritizeDeltas, prioritizeIslands, rescoreContinue, prioritizeContinues
  , pieceDead, findContinue
  -- * Island computations
  , FillNext, flood, islandizeFirst, islandizeArea, islands, graphIslands, previewIslands
  -- * Solver brain
  , solveContinue, solve', initProgress, backtrack, solve
  -- * Main
  , verify, storeBad, rotateStr, pļāpātArWebsocketu, solveFile, main
) where

-- solver

import Control.Lens.Internal.FieldTH (makeFieldOptics, LensRules(..))
import Language.Haskell.TH.Syntax (mkName, nameBase)
import Control.Lens.TH (DefName(..), lensRules)

import Control.Lens ((&), (%~), set, _2)
import Control.Monad.Extra (allM, whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join, filterM, void, when, mfilter, liftM)
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.State.Strict (StateT(..))
import Data.Bifunctor (bimap)
import Data.Char (ord)
import Data.Foldable (traverse_, for_, foldrM)
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.List (elemIndex)
import Data.List.Extra (nubOrd)
import Data.Map.Strict (Map, (!))
import Data.Maybe (fromMaybe, fromJust, isJust, maybeToList)
import Data.Monoid (Sum(..))
import Data.Set (Set)
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Vector.Mutable (MVector)
import Data.Word (Word8)
-- import Debug.Trace (trace)
import Graphics.Image.Interface (thaw, MImage, freeze, write)
import Graphics.Image (writeImage, makeImageR, Pixel(..), toPixelRGB, VU(..), RGB)
import Numeric (showHex)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bits as Bit
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.Environment (lookupEnv, getArgs)
import Text.Printf (printf)

-- graph debug
import Control.Concurrent (forkIO)
import Data.Graph.Inductive.Graph (Graph(..), nmap)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.ArtPoint (ap)
-- import Data.Graph.Inductive.Query.DFS (isConnected)
import Data.GraphViz.Attributes.Colors (WeightedColor(..))
import Data.GraphViz.Attributes.Complete (Color(..), Attribute(..), Shape(..), Overlap(..), EdgeType(..), DPoint(..), createPoint)
import Data.GraphViz (GraphvizCanvas(..), GraphvizCommand(..), GraphvizParams(..), GraphvizOutput(..))
import Data.GraphViz (runGraphvizCanvas, runGraphvizCommand, graphToDot, nonClusteredParams)

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

-- | Directions: top 0, right 1, bottom 2, left 3
type Direction = Int
-- | The set of rotation values are the same as directions.
type Rotation = Int
-- | The maze symbol bit-packed in 'charMap' as @2^d@ per direction, mirrored in @shiftL 4@ to help bit rotation
--
-- > I – ━, ┃
-- > L – ┏, ┛, ┓, ┗
-- > T – ┣, ┫, ┳, ┻
-- > X – ╋
-- > i – ╸, ╹, ╺, ╻
type Pix = Word8
type Cursor = (Int, Int)
-- | Flat cursors, for @f :: FCursor, (x, y) :: Cursor, f = x + y * width@. Through `Choices`, bound checking is cached, so deltas are just ±1 or ±width.
type Fursor = Int

-- | unlawful instance
instance Eq (MVector RealWorld Piece) where _ == _ = True
-- | unlawful instance
instance Ord (MVector RealWorld Piece) where _ <= _ = True

-- | Mutable maze operated on by functions in section /"Maze operations"/
data MMaze = MMaze
  { board :: MVector RealWorld Piece -- ^ flat MVector with implied 2d structure via 'Cursor'/'Fursor' + index computations
  , width :: Int
  , height :: Int
  , size :: Int
  , sizeLen :: Int -- ^ leading char count for @printf %0ni@ format @(~logBase 10 size + 1.5)@
  , level :: Int
  , trivials :: [Fursor] -- ^ cursors of the edge and @X@ pieces which have only one valid rotation
  , mazeId :: String -- ^ 'board's data scrambled into a 4-byte hexadecimal field
  } deriving (Eq, Ord, Generic)

data Piece = Piece
  { pipe :: Pix
  , solved :: Bool
  , partId :: PartId -- ^ meaningless if not connected
  , connected :: Bool -- ^ connected when pointed to by a 'solved' piece
  , wasIsland :: Bool -- ^ was this solved by an island 'Continue'?
  , initChoices :: Choices
  } deriving (Show, Eq, Ord, Generic)

-- | 'Choices' is bit-packed info related to the valid rotations of a picce.
-- In MSB order: (valid) rotation count 2b, invalid rotation directions 4b, solved requirements 4b, solved neighbours 4b
type Choices = Int
(choicesSolveds, choicesRequire, choicesInvalid, choicesCount) = (0, 4, 8, 12)

-- | Continue represents the piece that should be solved next, which is an open end of a component or starts one.
data Continue = Continue
  { cursor :: Fursor
  , char :: Pix -- ^ defaulted to 0 (i.e. ' '), but set when guessing in 'solve\''
  , origin :: PartId -- ^ component id, to be used with 'partEquate'
  , score :: Int -- ^ see 'rescoreContinue'
  , created :: Int -- ^ any unique id to make score order total ('Ord' requirement), taken from 'iter'
  , island :: Int -- ^ \> 0 if island
  , area :: Int -- ^ island area, 0 if not an island
  , choices :: Choices
  } deriving (Show, Eq, Ord, Generic)

-- | 'PartId' distinguishes the graph component by their smallest known 'Cursor' by its 'Ord' (unique),
-- so it is the same as its cursor initially. They're marked in 'origin' ahead of 'solved's.
-- 'PartId' in 'origin' is only to be used through 'partEquate', because 'origin' isn't being
-- updated after components have connected.
type PartId = Fursor

-- | 'Continue' priority queue, inserted by 'prioritizeContinues', popped by 'findContinue'
type Priority = IntMap Fursor
-- | Primary storage of 'Continue' data
type Continues = IntMap Continue
-- | The index of components' continues by their 'PartId' (which are always up-to-date).
data Components
  = Components (IntMap Int) -- ^ marginally faster, but less info
  | Components' (IntMap IntSet)
  deriving (Show, Eq, Ord, Generic)

-- | For mutable backtracking
type Unwind = (Fursor, Piece)

data Progress = Progress
  { iter :: Int -- ^ the total number of backtracking iterations (incl. failed ones)
  , depth :: Int -- ^ number of solves, so also the length of unwinds/space
  , priority :: Priority -- ^ priority queue for next guesses (tree, not a heap, because reprioritizing is required)
  , continues :: Continues -- ^ Primary continue store, pointed to by 'priority' (all 'Continue's within must be unique by their cursor)
  , components :: Components -- ^ component continue counts (for quickly computing disconnected components via `compAlive`)
  , space :: [[(Continue, Progress)]] -- ^ unexplored solution stack, might help with parallelization; item per choice, per cursor. must be non-empty
  , unwinds :: [[Unwind]] -- ^ backtracking's "rewind"; an item per a solve. pop when (last 'space' == [])
  , maze :: MMaze
  } deriving (Eq, Ord, Generic)

data Constraints = Constraints
  { cLifespan :: Int
  , cDeterministic :: Bool
  , cBounds :: Maybe (Set Fursor)
  } deriving (Show, Eq, Ord, Generic)

-- | Island is the patch of unsolved pieces surrounded by solved pieces, computed by `flood` in `islands`.
data Island = Island
  { iId :: Int
  , iSize :: Int
  , iConts :: [Continue]
  , iBounds :: Set Fursor
  -- , iNeighb :: Int -- ^ number of distinct neighbouring components
  , iAp :: Bool
  } deriving (Show, Eq, Ord, Generic)

makeFieldOptics lensRules { _fieldToDef = (\_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase) } ''MMaze
makeFieldOptics lensRules { _fieldToDef = (\_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase) } ''Piece
makeFieldOptics lensRules { _fieldToDef = (\_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase) } ''Continue
makeFieldOptics lensRules { _fieldToDef = (\_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase) } ''Progress
makeFieldOptics lensRules { _fieldToDef = (\_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase) } ''Constraints
makeFieldOptics lensRules { _fieldToDef = (\_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase) } ''Island

-- | unlawful
instance Show Progress where
  show Progress{iter, priority} =
    "Progress" ++ show (iter, length priority)

-- | unlawful
instance Show MMaze where
  -- | unlawful instance
  show _ = "MMaze"

instance ToJSON Piece
instance ToJSON Continue
instance ToJSON Components
instance ToJSON Island

-- | From: https://hackage.haskell.org/package/monad-extras-0.6.0/docs/src/Control-Monad-Extra.html#iterateMaybeM
-- | Monadic equivalent to 'iterate', which uses Maybe to know when to
--   terminate.
iterateMaybeM :: Monad m => Int -> (a -> m (Maybe a)) -> a -> m [a]
iterateMaybeM 0 _ _ = pure []
iterateMaybeM n f x =
  maybe (return []) (\x' -> (x':) `liftM` iterateMaybeM (n - 1) f x') =<< f x

{--- MMaze and Matrix operations ---}

parse :: String -> IO MMaze
parse input = do
  maze <- (\b -> (MMaze b width height size zeros level [] mazeId)) <$> V.thaw (V.fromList (map snd board))
  (\m -> trivialsL (const (trivials m)) m) =<< boardL (const (setDeltas maze)) =<< pure maze
  where
    mazeId = showHex (foldr (\a b -> (ord a + b) `mod` (2 ^ 16)) 0 input) ""
    rect :: [[Pix]] = filter (not . null) . map (map toPix) . lines $ input
    board = map piece . zip [0..] . join $ rect
    piece (fc, p) = (fc, Piece p False fc False False 0)

    (width, height) = (length (head rect), (length rect))
    size = width * height
    zeros = floor (logBase 10 (fromIntegral size) + 1.5)
    level = fromMaybe 0 (lookup width [(8,1), (25,2), (50,3), (200,4), (400,5), (1000,6)])

    setDeltas m = V.thaw . V.fromList =<< traverse (\(fc, p) -> initChoicesL (const (choices m (fc, p))) p) board
    choices m (fc, _p) = do
      let c = mazeCursor width fc
      choices <- pieceChoices m c
      let next = filter (not . mazeBounded' width height . mazeDelta c) directions
      pure (choices + directionsPix next)

    trivials :: MMaze -> IO [Fursor]
    trivials MMaze{board} = map fst . filter trivial . zip [0..] . V.toList <$> V.freeze board
    trivial (_, Piece{pipe, initChoices}) = pipe == 0b11111111 || Bit.shiftR initChoices choicesCount < 2

mazeStore :: MMaze -> String -> IO ()
mazeStore m label = writeFile label =<< renderStr m

{-# INLINE mazeBounded #-}
mazeBounded :: MMaze -> Cursor -> Bool
mazeBounded MMaze{width, height} c = mazeBounded' width height c

mazeBounded' :: Int -> Int -> Cursor -> Bool
mazeBounded' width height (!x, !y) = x >= 0 && y >= 0 && width > x && height > y

vectorLists :: Int -> Int -> V.Vector a -> [[a]]
vectorLists width height board = [ [ board V.! (x + y * width) | x <- [0..width - 1] ] | y <- [0..height - 1] ]

mazeCursor :: Int -> Int -> Cursor
mazeCursor width = swap . flip quotRem width

{-# INLINE mazeRead #-}
mazeRead :: MMaze -> Fursor -> IO Piece
mazeRead MMaze{board} fc = MV.unsafeRead board fc

{-# INLINE mazeModify #-}
mazeModify :: MMaze -> (Piece -> Piece) -> Fursor -> IO ()
mazeModify MMaze{board} f fc = MV.unsafeModify board f fc

mazeClone :: MMaze -> IO MMaze
mazeClone = boardL MV.clone

{-# INLINE mazeSolve #-}
mazeSolve :: MMaze -> Continue -> IO Unwind
mazeSolve m Continue{char, cursor, island} =
  (cursor, ) <$> mazeRead m cursor <* mazeModify m modify cursor
  where modify p = p { pipe = char, solved = True, wasIsland = if island > 0 then True else False }

{-# INLINE mazeDelta #-}
mazeDelta :: Cursor -> Direction -> Cursor
mazeDelta (x, y) 0 = (x, y - 1)
mazeDelta (x, y) 1 = (x + 1, y)
mazeDelta (x, y) 2 = (x, y + 1)
mazeDelta (x, y) 3 = (x - 1, y)
mazeDelta _ _      = error "wrong direction"

{-# INLINE mazeFDelta #-}
mazeFDelta :: Int -> Fursor -> Direction -> Fursor
mazeFDelta w f 0 = f - w
mazeFDelta _ f 1 = f + 1
mazeFDelta w f 2 = f + w
mazeFDelta _ f 3 = f - 1
mazeFDelta _ _ _  = error "wrong direction"

mazeDeltasWalls :: MMaze -> Cursor -> IO [(Piece, Direction)]
mazeDeltasWalls m c = traverse (mazeDeltaWall m c) directions

{-# INLINE mazeDeltaWall #-}
mazeDeltaWall :: MMaze -> Cursor -> Direction -> IO (Piece, Direction)
mazeDeltaWall m@MMaze{width} c dir =
  if mazeBounded m delta
  then (, dir) <$> mazeRead m (x + y * width)
  else pure (Piece 0 True 0 True False 0, dir)
  where delta@(x, y) = mazeDelta c dir

{-# INLINE mazeEquate #-}
-- | Connects 'PartId's on the board
mazeEquate :: MMaze -> PartId -> [Fursor] -> IO [Unwind]
mazeEquate m partId cursors =
  traverse (\c -> (c, ) <$> mazeRead m c) cursors
  <* traverse (mazeModify m (\p -> p { partId, connected = True })) cursors

mazePop :: MMaze -> [Unwind] -> IO ()
mazePop m = traverse_ (uncurry (flip (mazeModify m . const)))

-- | Looks up the fixed point of 'PartId' (i.e. when it points to itself)
{-# INLINE partEquate #-}
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
    border = 3
    canvas = makeImageR VU ((width + 2) * pw, (height + 2) * ph) $ const (PixelRGB 0 0 0)
    grid = (,) <$> [0..width - 1] <*> [0..height - 1]

    colorHash :: Cursor -> Double
    colorHash (x, y) =
      let
        n = ((83 * fromIntegral x) / (37 * fromIntegral (y + 2))) + 0.5
        unfloor m = m - fromIntegral (floor m)
      in unfloor $ (unfloor n) / 4 - 0.15

    drawPiece :: MImage RealWorld VU RGB Double -> Cursor -> IO ()
    drawPiece image (x, y) = do
      let fc = x + y * width
      Piece{pipe, partId, solved} <- mazeRead maze fc
      ch <- colorHash . mazeCursor width <$> partEquate maze partId
      let cont = IntMap.lookup fc continues
      let colo = maybe ch (\c -> if island c == 2 then 0.25 else (if island c == 1 then 0.5 else 0.7)) cont
      let satu = if solved then 0.15 else (if isJust cont then 1 else 0)
      let inte = if solved then 0.5 else (if isJust cont then 1 else 0.3)
      let fill = if not solved && pipe == 0b11111111 then PixelRGB 1 1 1 else toPixelRGB $ PixelHSI colo satu inte
      write image (border + x * pw + 1, border + y * ph + 1) fill
      for_ (pixDirections pipe) $ \d ->
        when (Bit.testBit pipe d) $ write image (mazeDelta (border + x * pw + 1, border + y * ph + 1) d) fill

-- | The output format is: @images/lvl%i-%s-%0*i-%s.png level mazeId (sizeLen iter) name@
renderImage' :: String -> Progress -> IO Progress
renderImage' name p@Progress{maze=maze@MMaze{sizeLen, level, mazeId}, iter, continues} =
  p <$ renderImage (printf ("images/lvl%i-%s-%0*i-%s.png") level mazeId sizeLen iter name) maze continues

renderWithPositions :: Maybe Continue -> Progress -> IO String
renderWithPositions _ Progress{maze=maze@MMaze{board, width, height}} =
  pure . unlines . map concat . vectorLists width height =<< V.imapM fmt . V.convert =<< V.freeze board
  where
    colorHash = (`mod` 70) . (+15) . (\(x, y) -> x * 67 + y * 23)
    fmt _ Piece{pipe, partId, solved} = do
      color <- mfilter (\_ -> solved) . Just . colorHash . mazeCursor width <$> partEquate maze partId
      pure $ case color of
        Just color -> printf "\x1b[38;5;%im%c\x1b[39m" ([24 :: Int, 27..231] !! color) (toChar pipe)
        _ -> (toChar pipe) : []

render :: MMaze -> IO ()
render = (putStrLn =<<) . renderWithPositions Nothing . Progress 0 0 IntMap.empty IntMap.empty (Components IntMap.empty) [] []

renderStr :: MMaze -> IO String
renderStr MMaze{board, width, height} = do
  unlines . map concat . vectorLists width height . V.map (return . toChar . pipe) . V.convert
  <$> V.freeze board

islandCounts :: Progress -> (Int, Int)
islandCounts Progress{continues} = bimap getSum getSum . foldMap count $ IntMap.toList continues
  where
    icount i n = Sum (fromEnum (i == n))
    count (_, Continue{island=i}) = (icount i 2, icount i 1)

-- | Tracing with at each @F\REQ@th step via @T\RACE@ (both compile-time variables, use with with @ghc -D@).
--
-- Modes: 1. print stats \/ 2. print maze with terminal escape code codes \/ 3. as 2., but with clear-screen before \/
-- 4. as 1., but with image output \/ 5. as 4., but only after islands have started
traceBoard :: Continue -> Progress -> IO Progress
traceBoard current progress@Progress{iter, depth, maze=MMaze{size}} = do
  let (mode, freq) = (TRACE :: Int, FREQ :: Int)
  tracer mode freq False
  pure progress
  where
    tracer :: Int -> Int -> Bool -> IO ()
    tracer mode freq islandish
      | iter `mod` freq == 0 && mode == 1 = putStrLn solvedStr
      | iter `mod` freq == 0 && mode == 2 = traceStr >>= putStrLn
      | iter `mod` freq == 0 && mode == 3 = ((clear ++) <$> traceStr) >>= putStrLn
      | iter `mod` freq == 0 && mode == 4 = do
        tracer 1 FREQ_DEF False
        when islandish (void (renderImage' ("trace") progress))
      | iter `mod` freq == 0 && mode == 5 =
        tracer 4 freq (maybe False ((> 0) . island) $ findContinue progress)
      | True = pure ()

    perc = (fromIntegral $ depth) / (fromIntegral size) * 100 :: Double
    ratio = (fromIntegral iter / fromIntegral depth :: Double)
    (i, is) = islandCounts progress
    solvedStr = printf "\x1b[2Kislands: %2i/%2i, solved: %02.2f%%, ratio: %0.2f\x1b[1A" i is perc ratio

    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    traceStr = renderWithPositions (Just current) progress

{--- Model ---}

directions = [0, 1, 2, 3]
-- | > directions = rotations = [0, 1, 2, 3]
rotations = directions

charMapEntries :: [(Char, Pix)]
charMapEntries = map (_2 %~ (mirrorNibble . directionsPix))
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
  where mirrorNibble = (\n -> n + Bit.shiftL n 4) :: Pix -> Pix

charMap :: Map Char Pix
charMap = Map.fromList charMapEntries

pixMap :: Map Pix Char
pixMap = Map.fromList $ map swap charMapEntries

{-# INLINE pixRotations #-}
-- | This accounts for some piece's rotational symmetry
pixRotations :: Pix -> [Rotation]
pixRotations 0b11111111 = [0]
pixRotations 0b10101010 = [0,1]
pixRotations 0b01010101 = [0,1]
pixRotations _ = rotations

{-# INLINE pixDirections #-}
pixDirections :: Bit.Bits p => p -> [Direction]
pixDirections b = foldMap (\n -> if b `Bit.testBit` n then [n] else []) [0, 1, 2, 3]

{-# INLINE pixNDirections #-}
pixNDirections :: Bit.Bits p => p -> [Direction]
pixNDirections b = foldMap (\n -> if b `Bit.testBit` n then [] else [n]) [0, 1, 2, 3]

{-# INLINE choiceCount #-}
choiceCount :: Bit.Bits p => p -> Int
choiceCount b = getSum $ foldMap (\n -> Sum (if b `Bit.testBit` (n + choicesInvalid) then 0 else 1)) [0, 1, 2, 3]

{-# INLINE directionsPix #-}
directionsPix :: Integral i => [Direction] -> i
directionsPix = getSum . foldMap (Sum . (2 ^))

toPix = (charMap !) :: Char -> Pix
toChar = (pixMap !) :: Pix -> Char

{-# INLINE rotate #-}
rotate :: Rotation -> Pix -> Pix
rotate = flip Bit.rotateL

{--- Solver bits: per-pixel stuff ---}

-- given current pixel at rotation, does it match the pixel at direction from it?
{-# INLINE pixValid #-}
pixValid :: (Pix, Pix, Rotation, Direction) -> Bool
pixValid (!this, !that, !rotation, !direction) =
  not $ (rotate rotation this `Bit.xor` rotate 2 that) `Bit.testBit` direction

{-# INLINE validateDirection #-}
validateDirection :: Pix -> Rotation -> (Piece, Direction) -> Bool
validateDirection this rotation (Piece{pipe=that, solved}, direction) = do
  not solved || pixValid (this, that, rotation, direction)

{-# INLINE validateRotation #-}
validateRotation :: Pix -> [(Piece, Direction)] -> Rotation -> Bool
validateRotation this deltas rotation = all (validateDirection this rotation) deltas

{-# INLINE validateRotationM #-}
validateRotationM :: MMaze -> Cursor -> Pix -> Rotation -> IO Bool
validateRotationM maze cursor this rotation =
  (fmap (validateDirection this rotation) . mazeDeltaWall maze cursor) `allM` directions

{-# INLINE pieceChoices #-}
-- | Compute initial rotation fields for a piece's 'Choices'
pieceChoices :: MMaze -> Cursor -> IO Choices
pieceChoices maze@MMaze{width} cur@(x, y) = do
  Piece{pipe=this} <- mazeRead maze (x + y * width)
  valids <- foldMap (\d -> Sum (Bit.bit 4 + Bit.bit d)) <$> filterM (validateRotationM maze cur this) (pixRotations this)
  pure . flip Bit.shiftL choicesInvalid . Bit.xor 0b1111 . getSum $ valids

{--- Solver bits: components ---}

{-# INLINE compInsert #-}
compInsert :: Continue -> Components -> Components
compInsert Continue{origin} (Components c) = Components (IntMap.insertWith (+) origin 1 c)
compInsert Continue{origin, cursor} (Components' c) = Components' (IntMap.insertWith (IntSet.union) origin (IntSet.singleton cursor) c)

{-# INLINE compRemove #-}
compRemove :: PartId -> Fursor -> Components -> Components
compRemove origin _cursor (Components c) = Components (IntMap.update (Just . (subtract 1)) origin c)
compRemove origin cursor (Components' c) = Components' (IntMap.update (Just . IntSet.delete cursor) origin c)

{-# INLINE compEquate #-}
compEquate :: PartId -> [PartId] -> Components -> Components
compEquate hub connections c = equate c
  where
    {-# INLINE equate #-}
    equate (Components c) = Components $ equate' Sum getSum c
    equate (Components' c) = Components' $ equate' id id c

    {-# INLINE equate' #-}
    equate' :: Monoid m => (a -> m) -> (m -> a) -> IntMap a -> IntMap a
    equate' lift drop c = IntMap.insertWith (\a b -> drop (lift a <> lift b)) hub (drop sum) removed
      where (sum, removed) = foldr (flip (extract lift)) (mempty, c) connections

    {-# INLINE extract #-}
    extract lift (sum, m) part = IntMap.alterF ((, Nothing) . mappend sum . foldMap lift) part m

{-# INLINE compAlive #-}
compAlive :: PartId -> Components -> Bool
compAlive k (Components c) = (Just 1 ==) $ IntMap.lookup k c
compAlive k (Components' c) = (Just 1 ==) . fmap IntSet.size $ IntMap.lookup k c

{-# INLINE compConnected #-}
compConnected :: PartId -> Components -> [Fursor]
compConnected k (Components' c) = foldMap IntSet.toList (IntMap.lookup k c)
compConnected _ _ = []

{--- Solver bits: continues ---}

{-# INLINE deltaContinue #-}
deltaContinue :: Continue -> Int -> Fursor -> Direction -> Piece -> Maybe Continue -> Continue
deltaContinue Continue{char, origin, island, area} id c from Piece{pipe, initChoices} prev = do
  let pointed = char `Bit.testBit` from
  let origin' = if pointed then origin else c
  let island' = min 4 (island * 2) -- islands get bumped once to set area, next time to solve islands whole
  let dir = ((from + 2) `mod` 4)

  let initChoices' = maybe initChoices choices prev
  let validRot = pixNDirections (initChoices' `Bit.shiftR` choicesInvalid)
  let invalids = filter (\r -> pointed /= (Bit.testBit (rotate r pipe) dir)) validRot
  let choices' :: Int = foldr (\d s -> s - Bit.bit choicesCount + Bit.bit (choicesInvalid + d)) initChoices' invalids
  let solveds = Bit.bit (dir + choicesSolveds)
  -- let require = fromEnum pointed `Bit.shiftL` (dir + choicesRequire) -- not used

  Continue c pipe origin' 0 id island' area (choices' Bit..|. solveds)

{-# INLINE prioritizeDeltas #-}
-- | Calls 'prioritizeContinues' on nearby pieces (delta = 1)
prioritizeDeltas :: Int -> Progress -> Continue -> IO Progress
prioritizeDeltas width p@Progress{iter, maze} continue@Continue{cursor=cur, choices} = do
  (\f -> foldrM f p (zip [0..] (pixNDirections choices))) $ \(i, d) p' -> do
    prioritizeContinues p' (mazeFDelta width cur d) . deltaContinue continue (iter * 4 + i) (mazeFDelta width cur d) d
      <$> mazeRead maze (mazeFDelta width cur d)

{-# INLINE prioritizeIslands #-}
-- | Sets 'island' to @1@ to nearby components to activate when in island mode.
prioritizeIslands :: [Fursor] -> Progress -> IO Progress
prioritizeIslands _ p@Progress{components = (Components _)} = pure p
prioritizeIslands directDeltas p@Progress{maze, components = (Components' _)} = do
  shores <- filterM (fmap solvedSea . mazeRead maze) directDeltas
  foldr prioritizeIsland p <$> traverse (partEquate maze) shores
  where
    solvedSea Piece{solved, wasIsland} = solved && not wasIsland

    prioritizeIsland :: PartId -> Progress -> Progress
    prioritizeIsland k p =
      (\f -> foldr f p (compConnected k (components p))) $
        (\c p -> prioritizeContinues p c (set islandL 1 . fromJust))

{-# INLINE rescoreContinue #-}
-- | Recalculates the 'Continue's score, less is better (because of 'IntMap.deleteFindMin' in 'findContinue').
--
-- > score = (0 - island << 17 + (choices << (15 - choicesCount)) + x + y) << 32 + created
rescoreContinue :: Int -> Continue -> Continue
rescoreContinue width c@Continue{cursor, choices=choicesBits, island, area, created} = set scoreL score c
  where
    score = (0 - island << 27 + area << 15 + (choices << (12 - choicesCount)) + x + y) << 28 + created
    choices = choicesBits Bit..&. (0b11 << choicesCount)
    (<<) = Bit.shiftL
    (x, y) = mazeCursor width cursor

{-# INLINE prioritizeContinues #-}
-- | Inserts or reprioritizes 'Continue'
prioritizeContinues :: Progress -> Fursor -> (Maybe Continue -> Continue) -> Progress
prioritizeContinues progress@Progress{maze=MMaze{width}, priority, continues, components} cursor getCont =
  (\((p, cp), cn) -> progress { priority = p, components = cp, continues = cn }) $
    IntMap.alterF (insertContinue (priority, components) getCont) cursor continues
  where
    {-# INLINE insertContinue #-}
    insertContinue :: (Priority, Components) -> (Maybe Continue -> Continue) -> Maybe Continue -> ((Priority, Components), Maybe Continue)
    insertContinue (p, c) getCont Nothing =
      let new@Continue{cursor} = rescoreContinue width (getCont Nothing)
      in ((IntMap.insert (score new) cursor p, compInsert new c), Just new)
    insertContinue (p, c) getCont (Just (exists@Continue{cursor, created})) =
      ((contModify p, c), Just putback)
      where
        new@Continue{choices} = rescoreContinue width (getCont (Just exists)) { created }
        (putback, contModify) =
          if score new < score exists
          then (new, IntMap.insert (score new) cursor . IntMap.delete (score exists))
          else (exists { choices }, id)

{-# INLINE pieceDead #-}
-- | Check if 'Continue' is about to become separated from the rest of the graph.
pieceDead :: MMaze -> Components -> Fursor -> Pix -> Choices -> IO Bool
pieceDead maze components cur pix choices = do
  thisPart <- partEquate maze . partId =<< mazeRead maze cur
  pure (compAlive thisPart components && stuck)
  where stuck = 0 == ((0b1111 Bit..&. pix) Bit..&. Bit.complement (fromIntegral choices))

-- | Pops `priority` by `score`, deletes from `continues`.
{-# INLINE findContinue #-}
findContinue :: Progress -> Maybe Continue
findContinue Progress{priority, continues} = (snd <$> IntMap.lookupMin priority) >>= (`IntMap.lookup` continues)

popContinue :: Progress -> Progress
popContinue p@Progress{priority=pr, continues=c} = p { priority, continues = IntMap.delete cursor c }
  where ((_, cursor), priority) = IntMap.deleteFindMin pr

{--- Island computations ---}

-- | The generic /paint/ of the 'flood' fill.
type FillNext s = MMaze -> Cursor -> Piece -> [(Piece, Direction)] -> StateT s IO [Cursor]

-- | Four-way flood fill with 'FillNext' as the "paint". The initial piece is assumed to be valid FillNext.
flood :: Monoid s => FillNext s -> MMaze -> Cursor -> IO (Set Cursor, s)
flood n m = flip runStateT mempty . flood' n m Set.empty . return
  where
  flood' :: FillNext s -> MMaze -> Set Cursor -> [Cursor] -> StateT s IO (Set Cursor)
  flood' _ _ visited [] = pure visited
  flood' fillNext maze@MMaze{width=w} visited (cursor@(x, y):next) = do
    this <- liftIO (mazeRead maze (x + y * w))
    more <- fillNext maze cursor this =<< liftIO (mazeDeltasWalls maze cursor)
    let next' = filter (not . flip Set.member visited) more ++ next
    flood' fillNext maze (Set.insert cursor visited) next'

islandizeFirst :: Progress -> IO Progress
islandizeFirst p = do
  let firstIsland = snd <$> IntMap.lookupMin (priority p)
  pure (maybe p (\cursor -> prioritizeContinues p cursor (set islandL 1 . fromJust)) firstIsland)

islandizeArea :: Progress -> IO Progress
islandizeArea p = do -- (islandizeFirst =<<) $
  (reduce p . fst <$> islands p <*>) . pure $ \_i@Island{iConts} p ->
    reduce p iConts $ \Continue{cursor} p ->
      prioritizeContinues p cursor (set islandL 1 . fromJust) -- . set areaL (area _i) -- makes ratio significantly worse
  where
    -- area Island{iSize, iConts, iNeighb} = 0 -- no dumb
    reduce a l f = foldr f a l

islands :: Progress -> IO ([Island], IntMap Int)
islands Progress{maze=maze@MMaze{width}, continues} = do
  islands <- snd <$> foldIsland perIsland (map (mazeCursor width . cursor . snd) . IntMap.toList $ continues)
  pure (islands, foldMap (\Island{iId, iConts = cs} -> IntMap.fromList $ (, iId) <$> map cursor cs) islands)
  where
    foldIsland perIsland continues =
      (\acc -> foldrM acc (Set.empty, []) continues) $ \cursor acc@(visited, _) ->
        if (cursor `Set.member` visited) then pure acc else perIsland cursor acc

    fursorEmbed = (\(x, y) -> x + y * width)

    -- border = island's border by continues
    perIsland :: Cursor -> (Set Cursor, [Island]) -> IO (Set Cursor, [Island])
    perIsland cursor (visited, islands) = do
      (area, borders) <- flood (fillNextSolved continues) maze cursor
      let iConts = (continues IntMap.!) . fursorEmbed <$> (Set.toList borders)
      -- iNeigh <- length . nubOrd <$> traverse (partEquate maze . origin) iConts
      let iBounds = Set.map fursorEmbed area
      let island = Island (maybe 0 (\(x, y) -> x + y * width) (Set.lookupMin borders)) (Set.size area) iConts iBounds False
      pure (visited `Set.union` borders, island : islands)

    fillNextSolved :: Continues -> FillNext (Set Cursor)
    fillNextSolved continues _ cur@(x, y) _ deltasWall = do
      when ((x + y * width) `IntMap.member` continues) $ State.modify (Set.insert cur)
      pure . map (mazeDelta cur . snd) . filter (\(Piece{pipe, solved}, _) -> pipe /= 0 && not solved) $ deltasWall

graphIslands :: Progress -> IO (Gr Island String)
graphIslands Progress{components=Components _} = pure (mkGraph [] [])
graphIslands p@Progress{maze, components=Components' components} = do
  (islands, embedCursor) <- bimap id (IntMap.!) <$> islands p
  let
    nodes = map (\island@Island{iId} -> (iId, island)) islands
    connectedIslands island Continue{cursor, origin} =
      nubOrd . filter (/= island) . map embedCursor . filter (/= cursor)
      . foldMap IntSet.toList . (`IntMap.lookup` components) <$> partEquate maze origin
    islandContinues = islands >>= \Island{iId, iConts} -> (iId, ) <$> iConts
    edges = for islandContinues (\(id, continue) -> map ((id, , "")) <$> connectedIslands id continue)
  graph <- mkGraph nodes . join <$> edges
  pure (nmap (\i@Island{iId} -> i { iAp = iId `elem` (ap graph) }) graph)

previewIslands :: Progress -> IO Progress
previewIslands p = (p <$) . forkIO . void $ do
  graph <- graphToDot params { isDirected = False } <$> graphIslands p
  runGraphvizCanvas Neato graph Xlib
  runGraphvizCommand Neato graph DotOutput "images/graph.dot"
  where
    params = nonClusteredParams { fmtNode = \(_, Island{iSize, iAp}) ->
      [ Shape PointShape
      , Width (log (1.1 + (fromIntegral iSize) / 500))
      , NodeSep 0.01
      , Sep (PVal (createPoint 10 10))
      -- , Overlap ScaleXYOverlaps
      -- , Overlap VoronoiOverlap
      , Overlap (PrismOverlap (Just 4000))
      , LWidth 1000
      , Splines SplineEdges
      , Color [WC (if iAp then RGB 66 135 245 else RGB 252 160 18) Nothing]
      ]
    }

{--- Solver ---}

-- | Solves a valid piece, mutates the maze and sets unwind.
-- Inefficient access: partEquate reads the same data as islands reads.
-- (All methods within this method are inlined)
solveContinue :: Progress -> Continue -> IO Progress
solveContinue
  progress@Progress{maze=maze@MMaze{width}, components = components_}
  continue@Continue{cursor, char, origin = origin_} = do
    unwindThis <- mazeSolve maze continue
    thisPart <- partEquate maze origin_
    let directDeltas = map (mazeFDelta width cursor) $ pixDirections char
    neighbours <- fmap (nubOrd . (thisPart :)) . traverse (partEquate maze) $ directDeltas
    let origin = minimum neighbours
    let components = compEquate origin (filter (/= origin) (neighbours)) (compRemove thisPart cursor components_)
    unwindEquate <- mazeEquate maze origin neighbours

    progress' <- prioritizeIslands directDeltas =<< prioritizeDeltas width progress { components } continue { origin }
    traceBoard continue $ progress' & iterL %~ (+1) & depthL %~ (+1) & unwindsL %~ ((unwindEquate ++ [unwindThis]) :)

-- | The initial 'Progress', 'space' stack, 'Progress' and 'MMaze' backtracking operations.
-- This returns a progress with 'space' that always has an element or the maze isn't solvable
-- (assuming the algo's correct and the stack hasn't been split for divide and conquer).
backtrack :: Bool -> Progress -> IO (Maybe (Progress, Continue))
backtrack _ Progress{space=[]} = pure Nothing
backtrack _ Progress{space=([]:_), unwinds=[]} = pure Nothing
backtrack _ p@Progress{space=([]:space), unwinds=(unwind:unwinds), maze} = mazePop maze unwind >> backtrack True p { space, unwinds }
backtrack _popped Progress{space=(((continue, p):guesses):space), maze, unwinds, iter} = do
  pure (Just (p { maze, iter, unwinds, space = guesses : space }, continue))

-- | Solves pieces by backtracking, stops when the maze is solved, lifespan reached or first choice encountered.
solve' :: Constraints -> Progress -> IO (Maybe Progress)
solve' _ p@Progress{depth, maze=MMaze{size}} | depth == size = pure (Just p)
solve' cstrs@(Constraints life det _) progress@Progress{depth, maze=maze@MMaze{size}, components} = do
  guesses <- foldMap guesses (maybeToList (findContinue progress))
  progress' <- backtrack False . (spaceL %~ (guesses :)) =<< pure progress
  progress' <- traverse (uncurry (solveContinue . popContinue)) progress'

  let stop = depth == size - 1 || life == 0 || (det && (length guesses /= 1))
  next stop progress'
  where
    next True = pure
    next False = fmap join . traverse (solve' ((cLifespanL %~ (subtract 1)) cstrs))

    guesses continue@Continue{cursor, char, choices} = do
      let rotations = pixNDirections (Bit.shiftR choices choicesInvalid)
      rotations <- filterDisconnected (map (\r -> (cursor, rotate r char, choices)) rotations)
      pure (map (\(_, pipe, _) -> (set charL pipe continue, progress)) rotations)

    filterDisconnected = filterM $ \(cur, pix, choices) -> do
      disconnected <- pieceDead maze components cur pix choices
      pure ((depth == size - 1) || not disconnected)

initProgress :: MMaze -> IO Progress
initProgress m@MMaze{trivials} =
  let
    -- init continue created is probably not well scored, I don't understand why this doesn't break
    init = \(i, c) -> (\Piece{pipe, initChoices} -> Continue c pipe c 0 (-i) 0 0 initChoices) <$> mazeRead m c
    p = Progress 0 0 IntMap.empty IntMap.empty (Components IntMap.empty) [] [] m
  in foldr (\c@Continue{cursor} p -> prioritizeContinues p cursor (return c)) p <$> traverse init (zip [0..] trivials)

-- | Solver main, returns solved maze
solve :: MMaze -> IO MMaze
solve m = do
  p <- initProgress m
  p <- islandizeArea =<< componentRecalc True =<< fmap fromJust (solve' (Constraints (-1) True Nothing) p)
  -- p <- islandizeFirst =<< componentRecalc True =<< solve' (-1) True p
  -- previewIslands p
  -- iterateMaybeM (solve' (-1) False) p
  p <- fmap (maybe p id) . solve' (Constraints (-1) False Nothing) =<< traceIslands p
  -- p <- foldrM id p . replicate 10 $ (\p -> previewIslands =<< solve' 300000 False =<< traceIslands p)
  -- print . ap =<< graphIslands p

  let Progress{iter, depth, maze} = p
  putStrLn (printf "\x1b[2K%i/%i, ratio: %0.5f" iter depth (fromIntegral iter / fromIntegral depth :: Double))
  -- maze <$ renderImage' "done" solved
  pure maze
  where
    traceIslands = renderImage' "islandize"

    -- Progress.components = Components -> Components'
    componentRecalc :: Bool -> Progress -> IO Progress
    componentRecalc deep p@Progress{maze, continues} = do
      comps <- foldr (IntMap.unionWith IntSet.union) IntMap.empty <$> traverse component (IntMap.toList continues)
      pure . (\c -> p { components = c }) $
        if deep then Components' comps else Components (IntMap.map IntSet.size comps)
      where component (_, Continue{origin, cursor}) = IntMap.singleton <$> partEquate maze origin <*> pure (IntSet.singleton cursor)

{--- Main ---}

verify :: MMaze -> IO Bool
verify maze@MMaze{size} = do
  (size ==) . Set.size . fst <$> flood fillNextValid maze (0, 0)
  where
    fillNextValid :: FillNext ()
    fillNextValid maze cur Piece{pipe=this} deltasWalls = pure $
      if validateRotation this deltasWalls 0
      then filter (mazeBounded maze) . map (mazeDelta cur) $ pixDirections this
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
    rotations MMaze{width, board=input} MMaze{board=solved} = do
      V.toList . V.imap (\i -> (mazeCursor width i, ) . uncurry (rotations `on` pipe))
      <$> (V.zip <$> V.freeze input <*> V.freeze solved)
      where
        rotations from to = fromJust $ to `elemIndex` iterate (rotate 1) from

-- | Gets passwords for solved levels from the maze server.
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

-- | Run solver, likely produce trace output and complain if solve is invalid ('verify').
solveFile :: String -> IO ()
solveFile file = do
  solved <- solve =<< parse =<< readFile file
  whenM (not <$> verify solved) (putStrLn "solution invalid")

-- | Executable entry point.
main :: IO ()
main = do
  websocket <- (== "1") . fromMaybe "0" <$> lookupEnv "websocket"

  if websocket
  then withSocketsDo $ WS.runClient "maze.server.host" 80 "/game-pipes/" pļāpātArWebsocketu
  else traverse_ solveFile . (\args -> if null args then ["/dev/stdin"] else args) =<< getArgs
