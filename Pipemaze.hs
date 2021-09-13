{-# LANGUAGE TupleSections, NamedFieldPuns, BinaryLiterals, TemplateHaskell, CPP, ScopedTypeVariables, NumericUnderscores, FlexibleInstances #-}

{-# LANGUAGE StrictData, BangPatterns #-}
{-# OPTIONS_GHC -O #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-} -- no observable effect from these
{-# OPTIONS_GHC -fspecialise-aggressively #-} -- no observable effect from these

{-# LANGUAGE RankNTypes #-} -- for maybeSet

{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-unused-do-bind -Wno-type-defaults -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- complains about unused lenses :/

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

A dynamic solver of pipe mazes with an internal 'priority' queue based on scores, position and priority creation order and much more.
-}

module Pipemaze (
  -- * High-level description
  -- $description

  -- * Islands
  -- $islands

  -- * Bugs and improvements
  -- $bugs

  -- * Types
  Direction, Rotation, Pix, Cursor, Fursor, MMaze(..), Piece(..), Choices, PartId, Continue(..)
  , Priority, Continues, Components(..), Unwind(..), Progress(..), Island(..), IslandSolution(..)
  , Bounds, SolveMode(..), Configuration(..), Solver, SolverT
  -- * Maze operations
  , parse, mazeStore, mazeBounded, mazeCursor, mazeFursor, mazeRead, mazeModify
  , mazeClone, mazeSolve, mazeDelta, mazeFDelta, mazeEquate, mazePop, partEquate
  -- * Tracing and rendering
  , renderColor, renderStr, renderImage'
  , traceBoard
  -- * Pixel model
  , directions, rotations, charMap, pixMap, pixRotations, pixDirections, directionsPix, toPix, toChar, rotate
  -- * Pixel solving
  , pixValid, validateDirection, pieceChoices
  -- * Component indexing
  , compInsert, compRemove, compEquate, compAlive, compConnected, compCounts
  -- * Continue operations
  , deltaContinue, prioritizeDeltas, rescoreContinue, prioritizeContinue, prioritizeContinues
  , pieceDead, findContinue, popContinue
  -- * Island computations
  , FillNext, flood, islandize, islandConnectivityRefinement, islandChoices, islands
  -- * Backtracker
  , solveContinue, backtrack, solve'
  -- * Metasolver
  , islandChoicesParallel, solveDetParallel, initProgress, solve
  -- * Main
  , verify, storeBad, rotateStr, configuration, pļāpātArWebsocketu, solveFile, main
) where

-- solver

-- Lens TemplateHaskell
import Control.Lens.Internal.FieldTH (makeFieldOptics, LensRules(..))
import Language.Haskell.TH.Syntax (mkName, nameBase)
import Control.Lens.TH (DefName(..), lensRules)

-- Solver
import Algebra.PartialOrd (PartialOrd(..))
import Control.Concurrent (getNumCapabilities)
import Control.Lens (Setter', (&), (%~), set, _1, _2, _head, _Just)
import Control.Monad.Extra (allM, whenM)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad (join, filterM, void, unless, when, mfilter, replicateM, (<=<))
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Reader (MonadReader(..), Reader, ReaderT(..), ask, asks, withReaderT, mapReaderT)
import Control.Monad.Trans.State.Strict (StateT(..))
import Data.Char (ord)
import Data.Foldable (traverse_, for_, foldlM, fold)
import Data.Function (on)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.List.Extra (nubOrd, groupSort, groupSortOn, splitOn, chunksOf, intersperse, transpose)
import Data.Map.Strict (Map, (!))
import Data.Maybe (fromMaybe, fromJust, isJust, maybeToList)
import Data.Monoid (Sum(..))
import Data.Set (Set)
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Vector.Storable.Mutable (IOVector)
import Data.Word (Word8)
-- import Debug.Trace (trace)
import Foreign.Storable.Generic
import Graphics.Image.Interface (thaw, MImage, freeze, write)
import Graphics.Image (writeImage, makeImageR, Pixel(..), toPixelRGB, VU(..), RGB)
import Numeric (showHex)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bits as Bit
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.POSet as POSet
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import System.Clock (getTime, Clock(Monotonic), diffTimeSpec, toNanoSecs, TimeSpec)
import System.Environment (lookupEnv, getArgs)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- parallel
import Control.Concurrent.ParallelIO.Global (parallelInterleaved)

-- json for debug outputs
import Data.Aeson (ToJSON(..))
-- import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Generics (Generic)

-- Main IO
import Data.Text (Text)
import Network.Socket (withSocketsDo)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.IO (hFlush, stdout)

-- t a = seq (trace (show a) a) a
-- t' l a = seq (trace (show l ++ ": " ++ show a) a) a
-- tM :: (Functor f, Show a) => f a -> f a
-- tM = fmap t

-- $description
-- The goal of the solver is to find the 'Rotation' of each 'Pix' in the grid to create a connected undirected graph.
-- Just like in this game here: <https://www.puzzle-pipes.com/>
--
-- * Each cursor has four nearby cursors – top, right, bottom, left, which wil be refered to as 'directions' or 'deltas'.
-- * The 'Cursor's in one square's distance in a 'direction' with respect to some cursor @c@ may also be called 'deltas'.
-- * The number of valid rotations of a piece are encoded in 'Choices'.
-- * Each 'Piece' contains initial 'Choices' which are bit-packed info 'initChoices',
-- * Distinct graphs are being refered to as /components/ and each has a distinct 'PartId' computed by the smallest 'Fursor' of the graph.
--  <https://en.wikipedia.org/wiki/Component_(graph_theory)>
-- * All 'Component\''s open ends are refered to as 'Continue's,
--  which then later get stored in 'Continue' to make a piece "active", making 'Piece' information is no longer necessary.
-- * The cursors are stored as flat indexes ('Fursor') to make comparison computations faster ('IntMap'). Conversions to 'Cursor'
--  are possible via 'mazeCursor', 'mazeFursor'.
-- * 'Progress' holds main data of the backtracker, the only mutable data is 'MMaze', which stores correctly solved 'Pix' and
--  'partId's which get updated as graphs become joined.
--
-- Solving each piece gives you hints about surrounding pieces,
-- so solving them by diagonals ('rescoreContinue') is more effective than solving in an arbitrary order.
-- If the connectivity of the pieces is efficiently computed ('PartId' \/ 'partEquate'),
-- the "open ends" ('Continues') have a good prioritization and the disconnected solves are efficiently (computed
-- and then) discarded ('pieceDead' \/ 'compAlive'), you can solve around 98% of the level 6 maze determinstically.

-- $islands
-- Island is a patch of unsolved pieces, each one has its own number of solutions.
-- Solving it by backtracking makes the search space multiply after guessing one island after another in a single 'Progress'.
-- Instead, you can force the backtracker to only count the solutions of a single island with 'cBounds'.
-- If you group the solutions ('islandConnectivityRefinement'), many islands have a single best or equivalent solution.
-- 'solveTrivialIslands' runs the backtracker for all "simple" islands, then recomputes all islands, then repeats.
-- The even simpler 'solveIslandStatic' solves all the pieces that remain the same in all 'IslandSolution's.
-- Running either solves level 6 very fast and 'solveIslandStatic' (+ 'solve') is a little faster.
--
-- Grouping in 'islandConnectivityRefinement' takes place in two steps: group by equal 'Components',
-- refine 'icConnections' by their partition 'PartialOrd'.

-- $bugs
-- * `partId`/`origin` is a little awkward, but `pipe`/`char` is even more so
-- * 'Continue's can be removed, if their old versions are added in 'Unwind's
-- * A separate priority list could be mae for choices = 1
-- * All 'IslandSolution's are getting recomputed after each determinstic solve, which can be fixed, but it's already very fast
-- * 'IslandSolution' could be recomputable without running main solver,
--  just by checking that nothing gets disconnected after running it through all the 'Unwind's of last solve.
-- * Backtracking on 'IslandSolution' could be implemented
-- * 'IslandSolution's could be chosen by just checking that the 'icConnections' connect some graph constructed from islands.
-- * 'IslandSolution' could have a heuristic for the number of solutions without solving all
--  solutions by solving in breadth-wise first choices – '[Progress]' and only then depth-wise.

-- | Directions: top 0, right 1, bottom 2, left 3
type Direction = Int
-- | The set of rotation values are the same as directions.
type Rotation = Int
-- | The maze symbol (has four edges) bit-packed in 'charMap' as @2^d@ per direction, mirrored in @shiftL 4@ to help bit rotation
--
-- > X – ╋          -- 4 edges
-- > I – ━, ┃       -- 2 edges
-- > L – ┏, ┛, ┓, ┗ -- 2 edges
-- > T – ┣, ┫, ┳, ┻ -- 3 edges
-- > i – ╸, ╹, ╺, ╻ -- 1 edge
type Pix = Word8
type Cursor = (Int, Int)
-- | Flat cursors, for @f :: FCursor, (x, y) :: Cursor, f = x + y * width@. Through `Choices`, bound checking is cached, so deltas are just ±1 or ±width.
type Width = Int
type Fursor = Int

-- | unlawful instance
instance Eq (IOVector Piece) where _ == _ = True
-- | unlawful instance
instance Ord (IOVector Piece) where _ <= _ = True

-- | Mutable maze operated on by functions in section /"Maze operations"/
data MMaze = MMaze
  { board :: IOVector Piece -- ^ flat MVector with implied 2d structure via 'Cursor'/'Fursor' + index computations
  , width :: Int
  , height :: Int
  , size :: Int
  , sizeLen :: Int -- ^ leading char count for @printf %0ni@ format @(~logBase 10 size + 1.5)@
  , level :: Int
  , trivials :: [Fursor] -- ^ cursors of the edge and @X@ pieces which have only one valid rotation
  , mazeId :: String -- ^ 'board's data scrambled into a 4-byte hexadecimal field
  , time :: TimeSpec
  } deriving (Eq, Ord, Generic)

data Piece = Piece
  { pipe :: Pix
  , solved :: Bool
  , partId :: PartId -- ^ meaningless if not connected
  , connected :: Bool -- ^ connected when pointed to by a 'solved' piece
  , initChoices :: Choices
  } deriving (Show, Eq, Ord, Generic)

instance GStorable Piece

-- | 'Choices' is bit-packed info related to the valid rotations of a picce.
-- In MSB order: (valid) rotation count 2b, invalid rotation directions 4b (unused), solved requirements 4b, solved neighbours 4b
type Choices = Int
(choicesSolveds, choicesInvalid, choicesCount) = (0, 4, 8)

-- | Continue represents the piece that should be solved next according to 'Priority', which is an open end of a component
-- (or starts one). Created in 'initProgress' or 'deltaContinue'.
data Continue = Continue
  { cursor :: Fursor
  , char :: Pix -- ^ from Piece at 'cursor'
  , origin :: PartId -- ^ component id, to be used with 'partEquate'
  , score :: Int -- ^ see 'rescoreContinue'
  , created :: Int -- ^ any unique id to make score order total ('Ord' requirement), taken from 'iter'
  , island :: Int -- ^ \> 0 if island
  , area :: Int -- ^ island score, 0 if not an island
  , choices :: Choices
  } deriving (Show, Eq, Ord, Generic)

-- | 'PartId' distinguishes the graph component by their smallest known 'Cursor' by its 'Ord' (unique),
-- so it is the same as its cursor initially. They're marked in 'origin' ahead of 'solved's.
-- 'PartId' in 'origin' is only to be used through 'partEquate', because 'origin' isn't being
-- updated after components have connected.
type PartId = Fursor

-- | 'Continue' priority queue, inserted by 'prioritizeContinue', found by 'findContinue', popped by 'popContinue'.
type Priority = IntMap Fursor
-- | Primary storage of 'Continue' data
type Continues = IntMap Continue
-- | The index of components' continues by their 'PartId' (which are always up-to-date).
data Components
  = Components (IntMap Int) -- ^ marginally faster, but less info
  | Components' (IntMap IntSet)
  deriving (Show, Eq, Ord, Generic)

type Space = [([(Continue, Progress)], [Unwind])]

-- | For backtracking on the mutable 'MMaze' and for extracting hints.
data Unwind
  = UnSolve Fursor Pix Pix -- ^ 'Pix' before, after (for deployHint)
  | UnEquate Fursor Bool PartId -- ^ 'connected', 'PartId' after
  deriving (Show, Eq, Ord, Generic)

data Progress = Progress
  { iter :: Int -- ^ the total number of backtracking iterations (incl. failed ones)
  , depth :: Int -- ^ number of solves, so also the length of unwinds/space
  , priority :: Priority -- ^ priority queue for next guesses (tree, not a heap, because reprioritizing is required)
  , continues :: Continues -- ^ Primary continue store, pointed to by 'priority' (all 'Continue's within must be unique by their cursor)
  , components :: Components -- ^ component continue counts (for quickly computing disconnected components via `compAlive`)
  , space :: Space -- ^ backtracking's "rewind" + unexplored solution stack; an item per a solve. pop when (last 'space' == [])
  , maze :: MMaze
  } deriving (Eq, Ord, Generic)

type PrioCompCont = (Priority, Components, Continues)

-- | unlawful
instance Show Progress where
  show Progress{depth, iter} =
    "Progress" ++ show (depth, '/', iter)

type Bounds = Maybe (Fursor -> Bool)

bounded :: Bounds -> Fursor -> Bool
bounded b c = all ($ c) b

-- | Amalgamation of the flags "determinstic", "save history" and "deprioritize unbounded continues"
-- (it's for parallelism, see 'rescoreContinue').
data SolveMode = SolveNormal | SolveDeterministic | SolveIslandDeterministic | SolveParallel deriving (Show, Eq, Ord)

solveDeterministic SolveNormal = True
solveDeterministic _ = False

solveWithHistory SolveNormal = True
solveWithHistory SolveIslandDeterministic = False -- could be True if you plan to solve islands directly (reply unwinds)
solveWithHistory _ = False

data Configuration = Configuration
  { cDebug :: Int
  , cDebugFreq :: Int
  , cPixSize :: Int
  , cLifespan :: Int
  , cMode :: SolveMode
  , cBounds :: Bounds -- ^ forces the solver to stay within bounds
  , cBench :: Bool
  , cImageDir :: String
  , cNumCap :: Int
  } -- deriving (Show, Eq, Ord, Generic)

type SolverT = ReaderT Configuration IO
type Solver = Reader Configuration

-- | Island is the patch of unsolved pieces surrounded by solved pieces, computed by 'flood' in 'islands'.
data Island = Island
  { iId :: Int
  , iSize :: Int
  , iConts :: [Continue]
  , iBounds :: IntSet
  , iSolutions :: [IslandSolution]
  -- ^ all possible combinations (partitioned by partition equivalence), with hints to force solver choose that solve
  , iChoices :: Int -- ^ same, but without details
  } deriving (Show, Eq, Ord, Generic)

-- | IslandSolution represent a solution for an island with a representative progress.
-- The 'icConnections' are a partition of the components the island joined.
-- Partitions have a partial ordering called _refinement_ with which you can group 'iSolutions'.
-- <https://en.wikipedia.org/wiki/Partition_of_a_set#Refinement_of_partitions>
data IslandSolution = IslandSolution
  -- { icProgess :: Progress
  { icConnections :: [Set PartId] -- the surrounding 'PartId' partition (induces a PartialOrd)
  , icComponents :: IntMap Int
  , icHints :: [Unwind] -- ^ forces the backtracker to choose this solution
  } deriving (Show, Eq, Ord, Generic)

instance PartialOrd IslandSolution where
  IslandSolution{icConnections=as} `leq` IslandSolution{icConnections=bs} =
    all (flip any bs . Set.isSubsetOf) as

makeFieldOptics lensRules { _fieldToDef = \_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase } ''MMaze
makeFieldOptics lensRules { _fieldToDef = \_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase } ''Piece
makeFieldOptics lensRules { _fieldToDef = \_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase } ''Continue
makeFieldOptics lensRules { _fieldToDef = \_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase } ''Progress
makeFieldOptics lensRules { _fieldToDef = \_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase } ''Configuration
makeFieldOptics lensRules { _fieldToDef = \_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase } ''Island

toSolverT = mapReaderT (pure . runIdentity)
determinstically = withReaderT (set cModeL SolveDeterministic)
determinsticallyI = withReaderT (set cModeL SolveIslandDeterministic) -- for islands, see withHistory

confDefault = Configuration
  { cDebug = 0
  , cDebugFreq = 10377
  , cPixSize = 3
  , cLifespan = - 1
  , cMode = SolveNormal
  , cBounds = Nothing
  , cBench = False
  , cImageDir = "images/"
  , cNumCap = 1
  }

-- | unlawful
instance Show MMaze where
  -- | unlawful instance
  show _ = "MMaze"

instance ToJSON Piece
instance ToJSON Continue
instance ToJSON Components
instance ToJSON Island
instance ToJSON Unwind
instance ToJSON IslandSolution
-- writeFile "out" . LBS.unpack . Aeson.encode . toJSON $ solutions

-- | https://hackage.haskell.org/package/monad-extras-0.6.0/docs/src/Control-Monad-Extra.html#iterateMaybeM
-- | Monadic equivalent to 'iterate', which uses Maybe to know when to terminate.
iterateMaybeM :: Monad m => Int -> (a -> m (Maybe a)) -> a -> m (Bool, [a])
iterateMaybeM 0 _ _ = pure (True, [])
iterateMaybeM n f x = fmap (False, ) $
  maybe (return []) (\x' -> ((x':) . snd) `fmap` iterateMaybeM (n - 1) f x') =<< f x

{--- MMaze and Matrix operations ---}

parse :: String -> IO MMaze
parse input = do
  time <- getTime Monotonic
  maze <- (\b -> MMaze b width height size zeros level [] mazeId time) <$> V.thaw (V.fromList (map snd board))
  (\m -> trivialsL (const (trivials m)) m) =<< boardL (const (setDeltas maze)) =<< pure maze
  where
    mazeId = showHex (foldr (\a b -> (ord a + b) `mod` (2 ^ 16)) 0 input) ""
    rect :: [[Pix]] = filter (not . null) . map (map toPix) . lines $ input
    board = zipWith (curry piece) [0..] . join $ rect
    piece (fc, p) = (fc, Piece p False fc False 0)

    (width, height) = (length (head rect), length rect)
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

mazeStore :: MonadIO m => MMaze -> String -> m ()
mazeStore m label = liftIO (writeFile label =<< renderStr m)

{-# INLINE mazeBounded #-}
mazeBounded :: MMaze -> Cursor -> Bool
mazeBounded MMaze{width, height} = mazeBounded' width height

mazeBounded' :: Int -> Int -> Cursor -> Bool
mazeBounded' width height (!x, !y) = x >= 0 && y >= 0 && width > x && height > y

vectorLists :: Storable a => Int -> Int -> V.Vector a -> [[a]]
vectorLists width height board = [ [ board V.! (x + y * width) | x <- [0..width - 1] ] | y <- [0..height - 1] ]

{-# INLINE mazeCursor #-}
mazeCursor :: Width -> Fursor -> Cursor
mazeCursor width = swap . flip quotRem width

{-# INLINE mazeFursor #-}
mazeFursor :: Width -> Cursor -> Fursor
mazeFursor w (x, y) = x + y * w

{-# INLINE mazeRead #-}
mazeRead :: MonadIO m => MMaze -> Fursor -> m Piece
mazeRead MMaze{board} fc = liftIO (MV.unsafeRead board fc)

{-# INLINE mazeModify #-}
mazeModify :: MonadIO m => MMaze -> (Piece -> Piece) -> Fursor -> m ()
mazeModify MMaze{board} f fc = liftIO $ MV.unsafeModify board f fc

mazeClone :: MonadIO m => MMaze -> m MMaze
mazeClone = liftIO . boardL MV.clone

{-# INLINE mazeSolve #-}
mazeSolve :: MonadIO m => MMaze -> Continue -> m Unwind
mazeSolve MMaze{board} Continue{char=after, cursor} = do
  p@Piece{pipe=before} <- liftIO (MV.unsafeRead board cursor)
  liftIO $ MV.unsafeWrite board cursor p { pipe = after, solved = True }
  pure (UnSolve cursor before after)

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
  else pure (Piece 0 True 0 True 0, dir)
  where delta@(x, y) = mazeDelta c dir

{-# INLINE mazeEquate #-}
-- | Connects 'PartId's on the board
mazeEquate :: MonadIO m => MMaze -> PartId -> [Fursor] -> m [Unwind]
mazeEquate MMaze{board} partId cursors = liftIO $
  for cursors $ \cursor -> do
    p@Piece{connected, partId=partId_} <- liftIO (MV.unsafeRead board cursor)
    liftIO $ MV.unsafeWrite board cursor p { partId, connected = True }
    pure (UnEquate cursor connected partId_)

{-# INLINE mazePop #-}
mazePop :: MonadIO m => MMaze -> Unwind -> m ()
mazePop m (UnSolve c pipe _) = mazeModify m (\p -> p { pipe, solved = False }) c
mazePop m (UnEquate c connected partId) = mazeModify m (\p -> p { partId, connected }) c

-- | Looks up the fixed point of 'PartId' (i.e. when it points to itself)
{-# INLINE partEquate #-}
partEquate :: MonadIO m => MMaze -> PartId -> m PartId
partEquate maze v = loop' =<< find v
  where
    find f = (\Piece{connected, partId} -> if connected then partId else f) <$> mazeRead maze f
    loop' v' = (\found -> if v' == v || v' == found then pure v' else loop' found) =<< find v'

{--- Rendering, tracing ---}

-- | Generate uncolorized output
renderStr :: MMaze -> IO String
renderStr MMaze{board, width, height} =
  unlines . map (concatMap (return . toChar . pipe)) . vectorLists width height . V.convert <$> V.freeze board

{- HLINT ignore renderImageN -}
renderImageN :: MonadIO m => Int -> String -> MMaze -> Continues -> m ()
renderImageN pixSize fn maze@MMaze{width, height} continues = liftIO $ seq continues $ do
  mcanvas <- thaw canvas :: IO (MImage RealWorld VU RGB Double)
  traverse_ (drawPiece mcanvas) grid
  writeImage fn =<< freeze mcanvas
  where
    (pixW, pixH) = (pixSize, pixSize)
    border = pixSize
    canvas = makeImageR VU ((width + 2) * pixW, (height + 2) * pixH) $ const (PixelRGB 0 0 0)
    grid = (,) <$> [0..width - 1] <*> [0..height - 1]

    colorHash :: Cursor -> Double
    colorHash (x, y) =
      let
        n = ((83 * fromIntegral x) / (37 * fromIntegral (y + 2)))
        unfloor m = m - fromIntegral (floor m)
      in unfloor n

    drawPiece :: MImage RealWorld VU RGB Double -> Cursor -> IO ()
    drawPiece image (x, y) = do
      let fc = x + y * width
      Piece{pipe, partId, solved} <- mazeRead maze fc
      ch <- colorHash . mazeCursor width <$> partEquate maze partId
      let cont = IntMap.lookup fc continues
      let colo = maybe ch (\c -> if island c == 2 then 0.25 else 0.6) cont
      let satu = if solved then 0.8 else (if isJust cont then 0.8 else 0)
      let inte = if solved then 0.5 else (if isJust cont then 1 else 0.3)
      let fill = if not solved && pipe == 0b11111111 then PixelRGB 1 1 1 else toPixelRGB $ PixelHSI colo satu inte
      write' pixSize pipe fill

      where
        write' :: Int -> Pix -> Pixel RGB Double -> IO ()
        write' 3 pipe fill = do
          write image (border + x * pixW + 1, border + y * pixH + 1) fill
          for_ (pixDirections pipe) $ \d ->
            when (Bit.testBit pipe d) $
              write image (mazeDelta (border + x * pixW + 1, border + y * pixH + 1) d) fill
        write' 1 _ fill = write image (border + x * pixW, border + y * pixH) fill
        write' _ _ _ = error "pixSize bad"

renderImage :: String -> MMaze -> Continues -> SolverT ()
renderImage s m c = asks cPixSize >>= \ps -> renderImageN ps s m c

-- | The output format is: @images/lvl%i-%s-%0*i-%s.png level mazeId (sizeLen iter) name@
renderImage' :: String -> Progress -> SolverT Progress
renderImage' name p@Progress{maze=maze@MMaze{sizeLen}, iter, continues} =
  (p <$) . whenM (asks (not . cBench)) $ do
    dir <- asks cImageDir
    renderImage (printf (dir ++ "%0*i-%s.png") sizeLen iter name) maze continues

renderColorProgress :: MonadIO m => Maybe Continue -> Progress -> m String
renderColorProgress _ Progress{maze=maze@MMaze{board, width, height}} = liftIO $ do
  lines <- vectorLists width height . V.convert <$> V.freeze board
  unlines . map concat <$> traverse (traverse fmt) lines
  where
    colorHash = (`mod` 70) . (+15) . (\(x, y) -> x * 67 + y * 23)
    fmt Piece{pipe, partId, solved} = do
      color <- mfilter (const solved) . Just . colorHash . mazeCursor width <$> partEquate maze partId
      pure $ case color of
        Just color -> printf "\x1b[38;5;%im%c\x1b[39m" ([24 :: Int, 27..231] !! color) (toChar pipe)
        _ -> [toChar pipe]

-- | Print unicode maze with colorized ANSI escape sequences to stdout.
renderColor :: MonadIO m => MMaze -> m ()
renderColor = liftIO . (putStrLn <=< renderColorProgress Nothing . Progress 0 0 IntMap.empty IntMap.empty (Components IntMap.empty) [])

-- | Tracing at each @freq=@ step with @debug=@ environment variables.
--
-- Modes: 1. print stats \/ 2. print maze with terminal escape code codes \/ 3. as 2., but with clear-screen before \/
-- 4. as 1., but with image output \/ 5. as 4., but only after islands have started
traceBoard :: Continue -> Progress -> SolverT Progress
traceBoard current progress@Progress{iter=iter', depth, maze=maze@MMaze{size}} = do
  Configuration{cDebug, cDebugFreq} <- ask
  islands <-
    if iter `mod` max 1 (cDebugFreq `div` islandSlowdown) == 0
    then any ((> 0) . island) <$> toSolverT (findContinue progress)
    else pure False
  progress <$ tracer cDebug (max 1 (cDebugFreq `div` (if islands then islandSlowdown else 1))) islands
  where
    (iter, islandSlowdown) = (iter' - 1, 50)
    tracer :: Int -> Int -> Bool -> SolverT ()
    tracer mode freq islandish
      | iter `mod` freq == 0 && mode == 0 = liftIO $ putStrLn solvedStr
      | iter `mod` freq == 0 && mode == 1 = liftIO $ renderStr maze >>= putStrLn
      | iter `mod` freq == 0 && mode == 2 = liftIO $ traceStr >>= putStrLn
      | iter `mod` freq == 0 && mode == 3 = liftIO $ traceStr >>= putStrLn . (clear ++)
      | iter `mod` freq == 0 && mode == 4 = tracer 1 freq False >> void (renderImage' "trace" progress)
      | iter `mod` freq == 0 && mode == 5 = if islandish then tracer 4 freq True else pure ()
      | otherwise = pure ()

    perc = fromIntegral depth / fromIntegral size * 100 :: Double
    ratio = fromIntegral iter / fromIntegral depth :: Double
    solvedStr = printf "\x1b[2Ksolved: %02.2f%%, ratio: %0.2f\x1b[1A" perc ratio

    clear = "\x1b[H\x1b[2K" -- move cursor 1,1; clear line
    traceStr = renderColorProgress (Just current) progress

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
pixRotations 0b00000000 = [0]
pixRotations 0b11111111 = [0]
pixRotations 0b10101010 = [0, 1]
pixRotations 0b01010101 = [0, 1]
pixRotations _ = rotations

{-# INLINE pixDirections #-}
pixDirections :: Bit.Bits p => p -> [Direction]
pixDirections b = foldMap (\n -> [n | b `Bit.testBit` n]) [0, 1, 2, 3]

{-# INLINE pixNDirections #-}
pixNDirections :: Bit.Bits p => p -> [Direction]
pixNDirections b = foldMap (\n -> [n | not (b `Bit.testBit` n)]) [0, 1, 2, 3]

{-# INLINE directionsPix #-}
directionsPix :: Integral i => [Direction] -> i
directionsPix = getSum . foldMap (Sum . (2 ^))

toPix = (charMap !) :: Char -> Pix
toChar = (pixMap !) :: Pix -> Char

{-# INLINE rotate #-}
-- | Rotates the 'Pix' to left by n 'Rotation's
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
pieceChoices maze@MMaze{width, height} cur@(x, y) = do
  Piece{pipe} <- mazeRead maze (mazeFursor width cur)
  if edge || length (pixRotations pipe) < 4
  then do
    valids <- foldMap choiceBits <$> filterM (validateRotationM maze cur pipe) (pixRotations pipe)
    pure . flip Bit.shiftL choicesInvalid . Bit.xor 0b1111 . getSum $ valids
  else pure (Bit.shiftL 4 choicesCount)
  where
    edge = (x + 1) `mod` width < 2 || ((y + 1) `mod` height < 2)
    choiceBits d = Sum (Bit.bit 4 + Bit.bit d)

forceChoice :: Pix -> Pix -> Choices -> Choices
forceChoice forced pix choices =
  let
    rotatation = fromJust (List.find (\r -> rotate r pix == forced) rotations)
    exceptSolveds = Bit.shiftL 0b1111 choicesSolveds
  in
    (exceptSolveds Bit..&. choices)
    + Bit.shiftL 1 choicesCount
    + Bit.shiftL (0b1111 `Bit.xor` Bit.bit rotatation) choicesInvalid

{-# INLINE forcePiece #-}
forcePiece :: Pix -> Piece -> Piece
forcePiece dst p@Piece{pipe=src} = (initChoicesL %~ forceChoice dst src) p

{-# INLINE forceContinue #-}
forceContinue :: Pix -> Continue -> Continue
forceContinue dst c@Continue{char=src} = (choicesL %~ forceChoice dst src) c

forceHints :: Continues -> Progress -> [Unwind] -> SolverT Progress
forceHints continues p@Progress{maze} = foldlM deployHint p
  where
    deployHint p (UnSolve c _ pix) =
      if IntMap.member c continues
      then toSolverT (prioritizeContinue p c (forceContinue pix . fromJust))
      else p <$ mazeModify maze (forcePiece pix) c
    deployHint p _ = pure p

{--- Solver bits: components ---}

{-# INLINE compInsert #-}
compInsert :: Continue -> Components -> Components
compInsert Continue{origin} (Components c) = Components (IntMap.insertWith (+) origin 1 c)
compInsert Continue{origin, cursor} (Components' c) = Components' (IntMap.insertWith IntSet.union origin (IntSet.singleton cursor) c)

{-# INLINE compRemove #-}
compRemove :: PartId -> Fursor -> Components -> Components
compRemove origin _cursor (Components c) = Components (IntMap.update (Just . subtract 1) origin c)
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

compCounts :: Components -> IntMap Int
compCounts (Components c) = c
compCounts (Components' c) = IntMap.map IntSet.size c

{--- Solver bits: continues ---}

{-# INLINE deltaContinue #-}
deltaContinue :: Continue -> Int -> Fursor -> Direction -> Piece -> Maybe Continue -> Continue
deltaContinue Continue{char, origin, island, area} id c from Piece{pipe, initChoices} prev = do
  let pointed = char `Bit.testBit` from
  let origin' = if pointed then origin else c
  let island' = min 4 (island * 2) -- islands get bumped once to set area, next time to solve islands whole
  let dir = (from + 2) `mod` 4

  let initChoices' = maybe initChoices choices prev
  let validRot = pixNDirections (initChoices' `Bit.shiftR` choicesInvalid)
  let invalids = filter (\r -> pointed /= Bit.testBit (rotate r pipe) dir) validRot
  let choices' :: Int = foldr (\d s -> s - Bit.bit choicesCount + Bit.bit (choicesInvalid + d)) initChoices' invalids
  let solveds = Bit.bit (dir + choicesSolveds)
  -- let require = fromEnum pointed `Bit.shiftL` (dir + choicesRequire) -- not used

  Continue c pipe origin' 0 id island' area (choices' Bit..|. solveds)

{-# INLINE prioritizeDeltas #-}
-- | Calls 'prioritizeContinue' on nearby pieces (delta = 1)
prioritizeDeltas :: Width -> Progress -> Continue -> SolverT Progress
prioritizeDeltas width p@Progress{iter, maze} continue@Continue{cursor=cur, choices} = do
  (toSolverT . prioritizeContinues p) <=< for (zip [0..] (pixNDirections choices)) $ \(i, d) -> do
    piece <- mazeRead maze (mazeFDelta width cur d)
    let delta = mazeFDelta width cur d
    pure (delta, deltaContinue continue (iter * 4 + i) delta d piece)

{-# INLINE rescoreContinue #-}
-- | Recalculates the 'Continue's score, less is better (because of 'IntMap.deleteFindMin' in 'findContinue').
--
-- > score = (0 - island << 17 + (choices << (15 - choicesCount)) + x + y) << 32 + created
rescoreContinue :: Bounds -> Width -> Continue -> Continue
rescoreContinue bounds width c@Continue{cursor, choices=choicesBits, island, area, created} = set scoreL score c
  where
    -- score = choices + x + y -- interesting for animations for smaller levels
    score = (0 + bound << 34 - island << 27 + area << 15 + (choices << (12 - choicesCount)) + x + y) << 28 + created
    bound = if bounded bounds cursor then 0 else 1
    choices = choicesBits Bit..&. (0b11 << choicesCount)
    (<<) = Bit.shiftL
    (x, y) = mazeCursor width cursor

{-# INLINE prioritizeContinue' #-}
prioritizeContinue' :: Width -> PrioCompCont -> Fursor -> (Maybe Continue -> Continue) -> Solver PrioCompCont
prioritizeContinue' width (p, cp, ct) c get =
  ReaderT $ \Configuration{cBounds} -> Identity $ found cBounds (IntMap.lookup c ct)
  where
    found :: Bounds -> Maybe Continue -> PrioCompCont
    found bounds Nothing =
      let new = rescoreContinue bounds width (get Nothing)
      in (IntMap.insert (score new) c p, compInsert new cp, IntMap.insert c new ct)
    found bounds (Just old@Continue{cursor, created, choices=choicesO}) =
      if score new < score old || choicesN /= choicesO
      then (IntMap.insert (score new) cursor . IntMap.delete (score old) $ p, cp, IntMap.insert c new ct)
      else (p, cp, ct)
      where new@Continue{choices=choicesN} = rescoreContinue bounds width (get (Just old)) { created }

{-# INLINE prioritizeContinues #-}
-- | Inserts or reprioritizes 'Continue'
prioritizeContinues :: Progress -> [(Fursor, Maybe Continue -> Continue)] -> Solver Progress
prioritizeContinues progress@Progress{maze=MMaze{width}, priority, continues, components} reprios =
  putback <$> foldlM prio (priority, components, continues) reprios
  where
    putback (p, cp, cn) = progress { priority = p, components = cp, continues = cn }
    prio acc (c, get) = prioritizeContinue' width acc c get

{-# INLINE prioritizeContinue #-}
prioritizeContinue :: Progress -> Fursor -> (Maybe Continue -> Continue) -> Solver Progress
prioritizeContinue p = curry (prioritizeContinues p . pure)

{-# INLINE pieceDead #-}
-- | Check if 'Continue' is about to become separated from the rest of the graph.
pieceDead :: MonadIO m => MMaze -> Components -> Fursor -> Pix -> Choices -> m Bool
pieceDead _ _ _ 0b00000000 _ = pure False
pieceDead maze components cur pix choices = do
  thisPart <- partEquate maze . partId =<< mazeRead maze cur
  pure (compAlive thisPart components && stuck)
  where stuck = 0 == ((0b1111 Bit..&. pix) Bit..&. Bit.complement (fromIntegral choices))

-- | Pops `priority` by `score`, deletes from `continues`.
{-# INLINE findContinue #-}
findContinue :: Progress -> Solver (Maybe Continue)
findContinue Progress{priority, continues} = do
  ReaderT $ \Configuration{cMode, cBounds} -> Identity $ do
    cursor <- mfilter (bounded cBounds) (snd <$> IntMap.lookupMin priority)
    mfilter
      (\Continue{choices} -> solveDeterministic cMode || (2 > Bit.shiftR choices choicesCount))
      (cursor `IntMap.lookup` continues)

{-# INLINE popContinue #-}
-- | Pops next 'Continue' from queue.
popContinue :: Progress -> Progress
popContinue p@Progress{priority=pr, continues=c} = p { priority, continues = IntMap.delete cursor c }
  where ((_, cursor), priority) = IntMap.deleteFindMin pr

{--- Backtracking solver ---}

-- | Solves a valid piece, mutates the maze and sets unwind.
-- Inefficient access: partEquate reads the same data as islands reads.
-- (All functions within this function are inlined)
solveContinue :: Progress -> Continue -> SolverT Progress
solveContinue
  progress@Progress{maze=maze@MMaze{width}, components = components_}
  continue@Continue{cursor, char, origin = origin_} = do
    thisPart <- partEquate maze origin_
    unwindThis <- mazeSolve maze continue
    let directDeltas = map (mazeFDelta width cursor) $ pixDirections char
    neighbours <- fmap (nubOrd . (thisPart :)) . traverse (partEquate maze) $ directDeltas
    let origin = minimum neighbours
    let components = compEquate origin (filter (/= origin) neighbours) (compRemove thisPart cursor components_)
    unwindEquate <- mazeEquate maze origin neighbours

    traceBoard continue . (iterL %~ (+1)) . (depthL %~ (+1))
      . (spaceL . _head %~ (, unwindThis : unwindEquate) . fst)
      =<< prioritizeDeltas width progress { components } continue { origin }

-- | The initial 'Progress', 'space' stack, 'Progress' and 'MMaze' backtracking operations.
-- This returns a progress with the first available 'Continue' from 'space' or Nothing.
-- If 'space' is empty, it gets popped, 'mazePop' gets called and it tries again until 'space' is empty.
backtrack :: MonadIO m => Progress -> m (Maybe (Progress, Continue))
backtrack Progress{space=[]} = pure Nothing
backtrack p@Progress{space=(([], []):space)} =
  backtrack p { space }
backtrack Progress{space=(((continue, p):guesses, []):space), maze, iter} = do
  pure (Just (p { maze, iter, space = (guesses, []) : space }, continue))
backtrack p@Progress{space=((guesses, unwind):space), maze} = do
  traverse_ (mazePop maze) unwind
  backtrack p { space = (guesses, []) : space }

-- | Solves pieces by backtracking, stops when the maze is solved or constraints met.
solve' :: Progress -> SolverT (Maybe Progress)
solve' p@Progress{depth, maze=MMaze{size}} | depth == size = pure (Just p) -- remove this to compute all solutions
solve' progress@Progress{depth, maze=maze@MMaze{size}, components} = do
  Configuration{cLifespan, cMode} <- ask
  guesses <- liftIO . foldMap (guesses progress) . maybeToList =<< toSolverT (findContinue progress)
  guess <- backtrack . (spaceL %~ if null guesses then id else ((guesses, []) :)) =<< pure progress
  guess <- pure $ guess & _Just . _1 . spaceL %~ (if solveWithHistory cMode then id else init)
  progress <- traverse (uncurry (solveContinue . popContinue)) guess

  unbounded <- null . join <$> toSolverT (traverse findContinue progress)
  let stop = depth == size - 1 || cLifespan == 0 || unbounded
  withReaderT (cLifespanL %~ subtract 1) $ next stop progress
  where
    next True = pure
    next False = fmap join . traverse solve'

    guesses :: MonadIO m => Progress -> Continue -> m [(Continue, Progress)]
    guesses progress continue@Continue{cursor, char, choices} = do
      let rotations = pixNDirections (Bit.shiftR choices choicesInvalid)
      rotations <- filterDisconnected (map (\r -> (cursor, rotate r char, choices)) rotations)
      pure (map (\(_, pipe, _) -> (set charL pipe continue, progress)) rotations)

    filterDisconnected :: MonadIO m => [(Fursor, Pix, Choices)] -> m [(Fursor, Pix, Choices)]
    filterDisconnected = filterM $ \(cur, pix, choices) -> do
      disconnected <- pieceDead maze components cur pix choices
      pure ((depth == size - 1) || not disconnected)

{--- Island computations ---}

-- | The generic /paint/ of the 'flood' fill.
type FillNext m s = MMaze -> Cursor -> Piece -> [(Piece, Direction)] -> StateT s m [Cursor]

-- | Four-way flood fill with 'FillNext' as the "paint". The initial piece is assumed to be valid FillNext.
flood :: MonadIO m => Monoid s => FillNext m s -> MMaze -> Cursor -> m (Set Cursor, s)
flood n m = flip runStateT mempty . flood' n m Set.empty . return
  where
  flood' :: MonadIO m => FillNext m s -> MMaze -> Set Cursor -> [Cursor] -> StateT s m (Set Cursor)
  flood' _ _ visited [] = pure visited
  flood' fillNext maze@MMaze{width=w} visited (cursor@(x, y):next) = do
    this <- liftIO (mazeRead maze (x + y * w))
    more <- fillNext maze cursor this =<< liftIO (mazeDeltasWalls maze cursor)
    let next' = filter (not . flip Set.member visited) more ++ next
    flood' fillNext maze (Set.insert cursor visited) next'

-- | Set low priority to all continues with island = 1.
islandize :: Progress -> SolverT Progress
islandize p@Progress{continues} = toSolverT $ do
  prioritizeContinues p (map (, mapContinue) (IntSet.toList (IntMap.keysSet continues)))
  where mapContinue = set areaL 999 . set islandL 1 . fromJust

islandConnectivityRefinement :: [IslandSolution] -> [IslandSolution]
islandConnectivityRefinement = POSet.lookupMax . POSet.fromList . map head . groupSortOn icComponents

{- HLINT ignore islandChoices "Redundant bang pattern" -}
-- | Computes and set 'iChoices'/'iSolutions' for the island, but also modifies maze with 'icHints' if len choices == 1.
islandChoices :: MMaze -> Progress -> Island -> SolverT Island
islandChoices _ Progress{components=Components _} _ = error "not enough info, unlikely"
islandChoices maze' p@Progress{maze, components=Components' compInit} i@Island{iBounds} = do
  !(capped, solutions) <- iterateMaybeM 1000 (solution . fst) . (, []) =<< toSolverT (islandProgress p i maze')
  !solutions <- pure (islandConnectivityRefinement . join . map snd $ solutions)
  when capped $ liftIO (MV.unsafeCopy (board maze') (board maze)) -- this copies much more than it needs to, but rarely

  pure (i & set iChoicesL (length solutions) & set iSolutionsL solutions)
  where
    constrain c = c { cLifespan = - 1, cBounds = Just (`IntSet.member` iBounds), cBench = True }

    solution :: Progress -> SolverT (Maybe (Progress, [IslandSolution]))
    solution p = withReaderT constrain (solve' p) >>= traverse (\p -> (p, ) . pure <$> islandSolution p)

    islandSolution :: MonadIO m => Progress -> m IslandSolution
    islandSolution Progress{components=Components _} = error "not enough info, unlikely"
    islandSolution Progress{maze, components=comp@(Components' compJoin), space} = do
      compEquated <- traverse (\p -> (, p) <$> partEquate maze p) $ compDiff compInit compJoin
      pure (IslandSolution (compParts compEquated) (compCounts comp) (snd =<< space))
      where
        compDiff a b = IntSet.toList (on IntSet.difference IntMap.keysSet a b)
        compParts = map (Set.fromList . uncurry (:)) . groupSort

    islandProgress _ Island{iConts=[]} _ = error "impossible because iConts is result of `group'"
    islandProgress p Island{iConts=(Continue{cursor}:_)} maze =
      prioritizeContinue (p { maze, space = [] }) cursor (set islandL 2 . fromJust)

islands :: MonadIO m => Progress -> m [Island]
islands Progress{maze=maze@MMaze{width}, continues} = do
  snd <$> foldIsland perIsland (map (mazeCursor width . cursor . snd) . IntMap.toList $ continues)
  -- pure (islands, foldMap (\Island{iId, iConts = cs} -> IntMap.fromList $ (, iId) <$> map cursor cs) islands)
  where
    foldIsland perIsland continues =
      (\acc -> foldlM acc (Set.empty, []) continues) $ \acc@(visited, _) cursor ->
        if cursor `Set.member` visited then pure acc else perIsland cursor acc

    -- border = island's border by continues
    perIsland :: MonadIO m => Cursor -> (Set Cursor, [Island]) -> m (Set Cursor, [Island])
    perIsland cursor (visited, islands) = do
      (area, borders) <- flood (fillNextSolved continues) maze cursor
      let iConts = (continues IntMap.!) . mazeFursor width <$> Set.toList borders
      let iBounds = IntSet.fromList . map (mazeFursor width) . Set.toList $ area
      let island = Island (maybe 0 (\(x, y) -> x + y * width) (Set.lookupMin borders)) (Set.size area) iConts iBounds [] 0
      pure (visited `Set.union` borders, island : islands)

    fillNextSolved :: MonadIO m => Continues -> FillNext m (Set Cursor)
    fillNextSolved continues _ cur@(x, y) _ deltasWall = do
      when ((x + y * width) `IntMap.member` continues) $ State.modify (Set.insert cur)
      pure . map (mazeDelta cur . snd) . filter (\(Piece{pipe, solved}, _) -> pipe /= 0 && not solved) $ deltasWall

-- | Set up 'Choices' for all 'Continue's and `Piece`s for all unique 'IslandSolution's.
islandHinting :: [Island] -> Progress -> SolverT Progress
islandHinting islands p@Progress{continues} = do
  reduceM p islands $ \p _i@Island{iSolutions} -> do
    foldlM (forceHints continues) p (icHints `map` unique iSolutions)
  where
    reduceM a l f = foldlM f a l

    unique []      = []
    unique [a]  = [a]
    unique (_:_:_) = []

{--- Meta solver ---}

islandChoicesParallel :: Progress -> [MMaze] -> [Island] -> SolverT [Island]
islandChoicesParallel p [copy] islands = for islands (islandChoices copy p)
islandChoicesParallel p copies islands = do
  conf@Configuration{cNumCap} <- ask
  let islandChunks = zip copies . transpose . chunksOf cNumCap $ islands
  fmap join . liftIO . parallelInterleaved . flip map islandChunks $ \(copy, islands) ->
    for islands (flip runReaderT conf . islandChoices copy p)

islandsWithChoices :: Progress -> SolverT ([Island], [MMaze])
islandsWithChoices p@Progress{maze} = do
  copies <- flip replicateM (mazeClone maze) =<< asks cNumCap
  (, copies) <$> (islandChoicesParallel p copies =<< islands p)

-- | Sovles deterministic parts of the maze in parallel.
solveDetParallel :: Int -> MMaze -> SolverT Progress
solveDetParallel n m@MMaze{width} = do
  (_, zeroth):rest <- divideProgress m
  conf <- ask
  (Sum iter, continues) <- liftIO . fmap fold . parallelInterleaved . map (fmap progressExtract . solvePar conf) $ rest
  toSolverT (prioritizeContinues (zeroth { iter, depth = iter }) (map ((\c -> (cursor c, return c)) . snd) continues))
  where
    solvePar conf (n, p) = fromJust <$> runReaderT (solve' p) (configuration conf n)
    progressExtract Progress{iter, continues} = (Sum iter, IntMap.toList continues)
    configuration c n = c
      { cBounds = Just (\f -> mazeQuadrant m (mazeCursor width f) == n)
      , cMode = SolveParallel }

    divideProgress :: MMaze -> SolverT [(Int, Progress)]
    divideProgress m@MMaze{width, trivials} =
      let
        p = Progress 0 0 IntMap.empty IntMap.empty (Components IntMap.empty) [] m
        continue (i, c) = (\Piece{pipe, initChoices} -> (c, return (Continue c pipe c 0 (-i) 0 0 initChoices))) <$> mazeRead m c
        quad = mazeQuadrant m
      in do
        continues <- traverse continue (zip [0..] trivials)
        toSolverT . traverse (_2 (prioritizeContinues p)) . groupSort . map (\c -> (quad . mazeCursor width . fst $ c, c)) $ continues

    mazeQuadrant :: MMaze -> Cursor -> Int
    mazeQuadrant MMaze{width} = uncurry (quadrant width) (coeff n)
      where
        splits = [(1, (1, 1)), (2, (2, 1)), (4, (2, 2)), (6, (3, 3)), (8, (4, 2)), (12, (4, 3)), (16, (4, 4))]
        coeff n = fromMaybe (error "define split for capabilities") (lookup n splits)

        -- | Returns a unique quadrant id for a 0-based n-size grid split into s*s quadrants
        -- which separated by lines of zeros. May be useful as the key function for groupSortOn.
        -- https://stackoverflow.com/questions/2745074/fast-ceiling-of-an-integer-division-in-c-c
        quadrant :: Int -> Int -> Int -> Cursor -> Int
        quadrant n' sx sy (x', y') = (l *) . (1 +) $ x `div` qx + wrap * (y `div` qy)
          where
            (x, y, qx, qy, n) = (x' + 2 , y' + 2, n `div` sx + 1, n `div` sy + 1, n' + 2)
            l = if x `mod` qx < 2 || y `mod` qy < 2 then 0 else 1
            wrap = (n + qy) `div` qy -- wrap x = ceiling (n / q)

-- | Solve all island's pieces which remain the same in all solutions.
islandStaticHints :: [Island] -> Progress -> SolverT Progress
islandStaticHints islands p@Progress{continues} =
  foldlM (\p _i@Island{iSolutions} -> forceHints continues p (uniqueHints iSolutions)) p islands
  where
    uniqueHints [solution] = map unwindEraseBefore (icHints solution)
    uniqueHints solutions = Set.toList (solutionIntersection solutions)

    solutionIntersection = List.foldl1 Set.intersection . map (Set.fromList . map unwindEraseBefore . icHints)

    unwindEraseBefore (UnSolve fursor _ pix) = UnSolve fursor 0 pix
    unwindEraseBefore a = a

initProgress :: MMaze -> SolverT Progress
initProgress m@MMaze{trivials} =
  let
    p = Progress 0 0 IntMap.empty IntMap.empty (Components IntMap.empty) [] m
    continue (i, c) = (\Piece{pipe, initChoices} -> (c, return (Continue c pipe c 0 (-i) 0 0 initChoices))) <$> mazeRead m c
  in toSolverT . prioritizeContinues p =<< traverse continue (zip [0..] trivials)

-- | First deterministic, possibly parallel run-through solve stage.
solveBasic :: MMaze -> SolverT Progress
solveBasic maze = do
  whenM (asks (not . cBench)) $ void . renderImage' "start" =<< initProgress maze
  p <- initSolve maze =<< asks cNumCap
  p <- componentRecalc True . fromJust =<< determinstically (solve' p)
  renderImage' "islandize" p
  where
    initSolve m@MMaze{level} n | n > 1 && 4 <= level =
      componentRecalc False =<< renderImage' "parallel" =<< solveDetParallel n m
    initSolve m _ = initProgress m

    -- | Progress.components = Components -> Components'
    componentRecalc :: MonadIO m => Bool -> Progress -> m Progress
    componentRecalc deep p@Progress{maze, continues} = do
      comps <- foldr (IntMap.unionWith IntSet.union) IntMap.empty <$> traverse component (IntMap.toList continues)
      pure . (\c -> p { components = c }) $ if deep then Components' comps else Components (IntMap.map IntSet.size comps)
      where component (_, Continue{origin, cursor}) = IntMap.singleton <$> partEquate maze origin <*> pure (IntSet.singleton cursor)

solveIslandStatic :: Progress -> SolverT Progress
solveIslandStatic p = do
  (islands, _) <- islandsWithChoices p
  p <- fmap (fromMaybe p) . determinstically . solve' =<< islandStaticHints islands p
  renderImage' "islandize-static" p

solveTrivialIslands :: Progress -> SolverT Progress
solveTrivialIslands p@Progress{maze=MMaze{level}} | 6 >= level = pure p
solveTrivialIslands p = do
  (islands, copies) <- islandsWithChoices p
  solveTrivialIslands' copies islands =<< islandHinting islands =<< islandize p
  where
    solveTrivialIslands' :: [MMaze] -> [Island] -> Progress -> SolverT Progress
    solveTrivialIslands' _ _ p@Progress{depth, maze=MMaze{size}} | depth == size = pure p
    solveTrivialIslands' copies islands p@Progress{maze} = solveT =<< determinstically (toSolverT (findContinue p))
      where
        solveT Nothing = pure p
        solveT (Just _) = do
          (_space, solve) <- spaceL ((,) =<< id) . fromJust <$> determinsticallyI (solve' p)
          islands <- filterM (fmap (not . solved) . mazeRead maze . cursor . head . iConts) islands
          islands <- islandChoicesParallel p copies islands
          solveTrivialIslands' copies [] =<< islandHinting islands solve

-- | Solve with backtracking, return the same maze, if search space exhausted.
solveBacktrack :: Progress -> SolverT Progress
solveBacktrack p = fmap (fromMaybe p) (solve' p)

-- | Solver main, returns solved maze
solve :: MMaze -> SolverT MMaze
solve maze@MMaze{time} = do
  p <- solveBacktrack =<< solveTrivialIslands =<< solveIslandStatic =<< solveBasic maze
  time' <- diffTimeSpec <$> liftIO (getTime Monotonic) <*> pure time
  let Progress{iter, depth, maze} = p
  let ratio = fromIntegral iter / fromIntegral depth :: Double
  let runtime = fromIntegral (toNanoSecs time') / 1_000_000_000 :: Double
  liftIO (putStrLn (printf "\x1b[2K%i/%i, ratio: %0.5f, time: %0.2fs" iter depth ratio runtime))
  maze <$ renderImage' "done" p

solveIO :: MMaze -> IO MMaze
solveIO m = configuration m >>= runReaderT (solve m)

{--- Main ---}

verify :: MMaze -> SolverT Bool
verify maze@MMaze{board, size} = do
  (Sum spaces) <- V.foldl' (flip (mappend . Sum . (fromEnum . (== 0) . pipe))) mempty <$> V.freeze board
  required <- asks (not . cBench)
  if required
  then (size - spaces ==) . Set.size . fst <$> flood fillNextValid maze (0, 0)
  else pure True
  where
    fillNextValid :: FillNext SolverT ()
    fillNextValid maze cur Piece{pipe=this} deltasWalls = pure $
      if validateRotation this deltasWalls 0
      then filter (mazeBounded maze) . map (mazeDelta cur) $ pixDirections this
      else []

storeBad :: Int -> MMaze -> MMaze -> SolverT MMaze
storeBad level original solved = (solved <$) $ do
  whenM (not <$> verify solved) $ do
    liftIO (putStrLn (printf "storing bad level %i solve" level))
    mazeStore original ("samples/bad-" ++ show level)

rotateStr :: Int -> MMaze -> MMaze -> IO [Text]
rotateStr split input solved =
  map concatenate . chunksOf split <$> rotations input solved
  where
    concatenate :: [(Cursor, Rotation)] -> Text
    concatenate =
      (T.pack "rotate " <>) . T.intercalate (T.pack "\n")
      . (>>= (\((x, y), r) -> replicate r (T.pack (printf "%i %i" x y))))

    rotations :: MMaze -> MMaze -> IO [(Cursor, Rotation)]
    rotations MMaze{width, board=input} MMaze{board=solved} = do
      (as, bs) <- on (,) V.toList <$> V.freeze input <*> V.freeze solved
      pure (map (\(idx, pa, pb) -> (mazeCursor width idx, on rotations pipe pa pb)) (zip3 [0..] as bs))
      where
        rotations from to = fromJust $ to `List.elemIndex` iterate (rotate 1) from

-- | Create 'Configuration' from environment variables, create image output directory.
configuration :: MMaze -> IO Configuration
configuration MMaze{mazeId, level} = do
  let mazeDir :: String = printf "lvl%i-%s" level mazeId
  imageDir :: String <- formatTime defaultTimeLocale ("images/%F-%H-%M-%S-" ++ mazeDir ++ "/") <$> getCurrentTime
  conf <- set cImageDirL imageDir <$> cNumCapL (const getNumCapabilities) confDefault
  conf <- (s cBenchL "bench" <=< s cDebugL "debug" <=< s cDebugFreqL "freq" <=< s cPixSizeL "pix") conf
  (conf <$) . unless (cBench conf) $ createDirectoryIfMissing True imageDir
  where
    s :: Read a => Setter' s a -> String -> s -> IO s
    s setter env s = (\v' -> (setter %~ (`fromMaybe` (read <$> v'))) s) <$> lookupEnv env

-- | Gets passwords for solved levels from the maze server.
pļāpātArWebsocketu :: [Int] -> Bool -> WS.ClientApp ()
pļāpātArWebsocketu levels hide conn = for_ levels solveLevel
  where
    send = WS.sendTextData conn
    recv = T.unpack <$> WS.receiveData conn

    solveLevel level = do
      send (T.pack $ "new " ++ show level)
      recv

      send (T.pack "map")
      maze <- parse . T.unpack . T.drop 5 =<< WS.receiveData conn

      solve <- runReaderT (storeBad level maze =<< solve =<< mazeClone maze) =<< configuration maze
      putStr "rotating..." >> hFlush stdout
      traverse (\r -> do send r; recv) =<< rotateStr 10_000 maze solve

      send (T.pack "verify")
      putStrLn . ("\r" ++) . (if hide then reverse . dropWhile (/= ' ') . reverse else id) =<< recv

-- | Run solver, likely produce trace output and complain if solve is invalid ('verify').
solveFile :: String -> IO ()
solveFile file = do
  maze <- parse =<< readFile file
  conf <- configuration maze

  flip runReaderT conf $ do
    solved <- solve maze
    whenM (not <$> verify solved) (liftIO (putStrLn "solution invalid"))

-- | Executable entry point.
main :: IO ()
main = run . fmap parseUrl =<< lookupEnv "websocket"
  where
    run (Just (host, path, levels)) = do
      hide <- or . fmap read <$> lookupEnv "hide"
      withSocketsDo $ WS.runClient host 80 path (pļāpātArWebsocketu levels hide)
    run Nothing =
      traverse_ solveFile . (\args -> if null args then ["/dev/stdin"] else args) =<< getArgs

    parseUrl :: String -> (String, String, [Int])
    parseUrl s =
      case splitOn "/" s of
        (host:rest) ->
          case splitOn "#" ("/" ++ join (intersperse "/" rest)) of
            [path, levels] -> (host, path, map read (splitOn "," levels))
            _ -> error "usage: websocket=maze.host/1,2,3,4,5,6"
        _ -> error "usage: websocket=maze.host/1,2,3,4,5,6"
