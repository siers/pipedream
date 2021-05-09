-- 2020-01-30-23:37:16
m <- parse =<< readFile "samples/sanity"
p = Progress 0 [Continue (0, 0) 0 True (0, 0) 0 0] m
fmap pipe <$> UV.toList <$> (UV.freeze $ board $ maze p)
putStrLn =<< renderWithPositions p

:l solve
m <- parse "   \n   \n   \n"
partEquate m (1,1)
mazeEquate m (1,1) [(2,2)]
partEquate m (1,1)
partEquate m (2,2)
mazeEquate m (0,0) [(1,1)]
partEquate m (2,2)

-- 2021-02-22-20:19:16

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
:l solve
renderImage' "ghci" =<< initProgress =<< parse =<< readFile "samples/5-1"
q <- reconnectComponents =<< islandize =<< solve' (-1) True =<< initProgress =<< parse =<< readFile "samples/5-1"
writeFile "out" . LBS.unpack . Aeson.encode . toJSON . (\(Components' c) -> Map.mapKeys show c) . components $ q

renderImage' "ghci" q
nub . map (area . snd) . Map.toList . continues . fst $ q

preview $ (mkGraph [(1, "")] [] :: Gr String String)

import qualified Data.List as L
L.sort . map snd . Map.toList <$> islandize' q
-- unique island sizes (slightly smaller number than the number of islands, but close)
-- [6,8,10,11,12,14,16,19,20,21,23,24,25,27,37,45,65,66,76,96,117,226,232]
-- [6,6,6,6,6,6,6,8,8,8,8,8,10,10,10,10,11,12,14,16,19,20,21,23,24,24,25,27,37,37,45,65,66,76,96,117,226,232]
map (uncurry (+) . cursor . snd) . Map.toList $ priority q
-- number of unique cursors
-- [68,69,70,71,72,73,74,75,76,77,...

islandize' :: Progress -> IO (Map Cursor Int)
islandize' p@Progress{maze, priority} =
  fmap snd . foldrM acc (Set.empty, Map.empty) $ map (cursor . snd) . Map.toList $ priority
  where
    acc cursor t@(visited, m) =
      if (cursor `Set.member` visited)
      then pure t
      else do
        (more, _inhabitants) <- fillSize (fillNextSolved Map.empty) maze cursor
        pure (visited `Set.union` more, Map.insert cursor (Set.size more) m)

    fillNextSolved :: Continues -> FillNext (Set Cursor)
    fillNextSolved continues _ cur _ deltasWall = do
      when (cur `Map.member` continues) $ State.modify (Set.insert cur)
      pure . map (cursorDelta cur . snd) . filter (\(Piece{pipe, solved}, _) -> pipe /= 0 && not solved) $ deltasWall

uniq = foldr uniq' [] where uniq' x acc = x : dropWhile (== x) acc
(iter q, depth q, length (space q))
uniq (map length (space q))
length $ filter (== 1) (map length (space q))
filter ((>= 1) . snd) (zip [0..] $ map length (space q))

-- 2021-02-27-11:34:52

import Graphics.Image.ColorSpace (toWord8Px)
import Graphics.Image.Interface (thaw, MImage, freeze, write)
import Graphics.Image (writeImage, makeImageR, Pixel(..), toPixelRGB, VS(..), RGB, Writable(..), VU(..), Image)
import Data.Word
let gc = makeImageR VS (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)))
let grad_gray = makeImageR VU (200, 200) (\(i, j) -> PixelY (fromIntegral i) / 200 * (fromIntegral j) / 200)
let grad_color = makeImageR VU (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j))) / 400
writeImage "images/grad_color.png" grad_color
writeImage "images/x.png" $ makeImageR VU (200, 200) (\(i, j) -> toPixelRGB $ PixelHSI 0 1 (fromIntegral (i+j)/400))

-- 2021-03-14-18:20:50

import Control.Lens.Internal.FieldTH (makeFieldOptics, LensRules(..))
import Language.Haskell.TH.Syntax (mkName, nameBase)
import Control.Lens.TH (DefName(..), lensRules)
suffixLNamer = (\_ _ -> (:[]) . TopName . mkName . (++ "L") . nameBase)
:t makeFieldOptics lensRules { _fieldToDef = suffixLNamer }

-- 2021-05-01-16:53:57

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
:l Pipemaze
p <- initProgress =<< parse =<< readFile "samples/3"
(Map.! (0, 1)) $ continues p
showBin . flip Bit.shiftR choicesInvalid . initChoices . (V.! 8) <$> V.freeze (board (maze p))

flip Bit.shiftR choicesCount  . choices . snd <$> Map.toList (continues p)
showBin = flip (showIntAtBase 2 intToDigit) ""
-- V.map (showBin . flip Bit.shiftR choicesInvalid . initChoices) <$> V.freeze (board (maze p))
showBin . flip Bit.shiftR choicesInvalid . initChoices . V.head <$> V.freeze (board (maze p))

-- 2021-05-06-21:33:38

p <- initProgress =<< parse =<< readFile "samples/3"
p <- fmap fromJust (solve' (-1) True p)
render . maze . fromJust =<< solve' (-1) False =<< (set mazeL <$> mazeClone (maze p) <*> pure p)

import Control.Concurrent (threadDelay)
:l Pipemaze
-- progressClone :: Progress -> IO Progress
progressClone = mazeL (boardL MV.clone) :: Progress -> IO Progress
progressRender = (\p -> putStrLn "\x1b[H\x1b[2K" >> render (maze p))
progressSolved Progress{maze=MMaze{width, height}, depth} = width * height == depth
showSpace = filter (not . null . fst) . flip zip [0..] . fmap (fmap fst) . space
p_ <- initProgress =<< parse =<< readFile "samples/3"
p_ <- fmap fromJust (solve' (-1) True p_)
ps <- iterateMaybeM 100 (\p -> fmap (mfilter progressSolved) . solve' (2500 * 10) False =<< progressClone p) p_
traverse_ ((>> threadDelay 1000000) . progressRender ) ps

-- traverse print . fmap (take 2 . showSpace) $ ps
-- length . nubOrd . fmap (filter (not . null . fst) . flip zip [0..] . fmap (fmap fst) . space) $ ps

s n p = foldrM id p . replicate n $ (\p -> fromJust <$> solve' 300000 False p)
ss n p = s n =<< progressClone p
progressRender =<< ss 3 p_

s n p = foldrM id p . replicate n $ (\p -> fmap fromJust . solve' 300000 False =<< progressClone p)
ss n p = s n =<< progressClone p
progressRender =<< ss 3 p_

-- 2021-05-09-11:05:00

-- bug: backtracking doesn't reuse previous' solve's maze
progressRender =<< fmap fromJust . solve' 5000 False =<< progressClone =<< pure p_
progressRender =<< fmap fromJust . solve' 5000 False =<< progressClone =<< fmap fromJust . solve' 5000 False =<< progressClone =<< pure p_
progressRender =<< fmap fromJust . solve' 5000 False =<< (mazeL (boardL (\b -> MV.clone b)) =<< fmap fromJust . solve' 5000 False =<< progressClone =<< pure p_)
progressRender =<< fmap fromJust . solve' 5000 False =<< mazeL (boardL (\b -> MV.clone b >> pure b)) =<< fmap fromJust . solve' 5000 False =<< progressClone =<< pure p_
p <- fmap fromJust . solve' 25000 False =<< progressClone =<< pure p_
rs = set spaceL []
(rs p ==) . rs <$> progressClone p

fmap (take 2 . showSpace . fromJust) . solve' (-1) False =<< fmap fromJust . solve' (-1) False =<< fmap Just (progressClone p_)
fmap depth . fmap fromJust . solve' (-1) False =<< progressClone p_
fmap depth . fmap fromJust . solve' (-1) False =<< fmap fromJust . solve' (-1) False =<< progressClone p_

λ: f = pure . fmap (+1) . mfilter (\a -> mod a 11 /= 3) . Just
λ: iterateMaybeM 1000 f 4
[5,6,7,8,9,10,11,12,13,14]
λ: iterateMaybeM 1000 f 7
[8,9,10,11,12,13,14]
