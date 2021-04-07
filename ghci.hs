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
