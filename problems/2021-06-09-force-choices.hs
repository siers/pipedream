{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}

module Main where

import Data.Char (intToDigit)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Numeric (showIntAtBase)
import qualified Data.Bits as Bit
import qualified Data.List as List

type Pix = Word8
type Rotation = Int

type Choices = Int
(choicesSolveds, _choicesRequire, choicesInvalid, choicesCount) = (0, 4, 8, 12)

directions = [0, 1, 2, 3]
-- | > directions = rotations = [0, 1, 2, 3]
rotations = directions

rotate :: Rotation -> Pix -> Pix
rotate = flip Bit.rotateL

forceChoice :: Pix -> Pix -> Choices -> Choices
forceChoice forced pix choices =
  let
    rotatation = fromJust (List.find (\r -> rotate r pix == forced) rotations)
    exceptSolveds = Bit.shiftL 0b1111 choicesSolveds
  in
    (exceptSolveds Bit..&. choices)
    + (Bit.shiftL 1 choicesCount)
    + (Bit.shiftL (0b1111 `Bit.xor` Bit.bit rotatation) choicesInvalid)

showBin = flip (showIntAtBase 2 intToDigit) ""
showBin' = showBin . (Bit.bit (4 * 6) +)

problems :: [(Word8, Word8, Int, Int)]
problems =
  [ (0b01100110, 0b11001100, 0b1111_0100_0000_1001, 0b0001_0111_0000_1001)
  ]

main = flip traverse problems $ \(force, pix, from, to) -> do
  print ("should", showBin' $ to)
  print ("is    ", showBin' $ forceChoice force pix from)
  print (if forceChoice force pix from == to then "OK" else "FAIL")

{-

:set -XNamedFieldPuns
import Data.Char (intToDigit)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Numeric (showIntAtBase)
import qualified Data.Bits as Bit
import qualified Data.List as List
showBin = flip (showIntAtBase 2 intToDigit) ""
showBin' = showBin . (Bit.bit (4 * 6) +)
p@Piece{initChoices} <- flip mazeRead 0 =<< parse =<< (readFile "samples/1")

1000000000001_0111_0000_1001


λ: showBin' initChoices
"1000000000001_0111_0000_1001"

λ: showBin' (forceChoice (toPix '┏') (toPix '┓') initChoices)
"1000000000001_0111_0000_1001"

showBin' (forceChoice (toPix '┓') (toPix '┓') initChoices)
"1000000000001_1110_0000_1001"

-}
