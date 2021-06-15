-- https://github.com/sgraf812/pomaps/issues/3

import Algebra.PartialOrd (PartialOrd(..))
import Control.Monad (foldM)
import Data.POSet (POSet)
import qualified Data.POSet as S

data XY = XY (Int, Int) Int deriving Show

instance Eq XY where XY ca sa == XY cb sb = ca == cb && sa == sb
instance PartialOrd XY where XY ca sa `leq` XY cb sb = if sa == sb then ca == cb else sa < sb

a = XY (0,5) 15
b = XY (2,2) 14

s = S.singleton (XY (0, 0) 0)

l =
  [ ("S.delete", S.delete, (XY (0,0) 0))
  , ("S.insert", S.insert, (XY (1,1) 32))
  , ("S.insert", S.insert, (XY (2,1) 33))
  , ("S.insert", S.insert, (XY (1,2) 13))
  , ("S.insert", S.insert, (XY (1,3) 14))
  , ("S.insert", S.insert, (XY (0,4) 14))
  , ("S.delete", S.delete, (XY (1,2) 13))
  , ("S.insert", S.insert, (XY (2,2) 14))
  , ("S.insert", S.insert, (XY (2,3) 35))
  , ("S.insert", S.insert, (XY (1,4) 15))
  , ("S.insert", S.insert, (XY (0,5) 15))
  , ("S.delete", S.delete, (XY (2,2) 14))
  , ("S.insert", S.insert, (XY (2,1) 13))
  ]

combine s (label, f, c) = print s *> putStrLn "" *> print (label, c) *> pure (f c s)
main = do
  print (a == b, a `leq` b, b `leq` a)
  -- (False,False,True)
  foldM combine s l

{-
==> (False,False,True)
fromList [XY (0,0) 0]

("S.delete",XY (0,0) 0)
fromList []

("S.insert",XY (1,1) 32)
fromList [XY (1,1) 32]

("S.insert",XY (2,1) 33)
fromList [XY (1,1) 32,XY (2,1) 33]

("S.insert",XY (1,2) 13)
fromList [XY (1,2) 13,XY (1,1) 32,XY (2,1) 33]

("S.insert",XY (1,3) 14)
fromList [XY (1,2) 13,XY (1,3) 14,XY (1,1) 32,XY (2,1) 33]

("S.insert",XY (0,4) 14)
fromList [XY (0,4) 14,XY (1,2) 13,XY (1,3) 14,XY (1,1) 32,XY (2,1) 33]

("S.delete",XY (1,2) 13)
fromList [XY (0,4) 14,XY (1,1) 32,XY (2,1) 33]

("S.insert",XY (2,2) 14)
fromList [XY (0,4) 14,XY (2,2) 14,XY (1,1) 32,XY (2,1) 33]

("S.insert",XY (2,3) 35)
fromList [XY (0,4) 14,XY (2,3) 35,XY (2,2) 14,XY (1,1) 32,XY (2,1) 33]

("S.insert",XY (1,4) 15)
fromList [XY (0,4) 14,XY (1,4) 15,XY (2,3) 35,XY (2,2) 14,XY (1,1) 32,XY (2,1) 33]

("S.insert",XY (0,5) 15)
fromList [XY (0,4) 14,XY (1,4) 15,XY (2,3) 35,XY (2,2) 14,XY (0,5) 15,XY (1,1) 32,XY (2,1) 33]

("S.delete",XY (2,2) 14)
fromList [XY (0,4) 14,XY (1,4) 15,XY (2,3) 35,XY (1,1) 32,XY (2,1) 33]

("S.insert",XY (2,1) 13)
-}
