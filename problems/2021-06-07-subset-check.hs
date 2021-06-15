module Main where

import Algebra.PartialOrd (PartialOrd(..))
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.Function (on)
import Data.List.Extra (groupSortOn)
import Data.List (sortOn)
import Data.Set (Set)
import qualified Data.POSet as POSet
import qualified Data.Set as S
import qualified Data.Set as Set

newtype Partition = Partition { getPart :: [Set Int] } deriving (Eq, Show, Ord)

instance PartialOrd Partition where
  (Partition as) `leq` (Partition bs) = all (\a -> any (\b -> a `Set.isSubsetOf` b) bs) as

type Problem = [Partition] -- list of partitions that should be filtered to become pairwise distinct
type Problems = [(Problem, Problem)] -- question and the answer

problems :: Problems
problems = map (bimap (map (Partition . map S.fromList)) (map (Partition . map S.fromList)))
  [
    ( [[[1], [2]]]
    , [[[1], [2]]]
    )
  ,
    ( [[[1], [2]], [[1, 2]]]
    , [[[1, 2]]]
    )
  ,
    ( [[[1, 2], [3, 4]], [[1, 2, 3], [4]]]
    , [[[1, 2], [3, 4]], [[1, 2, 3], [4]]]
    )
  ,
    ( [[[1, 2], [3, 4]], [[1, 2, 3, 4]]]
    , [[[1, 2, 3, 4]]]
    )
  ,
    ( [map return [1, 2, 3, 4] ++ [[5]], map return [1, 2, 3] ++ [[4, 5]]]
    , [map return [1, 2, 3] ++ [[4, 5]]]
    )
  ]

-- the problem is to partiton the graphs by "vertex unions" and return any from the partition
-- that is a graph A is equivalent to graph B, if A has two vertexes V and V',
-- then A is equivalent to B, if B = [V \cup V'].
-- UPDATE: alternatively you can say that A <= B if all A elements are subsets of some element in B
-- which is also called "coarser" since we're talking about partitions
-- https://en.wikipedia.org/wiki/Partition_of_a_set#Refinement_of_partitions
f :: Problem -> Problem
f = POSet.lookupMax . POSet.fromList

main = for_ problems . uncurry $ \problem answer -> do
  print (((==) `on` Set.fromList) (f problem) answer)
  print problem
  print (f problem)
  print answer
  putStrLn ""
