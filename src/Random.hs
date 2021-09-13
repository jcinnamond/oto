module Random (
  shuffle,
) where

import Data.List (sortBy)
import Data.Time (getCurrentTime, utctDayTime)
import OtoState (OtoState (OtoState, idx, names))
import System.Random (Random (randoms), mkStdGen)

type Name = String

shuffle :: Int -> [Name] -> [Name]
shuffle seed xs = map snd $ sortBy listOrder (zip rs xs)
 where
  listOrder :: Ord a => (a, b) -> (a, b) -> Ordering
  listOrder x y = compare (fst x) (fst y)
  rs = randoms (mkStdGen seed) :: [Int]
