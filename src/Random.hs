module Random (
  shuffle,
) where

import Data.List (sortBy)
import Data.Time (getCurrentTime, utctDayTime)
import OtoState (OtoState (OtoState, idx, names))
import System.Random (Random (randomRs), mkStdGen, uniformR)

type Name = String

shuffle :: Int -> [Name] -> [Name]
shuffle seed xs = map snd $ sortBy firstThing (namesPairedWithRandomValue xs)
 where
  firstThing :: (Int, a) -> (Int, a) -> Ordering
  firstThing x y = compare (fst x) (fst y)

  namesPairedWithRandomValue :: [Name] -> [(Int, Name)]
  namesPairedWithRandomValue ns = zip rs ns

  rs = randomRs (0, length xs - 1) (mkStdGen seed)
