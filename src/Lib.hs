module Lib (
  shuffle,
  maybeShuffle,
) where

import Data.List (sortBy)
import Data.Time (getCurrentTime, utctDayTime)
import OtoState (OtoState (OtoState, idx, names, seed))
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

maybeShuffle :: OtoState -> OtoState
maybeShuffle s@OtoState{idx = i, names = ns, seed = seed} =
  if i >= length ns
    then s{idx = 0, names = shuffle seed ns}
    else s
