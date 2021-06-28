module Lib (
    shuffle,
) where

import Data.List (sortBy)
import System.Random (Random (randomRs), mkStdGen, uniformR)

type Name = String

shuffle :: Int -> [Name] -> [Name]
shuffle seed xs = map fst $ sortBy secondThing (namesPairedWithRandomValue xs)
  where
    secondThing :: (a, Int) -> (a, Int) -> Ordering
    secondThing x y = compare (snd x) (snd y)

    namesPairedWithRandomValue :: [Name] -> [(Name, Int)]
    namesPairedWithRandomValue ns = zip ns rs

    rs = randomRs (0, length xs - 1) (mkStdGen seed)
