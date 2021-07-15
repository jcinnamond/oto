module Actions (list, showCurrent) where

import Data.Foldable (toList)
import Data.Sequence (fromList, mapWithIndex)
import OtoState (Name, OtoState (OtoState, idx, names))

list :: OtoState -> IO ()
list s = do
    putStrLn "Names are:"
    putStrLn $ unlines $ toList $ mapWithIndex addPrefix $ fromList $ names s
  where
    addPrefix :: Int -> Name -> String
    addPrefix i n
        | i == idx s = " *> " ++ n
        | otherwise = " -  " ++ n

showCurrent :: OtoState -> IO ()
showCurrent c = putStrLn $ "Current person is: " ++ (names c !! idx c)
