module Actions (list, showCurrent, next, add, remove, removeOne, removeMany) where

import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), State, StateT, execState, execStateT)
import Data.Foldable (toList)
import Data.List (delete, elemIndex)
import Data.Sequence (fromList, mapWithIndex)
import Data.Time (getCurrentTime, utctDayTime)
import Lib (maybeShuffle, shuffle)
import OtoState (Name, OtoState (OtoState, fileName, idx, names, seed), saveState)

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

next :: OtoState -> IO ()
next s = do
  ns <- execStateT run s
  showCurrent ns
  saveState ns

add :: [Name] -> OtoState -> IO ()
add n s = do
  saveState s{names = names s ++ n}

remove :: [Name] -> OtoState -> IO ()
remove ns s = saveState $ removeMany ns s

removeMany :: [Name] -> OtoState -> OtoState
removeMany ns = execState (mapM removeOne ns)

removeOne :: Name -> State OtoState ()
removeOne n = do
  s <- get
  let newNames = delete n $ names s
  let removeBefore x = x < idx s
  let removeLastName = idx s == length newNames
  let shuffledNames = shuffle (seed s) newNames
  case elemIndex n (names s) of
    Nothing -> pure ()
    Just x | removeBefore x -> put s{idx = pred $ idx s, names = newNames}
    Just x | removeLastName -> put s{idx = 0, names = shuffledNames}
    _ -> put s{names = newNames}

run :: StateT OtoState IO ()
run = do
  s <- get
  put $ maybeShuffle s{idx = succ (idx s)}
