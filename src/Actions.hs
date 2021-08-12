module Actions (
  list,
  showCurrent,
  next,
  add,
  remove,
  shuffle,
  Action (..),
  ActionWithArgs (..),
) where

import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), State, StateT, execState, execStateT)
import Data.Foldable (toList)
import Data.List (delete, elemIndex)
import Data.Sequence (fromList, mapWithIndex)
import Data.Time (getCurrentTime, utctDayTime)
import OtoState (Name, OtoState (OtoState, fileName, idx, names, seed), saveState)
import qualified Random as R (maybeShuffle, shuffle)

type Action = OtoState -> IO OtoState
type ActionWithArgs = [String] -> Action

list :: OtoState -> IO OtoState
list s = do
  putStrLn "Names are:"
  putStrLn $ unlines $ toList $ mapWithIndex addPrefix $ fromList $ names s
  pure s
 where
  addPrefix :: Int -> Name -> String
  addPrefix i n
    | i == idx s = " *> " ++ n
    | otherwise = " -  " ++ n

showCurrent :: OtoState -> IO OtoState
showCurrent s = putStrLn ("Current person is: " ++ (names s !! idx s)) >> pure s

next :: OtoState -> IO OtoState
next s = showCurrent $ R.maybeShuffle s{idx = succ (idx s)}

add :: [Name] -> OtoState -> IO OtoState
add n s = pure s{names = names s ++ n}

remove :: [Name] -> OtoState -> IO OtoState
remove ns s = pure $ execState (mapM removeOne ns) s

removeOne :: Name -> State OtoState ()
removeOne n = do
  s <- get
  let newNames = delete n $ names s
  let removeBefore x = x < idx s
  let removeLastName = idx s == length newNames
  let shuffledNames = R.shuffle (seed s) newNames
  case elemIndex n (names s) of
    Nothing -> pure ()
    Just x | removeBefore x -> put s{idx = pred $ idx s, names = newNames}
    Just x | removeLastName -> put s{idx = 0, names = shuffledNames}
    _ -> put s{names = newNames}

shuffle :: OtoState -> IO OtoState
shuffle s = pure s{names = R.shuffle (seed s) (names s), idx = 0}