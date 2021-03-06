{-# LANGUAGE NamedFieldPuns #-}

module Actions (
  list,
  showCurrent,
  next,
  add,
  remove,
  shuffle,
  delay,
  Action,
  ActionWithArgs,
  OtoItem (..),
) where

import Control.Monad.RWS (MonadReader (ask), MonadWriter (tell), RWS)
import Control.Monad.State (MonadState (get, put))
import Data.Foldable (toList)
import Data.List (delete, elemIndex)
import Data.Sequence (fromList, mapWithIndex)
import OtoState (Name, OtoConfig (OtoConfig, seed), OtoState (OtoState, idx, names))
import qualified Random as R (shuffle)

type Action = RWS OtoConfig [OtoItem] OtoState ()
type ActionWithArgs = [String] -> Action

data OtoItem
  = CurrentItem Name
  | OtherItem Name
  | NoItems
  deriving (Show, Eq)

list :: Action
list = do
  OtoState{names, idx} <- get
  tell $ toList $ mapWithIndex (toOtoItem idx) $ fromList names
 where
  toOtoItem :: Int -> Int -> Name -> OtoItem
  toOtoItem idx i n
    | i == idx = CurrentItem n
    | otherwise = OtherItem n

showCurrent :: Action
showCurrent = do
  OtoState{names, idx} <- get
  case names of
    [] -> tell [NoItems]
    _ -> tell [CurrentItem $ names !! idx]

next :: Action
next = do
  c <- ask
  s <- get
  let s' =
        if idx s >= length (names s)
          then s{idx = 0, names = R.shuffle (seed c) (names s)}
          else s{idx = idx s + 1}
  put s'
  tell [CurrentItem $ names s' !! idx s']

add :: [Name] -> Action
add ns = do
  s <- get
  put s{names = names s ++ ns}

remove :: [Name] -> Action
remove = mapM_ removeOne

removeOne :: Name -> Action
removeOne n = do
  OtoConfig{seed = seed} <- ask
  s@OtoState{names = ns, idx = i} <- get
  let newNames = delete n ns
  let removeBefore x = x < i
  let removeLastName = i == length newNames
  let shuffledNames = R.shuffle seed newNames
  case elemIndex n (names s) of
    Nothing -> pure ()
    Just x | removeBefore x -> put s{idx = pred $ idx s, names = newNames}
    Just _ | removeLastName -> put s{idx = 0, names = shuffledNames}
    _ -> put s{names = newNames}

shuffle :: Action
shuffle = do
  OtoConfig{seed} <- ask
  s <- get
  put s{names = R.shuffle seed (names s), idx = 0}

delay :: Action
delay = do
  OtoState{names = ns, idx = i} <- get
  let n = ns !! i
  removeOne n
  addAfter n
  showCurrent

addAfter :: Name -> Action
addAfter n = do
  s@OtoState{names = ns, idx = i} <- get
  let (xs, ys) = splitAt (i + 1) ns
  put s{names = xs ++ [n] ++ ys}
