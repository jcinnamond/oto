{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.State (
    Monad ((>>=)),
    MonadIO (liftIO),
    MonadState (get, put),
    StateT,
    execStateT,
 )
import Data.Foldable (toList)
import Data.Sequence (fromList, mapWithIndex)
import Data.Time (UTCTime (utctDayTime), getCurrentTime)
import Lib (shuffle)
import System.Environment (getArgs)
import System.IO.Strict (readFile)
import Prelude hiding (readFile)

import Commands (Command (Command, CommandWithArgs, DefaultCommand), runCommand)
import OtoState (Name, OtoConfig (..), OtoState (..), initialState)

run :: StateT OtoState IO ()
run = do
    ns <- get
    ns' <- liftIO $ newNames ns
    put ns'

newNames :: OtoState -> IO OtoState
newNames c@OtoState{idx = i, names = ns} =
    if i == length ns - 1
        then do
            newNames <- shuffleNames ns
            pure c{idx = 0, names = newNames}
        else pure c{idx = i + 1}

saveState :: OtoState -> IO ()
saveState c = writeFile path newNames
  where
    newNames = unlines $ show (idx c) : names c
    path = (filepath . config) c

shuffleNames :: [String] -> IO [String]
shuffleNames l = do
    seed <- floor . realToFrac . utctDayTime <$> getCurrentTime
    pure $ shuffle seed l

loadConfig :: OtoConfig
loadConfig = OtoConfig{filepath = "people2"}

showCurrent :: OtoState -> IO ()
showCurrent c = putStrLn $ "Current person is: " ++ (names c !! idx c)

next :: OtoState -> IO ()
next s = do
    ns <- execStateT run s
    showCurrent ns
    saveState ns

add :: [Name] -> OtoState -> IO ()
add n s = do
    saveState ns
  where
    ns = s{names = names s ++ n}

list :: OtoState -> IO ()
list s = do
    putStrLn "Names are:"
    putStrLn $ unlines $ toList $ mapWithIndex addPrefix $ fromList $ names s
  where
    addPrefix :: Int -> Name -> String
    addPrefix i n
        | i == idx s = " *> " ++ n
        | otherwise = " -  " ++ n

commands :: [Command]
commands =
    [ DefaultCommand showCurrent
    , Command "list" list
    , CommandWithArgs "add" add
    , Command "next" next
    ]

main :: IO ()
main = initialState loadConfig >>= runCommand commands
