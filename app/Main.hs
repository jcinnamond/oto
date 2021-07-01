{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.State
import Data.Time (UTCTime (utctDayTime), getCurrentTime)
import Lib (shuffle)
import System.Environment (getArgs)
import System.IO.Strict (readFile)
import Prelude hiding (readFile)

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

initialState :: IO OtoState
initialState = do
    let c = loadConfig
    args <- getArgs
    x <- readFile $ filepath c
    let (idxstr : names) = lines x
    pure OtoState{config = c, commands = args, idx = read idxstr, names = names}

saveState :: OtoState -> IO ()
saveState c = writeFile path newNames
  where
    newNames = unlines $ show (idx c) : names c
    path = (filepath . config) c

shuffleNames :: [String] -> IO [String]
shuffleNames l = do
    seed <- floor . realToFrac . utctDayTime <$> getCurrentTime
    pure $ shuffle seed l

newtype OtoConfig = OtoConfig
    { filepath :: String
    }

type Name = String

data OtoState = OtoState
    { config :: OtoConfig
    , commands :: [String]
    , idx :: Int
    , names :: [Name]
    }

loadConfig :: OtoConfig
loadConfig = OtoConfig{filepath = "people2"}

showCurrent :: OtoState -> IO ()
showCurrent c = putStrLn $ "Current person is: " ++ (names c !! idx c)

next :: OtoState -> IO ()
next s = do
    ns <- execStateT run s
    showCurrent ns
    saveState ns

add :: OtoState -> Name -> IO ()
add s n = do
    saveState ns
  where
    ns = s{names = names s ++ [n]}

list :: OtoState -> IO ()
list s = do
    putStrLn "Names are:"
    putStrLn $ unlines $ names s

showUsage :: IO ()
showUsage = putStrLn "usage: oto [next | add <name> | list]"

runCommand :: OtoState -> IO ()
runCommand s@OtoState{commands = []} = showCurrent s
runCommand s@OtoState{commands = ["next"]} = next s
runCommand s@OtoState{commands = ["add", name]} = add s name
runCommand s@OtoState{commands = ["list"]} = list s
runCommand _ = showUsage

main :: IO ()
main = initialState >>= runCommand
