{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.State
import Data.Time (UTCTime (utctDayTime), getCurrentTime)
import Lib (shuffle)
import System.IO.Strict (readFile)
import Prelude hiding (readFile)

data Names = Names
    { idx :: Int
    , names :: [String]
    }

run :: StateT Names IO ()
run = do
    ns <- get
    ns' <- liftIO $ newNames ns
    put ns'

newNames :: Names -> IO Names
newNames n@Names{idx = i, names = ns} =
    if i == length ns - 1
        then do
            newNames <- shuffleNames ns
            pure Names{idx = 0, names = newNames}
        else pure n{idx = i + 1}

initialState :: IO Names
initialState = do
    x <- readFile "people2"
    let (idxstr : names) = lines x
    pure Names{idx = read idxstr, names = names}

saveState :: Names -> IO ()
saveState ns = do
    let newNames = unlines $ show (idx ns) : names ns
    writeFile "people2" newNames

shuffleNames :: [String] -> IO [String]
shuffleNames l = do
    seed <- floor . realToFrac . utctDayTime <$> getCurrentTime
    pure $ shuffle seed l

main :: IO ()
main = do
    s <- initialState
    putStrLn $ "The current person is: " ++ (names s !! idx s)
    ns <- execStateT run s
    saveState ns
