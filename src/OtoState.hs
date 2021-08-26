module OtoState (
    OtoState (..),
    OtoConfig (..),
    Name,
    initialConfig,
    initialState,
    saveState,
) where

import Data.Time (UTCTime (utctDayTime), getCurrentTime)
import System.Environment (getArgs)
import System.IO.Strict (readFile)
import Prelude hiding (readFile)

data OtoState = OtoState
    { idx :: Int
    , names :: [Name]
    }
    deriving (Show, Eq)

data OtoConfig = OtoConfig
    { filepath :: String
    , seed :: Int
    }

type Name = String

initialConfig :: IO OtoConfig
initialConfig = do
    seed <- floor . realToFrac . utctDayTime <$> getCurrentTime
    pure
        OtoConfig
            { filepath = "people2"
            , seed = seed
            }

initialState :: OtoConfig -> IO OtoState
initialState c = do
    x <- readFile (filepath c)
    let (idxstr : names) = lines x
    pure OtoState{idx = read idxstr, names = names}

saveState :: OtoConfig -> OtoState -> IO ()
saveState c s = writeFile path newNames
  where
    newNames = unlines $ show (idx s) : names s
    path = filepath c
