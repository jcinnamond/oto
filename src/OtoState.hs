module OtoState (OtoState (..), OtoConfig (..), Name, initialState, saveState) where

import Data.Time (UTCTime (utctDayTime), getCurrentTime)
import System.Environment (getArgs)
import System.IO.Strict (readFile)
import Prelude hiding (readFile)

data OtoState = OtoState
    { fileName :: String
    , idx :: Int
    , names :: [Name]
    , seed :: Int
    }
    deriving (Show, Eq)

newtype OtoConfig = OtoConfig
    { filepath :: String
    }

type Name = String

initialState :: IO OtoState
initialState = do
    let filename = "people2"
    x <- readFile filename
    let (idxstr : names) = lines x
    seed <- floor . realToFrac . utctDayTime <$> getCurrentTime
    pure OtoState{fileName = filename, idx = read idxstr, names = names, seed = seed}

saveState :: OtoState -> IO ()
saveState c = writeFile path newNames
  where
    newNames = unlines $ show (idx c) : names c
    path = fileName c
