module OtoState (OtoState (..), OtoConfig (..), Name, initialState) where

import System.Environment (getArgs)
import System.IO.Strict (readFile)
import Prelude hiding (readFile)

data OtoState = OtoState
    { config :: OtoConfig
    , subcommand :: Maybe String
    , args :: [String]
    , idx :: Int
    , names :: [Name]
    }

newtype OtoConfig = OtoConfig
    { filepath :: String
    }

type Name = String

initialState :: OtoConfig -> IO OtoState
initialState c = do
    args <- getArgs
    x <- readFile $ filepath c
    let (idxstr : names) = lines x
    pure OtoState{config = c, subcommand = safeHead args, args = tail args, idx = read idxstr, names = names}
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x
