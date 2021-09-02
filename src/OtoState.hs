module OtoState (
    OtoState (..),
    OtoConfig (..),
    Name,
    initialConfig,
    initialState,
    saveState,
) where

import Data.List (find, isPrefixOf, partition, stripPrefix, uncons)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime (utctDayTime), getCurrentTime)
import System.Environment (getArgs)
import System.Environment.Blank (getEnvDefault)
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
    , cmd :: Maybe String
    , extraArgs :: [String]
    }
    deriving (Show)

type Name = String

defaultConfig :: [String] -> IO (OtoConfig, [String])
defaultConfig a = do
    home <- getEnvDefault "HOME" "."
    xdgHome <- getEnvDefault "XDG_CONFIG_HOME" (home ++ "/.config")
    pure
        ( OtoConfig
            { filepath = xdgHome ++ "/oto/state"
            , seed = 1
            , cmd = Nothing
            , extraArgs = []
            }
        , a
        )

setSeed :: OtoConfig -> IO OtoConfig
setSeed c = do
    s <- floor . realToFrac . utctDayTime <$> getCurrentTime
    pure c{seed = s}

setCommand :: (OtoConfig, [String]) -> IO OtoConfig
setCommand (c, args) = do
    let splitArgs = uncons args
    let command = fst <$> splitArgs
    let extraArgs = maybe [] snd splitArgs
    pure c{cmd = command, extraArgs = extraArgs}

setFilepath :: (OtoConfig, [String]) -> IO (OtoConfig, [String])
setFilepath (c, args) = do
    let flag = "--state="
    let (opt, rest) = partition (isPrefixOf flag) args
    case opt of
        [] -> pure (c, rest)
        (m : _) -> do
            let path = fromMaybe "" $ stripPrefix flag m
            pure (c{filepath = path}, rest)

initialConfig :: IO OtoConfig
initialConfig = getArgs >>= defaultConfig >>= setFilepath >>= setCommand >>= setSeed

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
