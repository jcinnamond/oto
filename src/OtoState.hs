module OtoState (
    OtoState (..),
    OtoConfig (..),
    Name,
    initialConfig,
    initialState,
    blankState,
    saveState,
) where

import Control.Exception (catch)
import Control.Monad (when)
import Data.List (isPrefixOf, partition, stripPrefix, uncons)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime (utctDayTime), getCurrentTime)
import System.Environment (getArgs, getProgName)
import System.Environment.Blank (getEnvDefault)
import System.IO.Error (isDoesNotExistError)
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
    , needInit :: Bool
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
            , needInit = False
            }
        , a
        )

setSeed :: OtoConfig -> IO OtoConfig
setSeed c = do
    s <- floor . utctDayTime <$> getCurrentTime
    pure c{seed = s}

setCommand :: (OtoConfig, [String]) -> IO OtoConfig
setCommand (c, args) = do
    let splitArgs = uncons args
    let command = fst <$> splitArgs
    let xArgs = maybe [] snd splitArgs
    pure c{cmd = command, extraArgs = xArgs}

setFilepath :: (OtoConfig, [String]) -> IO (OtoConfig, [String])
setFilepath (c, args) = do
    let flag = "--state="
    let (opt, rest) = partition (isPrefixOf flag) args
    case opt of
        [] -> pure (c, rest)
        (m : _) -> do
            let path = fromMaybe "" $ stripPrefix flag m
            pure (c{filepath = path}, rest)

setInit :: (OtoConfig, [String]) -> IO (OtoConfig, [String])
setInit (c, args) = do
    let flag = "--init"
    let (opt, rest) = partition (isPrefixOf flag) args
    case opt of
        [] -> pure (c, rest)
        _ -> do
            pure (c{needInit = True}, rest)

initialConfig :: IO OtoConfig
initialConfig = getArgs >>= defaultConfig >>= setInit >>= setFilepath >>= setCommand >>= setSeed

initialState :: OtoConfig -> IO OtoState
initialState c = do
    x <- readFile (filepath c) `catch` warnReadError
    let (idxstr : ns) = lines x
    pure OtoState{idx = read idxstr, names = ns}

warnReadError :: IOError -> IO String
warnReadError e = do
    when (isDoesNotExistError e) $ do
        pname <- getProgName
        putStrLn $ pname ++ ": run `" ++ pname ++ " --init` to create a new state file"
    ioError e

blankState :: OtoState
blankState = OtoState{idx = 0, names = []}

saveState :: OtoConfig -> OtoState -> IO ()
saveState c s = writeFile path newNames
  where
    newNames = unlines $ show (idx s) : names s
    path = filepath c
