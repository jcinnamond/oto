module OtoState (
    OtoState (..),
    OtoConfig (..),
    Name,
    initialConfig,
    initialState,
    saveState,
) where

import Control.Exception (catch)
import Control.Monad (when)
import Data.List (isPrefixOf, partition, stripPrefix, uncons)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime (utctDayTime), getCurrentTime)
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, getXdgDirectory)
import System.Environment (getArgs, getProgName)
import System.Environment.Blank (getEnvDefault)
import System.FilePath (takeDirectory, (</>))
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
    }
    deriving (Show)

type Name = String

defaultConfig :: [String] -> IO (OtoConfig, [String])
defaultConfig a = do
    dir <- getXdgDirectory XdgConfig "oto"
    pure
        ( OtoConfig
            { filepath = dir </> "state"
            , seed = 1
            , cmd = Nothing
            , extraArgs = []
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

initialConfig :: IO OtoConfig
initialConfig = getArgs >>= defaultConfig >>= setFilepath >>= setCommand >>= setSeed

initialState :: OtoConfig -> IO OtoState
initialState c = do
    x <- readFile (filepath c) `catch` handleReadError (filepath c)
    let (idxstr : ns) = lines x
    pure OtoState{idx = read idxstr, names = ns}

handleReadError :: String -> IOError -> IO String
handleReadError f e
    | isDoesNotExistError e = do
        pname <- getProgName
        putStrLn $ pname ++ ": `" ++ f ++ "` does not exist -- creating empty state"
        return "0"
    | otherwise = ioError e

saveState :: OtoConfig -> OtoState -> IO ()
saveState c s = createDirectoryIfMissing True dir >> writeFile path newNames
  where
    newNames = unlines $ show (idx s) : names s
    path = filepath c
    dir = takeDirectory path
