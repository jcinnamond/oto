module Commands (Command (..), runCommand) where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import qualified Data.Maybe
import OtoState (OtoState (args, subcommand))

type Action = OtoState -> IO ()
type ActionWithArgs = [String] -> Action

data Command
    = DefaultCommand Action
    | Command String Action
    | CommandWithArgs String ActionWithArgs

runCommand :: [Command] -> OtoState -> IO ()
runCommand commands s = do
    case subcommand s of
        Nothing -> findDefault commands s
        Just sc -> findCommand sc (args s) commands s

findDefault :: [Command] -> Action
findDefault [] = usage
findDefault (DefaultCommand a : _) = a
findDefault (_ : xs) = findDefault xs

findCommand :: String -> [String] -> [Command] -> Action
findCommand _ _ [] = usage
findCommand want _ (Command sc a : _) | sc == want = a
findCommand want args (CommandWithArgs sc a : _) | sc == want = a args
findCommand sc args (_ : xs) = findCommand sc args xs

usage :: Action
usage _ = putStrLn "Usage: ..."
