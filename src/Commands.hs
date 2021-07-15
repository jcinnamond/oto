module Commands (defaultCommand, command, commandWithArgs, runCommand, Commands) where

import Data.Foldable (find)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Maybe
import OtoState (OtoState (args, subcommand))

type Action = OtoState -> IO ()
type ActionWithArgs = [String] -> Action

data Command
    = Command Action
    | CommandWithArgs ActionWithArgs

data Commands = Commands
    { d :: Maybe Action
    , cs :: [(String, Command)]
    }

instance Semigroup Commands where
    (<>) x y = Commands{d = resolved, cs = resolvecs}
      where
        resolved
            | isJust $ d y = d y
            | otherwise = d x
        resolvecs = cs x <> cs y

defaultCommand :: Action -> Commands
defaultCommand a = Commands{d = Just a, cs = []}

command :: String -> Action -> Commands
command n a = Commands{d = Nothing, cs = [(n, Command a)]}

commandWithArgs :: String -> ActionWithArgs -> Commands
commandWithArgs n a = Commands{d = Nothing, cs = [(n, CommandWithArgs a)]}

runCommand :: Commands -> OtoState -> IO ()
runCommand commands s = do
    case subcommand s of
        Nothing -> (fromMaybe (usage commands) $ d commands) s
        Just sc -> case find (\(n, _) -> n == sc) (cs commands) of
            Just (_, Command a) -> a s
            Just (_, CommandWithArgs a) -> a (args s) s
            Nothing -> usage commands s

usage :: Commands -> Action
usage c _ = do
    putStrLn "Usage: oto [COMMAND]"
    putStrLn ""
    putStrLn "COMMANDS:"
    putStrLn subcommands
  where
    subcommands = unlines $ showCommand <$> cs c
    showCommand (n, Command _) = "    " ++ n
    showCommand (n, CommandWithArgs _) = "    " ++ n ++ " <args>"
