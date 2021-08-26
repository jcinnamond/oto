module Commands (defaultCommand, command, commandWithArgs, runCommand, Commands) where

import Actions (Action, ActionWithArgs, OtoItem)
import Control.Monad.RWS (execRWS)
import Data.Foldable (find)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Maybe
import OtoState (OtoConfig (OtoConfig), OtoState ())
import System.Environment (getArgs)

type CommandName = String
type HelpText = String
type ArgsFormat = String

data Command
    = Command Action HelpText
    | CommandWithArgs ActionWithArgs HelpText ArgsFormat

data DefaultCommand = DefaultCommand {dcAction :: Action, dcHelpText :: HelpText}

data Commands = Commands
    { d :: Maybe DefaultCommand
    , cs :: [(String, Command)]
    }

instance Semigroup Commands where
    (<>) x y = Commands{d = resolved, cs = resolvecs}
      where
        resolved
            | isJust $ d y = d y
            | otherwise = d x
        resolvecs = cs x <> cs y

defaultCommand :: HelpText -> Action -> Commands
defaultCommand h a = Commands{d = Just DefaultCommand{dcAction = a, dcHelpText = h}, cs = []}

command :: CommandName -> HelpText -> Action -> Commands
command n h a = Commands{d = Nothing, cs = [(n, Command a h)]}

commandWithArgs :: CommandName -> ArgsFormat -> HelpText -> ActionWithArgs -> Commands
commandWithArgs n af h a = Commands{d = Nothing, cs = [(n, CommandWithArgs a h af)]}

runCommand :: Commands -> OtoConfig -> OtoState -> IO (OtoState, [OtoItem])
runCommand commands config s = do
    args <- getArgs

    let cmd = case safeHead args of
            Nothing -> dcAction <$> d commands
            Just sc -> case find (\(n, _) -> n == sc) (cs commands) of
                Just (_, Command a _) -> Just a
                Just (_, CommandWithArgs a _ _) -> Just $ a (tail args)
                Nothing -> Nothing

    case cmd of
        Nothing -> do
            usage commands
            pure (s, [])
        Just a -> do
            let (s', w) = execRWS a config s
            pure (s', w)
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

usage :: Commands -> IO ()
usage c = do
    putStrLn "Usage: oto [COMMAND]"
    putStrLn ""
    putStrLn "COMMANDS:"
    putStr $ maybe "" (\h -> "    <default>\t\t" ++ dcHelpText h ++ "\n") (d c)
    putStrLn subcommands
  where
    subcommands = unlines $ showCommand <$> cs c
    showCommand (n, Command _ h) = "    " ++ n ++ "\t\t" ++ h
    showCommand (n, CommandWithArgs _ h af) = "    " ++ n ++ " " ++ af ++ "\t" ++ h
