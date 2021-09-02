module Main where

import Actions (OtoItem (CurrentItem, OtherItem))
import qualified Actions as A
import Commands (Commands, command, commandWithArgs, defaultCommand, runCommand)
import OtoState (initialConfig, initialState, saveState)

commands :: Commands
commands =
  defaultCommand "show the current person" A.showCurrent
    <> commandWithArgs "add" "[name...]" "add new people to the list" A.add
    <> command "list" "show the randomized list of people" A.list
    <> command "next" "advance to the next random person" A.next
    <> commandWithArgs "remove" "[name...]" "remove people from the list" A.remove
    <> command "shuffle" "shuffles the list" A.shuffle

printResult :: [OtoItem] -> IO ()
printResult = mapM_ (putStrLn . showItem)
 where
  showItem (CurrentItem i) = " *> " ++ i
  showItem (OtherItem i) = " -  " ++ i

main :: IO ()
main = do
  c <- initialConfig
  s <- initialState c
  (s', w) <- runCommand commands c s
  printResult w
  saveState c s'
