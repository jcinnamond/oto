module Main where

import qualified Actions as A
import Commands (Commands, command, commandWithArgs, defaultCommand, runCommand)
import OtoState (initialState, saveState)

commands :: Commands
commands =
    defaultCommand "show the current person" A.showCurrent
        <> commandWithArgs "add" "[name...]" "add new people to the list" A.add
        <> command "list" "show the randomized list of people" A.list
        <> command "next" "advance to the next random person" A.next
        <> commandWithArgs "remove" "[name...]" "remove people from the list" A.remove

main :: IO ()
main = initialState >>= runCommand commands >>= saveState
