module Main where

import Actions (OtoItem (CurrentItem, OtherItem), add, delay, list, next, remove, showCurrent, shuffle)
import Control.Monad.RWS (execRWS)
import Control.Monad.State (liftIO)
import OtoState (OtoConfig (OtoConfig, filepath, seed), OtoState (OtoState, idx, names))
import Test.Hspec (describe, hspec, it, shouldBe)

initialConfig :: OtoConfig
initialConfig = OtoConfig{filepath = "", seed = 2}

initialState :: OtoState
initialState = OtoState{idx = 1, names = ["Beppy", "Buppy", "Bippy"]}

testShowCurrent :: IO ()
testShowCurrent = hspec $ do
    describe "show current" $ do
        it "returns the current person" $ do
            let (s, w) = execRWS showCurrent initialConfig initialState
            w `shouldBe` [CurrentItem "Buppy"]
            s `shouldBe` initialState{idx = 1}

testList :: IO ()
testList = hspec $ do
    describe "list" $ do
        it "returns everyone in the list" $ do
            let (s, w) = execRWS list initialConfig initialState
            w `shouldBe` [OtherItem "Beppy", CurrentItem "Buppy", OtherItem "Bippy"]
            s `shouldBe` initialState

testNext :: IO ()
testNext = hspec $ do
    describe "next" $ do
        it "returns the next person" $ do
            let (s, w) = execRWS next initialConfig initialState
            w `shouldBe` [CurrentItem "Bippy"]
            s `shouldBe` initialState{idx = 2}
        it "shuffles at the end of the list" $ do
            let (s, w) = execRWS next initialConfig initialState{idx = 3}
            w `shouldBe` [CurrentItem "Buppy"]
            s `shouldBe` initialState{idx = 0, names = ["Buppy", "Beppy", "Bippy"]}

testAdd :: IO ()
testAdd = hspec $ do
    describe "add" $ do
        it "adds people to the list" $ do
            let newNames = ["Pleppy", "Pooty"]
            let (s, w) = execRWS (add newNames) initialConfig initialState
            w `shouldBe` []
            s `shouldBe` initialState{names = names initialState ++ newNames}

testRemove :: IO ()
testRemove = hspec $ do
    describe "remove" $ do
        it "ignores people not in the list" $ do
            let (s, w) = execRWS (remove ["Betty S"]) initialConfig initialState
            s `shouldBe` initialState

        it "removes people later in the list" $ do
            let (s, w) = execRWS (remove ["Bippy"]) initialConfig initialState
            w `shouldBe` []
            s `shouldBe` initialState{names = ["Beppy", "Buppy"]}

        it "removes people earlier in the list" $ do
            let (s, w) = execRWS (remove ["Beppy"]) initialConfig initialState
            s `shouldBe` s{names = ["Buppy", "Bippy"], idx = 0}

        it "removes the current person" $ do
            let (s, w) = execRWS (remove ["Buppy"]) initialConfig initialState
            s `shouldBe` s{names = ["Beppy", "Bippy"]}

        it "removes the current person at the end of the list" $ do
            let (s, w) = execRWS (remove ["Bippy"]) initialConfig initialState{idx = 2}
            s `shouldBe` s{names = ["Buppy", "Beppy"], idx = 0}

        it "removes multiple names from the list" $ do
            let (s, w) = execRWS (remove ["Beppy", "Bippy"]) initialConfig initialState
            s `shouldBe` s{names = ["Buppy"], idx = 0}

        it "removes multiple names including the current last" $ do
            let (s, w) = execRWS (remove ["Bippy", "Buppy"]) initialConfig initialState{idx = 2}
            s `shouldBe` s{names = ["Beppy"], idx = 0}

testShuffle :: IO ()
testShuffle = hspec $ do
    describe "shuffle" $ do
        it "returns the shuffled list" $ do
            let (s, w) = execRWS shuffle initialConfig initialState
            s `shouldBe` s{names = ["Buppy", "Beppy", "Bippy"], idx = 0}

testDelay :: IO ()
testDelay = hspec $ do
    describe "delay" $ do
        it "moves the current person forward in the list" $ do
            let (s, w) = execRWS delay initialConfig initialState
            s `shouldBe` s{names = ["Beppy", "Bippy", "Buppy"]}
            w `shouldBe` [CurrentItem "Bippy"]
        it "shuffles the list if the current person is at the end" $ do
            let (s, w) = execRWS delay initialConfig initialState{idx = 2}
            s `shouldBe` s{names = ["Buppy", "Bippy", "Beppy"]}
            w `shouldBe` [CurrentItem "Buppy"]
        it "shuffles and moves the current person if they are at the start of the shuffled list" $ do
            let (s, w) = execRWS delay initialConfig{seed = 1} initialState{idx = 2}
            s `shouldBe` s{names = ["Beppy", "Bippy", "Buppy"]}
            w `shouldBe` [CurrentItem "Beppy"]

main :: IO ()
main = testNext >> testAdd >> testRemove >> testShowCurrent >> testList >> testShuffle >> testDelay