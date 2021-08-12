module Main where

import Actions (add, remove, shuffle)
import Control.Monad.State (liftIO)
import OtoState (OtoState (OtoState, fileName, idx, names, seed))
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    describe "add" $ do
        it "adds people to the list" $ do
            s' <- add ["Dappy", "Doppy"] s
            s' `shouldBe` s{names = ["Beppy", "Buppy", "Bippy", "Dappy", "Doppy"]}

    describe "remove" $ do
        it "adds people to the list" $ do
            s' <- add ["Dappy", "Doppy"] s
            s' `shouldBe` s{names = ["Beppy", "Buppy", "Bippy", "Dappy", "Doppy"]}
        it "ignores people not in the list" $ do
            s' <- remove ["Boppy s"] s
            s' `shouldBe` s
        it "removes people later in the list" $ do
            s' <- remove ["Bippy"] s
            s' `shouldBe` s{names = ["Beppy", "Buppy"]}
        it "removes people earlier in the list" $ do
            s' <- remove ["Beppy"] s
            s' `shouldBe` s{names = ["Buppy", "Bippy"], idx = 0}
        it "removes the current person" $ do
            s' <- remove ["Buppy"] s
            s' `shouldBe` s{names = ["Beppy", "Bippy"]}
        it "removes the current person at the end of the list" $ do
            s' <- remove ["Bippy"] s{idx = 2}
            s' `shouldBe` s{names = ["Buppy", "Beppy"], idx = 0}
        it "removes multiple names from the list" $ do
            s' <- remove ["Beppy", "Bippy"] s
            s' `shouldBe` s{names = ["Buppy"], idx = 0}
        it "removes multiple names including the current last" $ do
            s' <- remove ["Bippy", "Buppy"] s{idx = 2}
            s' `shouldBe` s{names = ["Beppy"], idx = 0}

    describe "shuffle" $ do
        it "shuffles the list" $ do
            s' <- shuffle s
            s' `shouldBe` s{names = ["Buppy", "Beppy", "Bippy"], idx = 0}
  where
    -- setting the seed to 2 shuffles correctly for the test
    s = OtoState{fileName = "", idx = 1, names = ["Beppy", "Buppy", "Bippy"], seed = 2}
