module Main where

import Actions (removeMany)
import Control.Monad.State (liftIO)
import OtoState (OtoState (OtoState, fileName, idx, names, seed))
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $
    describe "remove" $ do
        it "ignores people not in the list" $ do
            removeMany ["Boppy"] s `shouldBe` s
        it "removes people later in the list" $ do
            removeMany ["Bippy"] s `shouldBe` s{names = ["Beppy", "Buppy"]}
        it "removes people earlier in the list" $ do
            removeMany ["Beppy"] s `shouldBe` s{names = ["Buppy", "Bippy"], idx = 0}
        it "removes the current person" $ do
            removeMany ["Buppy"] s `shouldBe` s{names = ["Beppy", "Bippy"]}
        it "removes the current person at the end of the list" $ do
            removeMany ["Bippy"] s{idx = 2} `shouldBe` s{names = ["Buppy", "Beppy"], idx = 0}
        it "removes multiple names from the list" $ do
            removeMany ["Beppy", "Bippy"] s `shouldBe` s{names = ["Buppy"], idx = 0}
        it "removes multiple names including the current last" $ do
            removeMany ["Bippy", "Buppy"] s{idx = 2} `shouldBe` s{names = ["Beppy"], idx = 0}
  where
    -- setting the seed to 2 shuffles correctly for the test
    s = OtoState{fileName = "", idx = 1, names = ["Beppy", "Buppy", "Bippy"], seed = 2}
