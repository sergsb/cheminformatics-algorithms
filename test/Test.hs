module Main where
import VF2_test (prop_isIsomorph_self, prop_permutedIsomorph)
import Enumeration_test (prop_orig_paper, prop_fullerene_c60)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "VF2" $ do
        it "Self isomorphism" $ do
            property prop_isIsomorph_self
    describe "VF2" $ do
        it "Permutation isomorphism" $ do
            property prop_permutedIsomorph
    describe "Subgraphs Enumeration" $ do
        it "Original Paper" $ do
            property prop_orig_paper
        it "Fullerene C60" $ do
            property prop_fullerene_c60
