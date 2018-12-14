--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 6: Functors                                                            --
--------------------------------------------------------------------------------

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Char

import qualified Lab6 as L

--------------------------------------------------------------------------------

-- | The main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "ch" $ do
        it "fails when given the empty list" $
            L.parse (L.ch undefined) "" `shouldBe` Nothing
        it "fails when given an input which does not satisfy the predicate" $
            L.parse (L.ch (=='x')) "zyx" `shouldBe` Nothing
        prop "succeeds when given an input which does satisfy the predicate" $
            \x xs -> L.parse (L.ch (==x)) (x:xs) == Just (x,xs)
    describe "Parser is a Functor" $ do
        it "applies isUpper to the result of a successful parser" $
            L.parse (fmap isUpper (L.ch (=='x'))) "xyz"
            `shouldBe` Just (False,"yz")
        it "fails when applying isUpper to the result of a failed parser" $
            L.parse (fmap isUpper (L.ch (=='y'))) "xyz"
            `shouldBe` Nothing
        it "applies digitToInt to the result of a successful parser" $
            L.parse (fmap digitToInt (L.ch isDigit)) "1xy"
            `shouldBe` Just (1,"xy")
        it "fails when applying digitToInt to the result of a failed parser" $
            L.parse (fmap digitToInt (L.ch isDigit)) "xy"
            `shouldBe` Nothing

--------------------------------------------------------------------------------
