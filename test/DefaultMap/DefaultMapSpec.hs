{-# LANGUAGE ScopedTypeVariables #-}

module DefaultMap.DefaultMapSpec where

import qualified DefaultMap.DefaultMap as DM
import DefaultMap.KeyValue

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

-- QuickCheck
import Test.QuickCheck (Gen, arbitrary, forAll)

arbitraryDefaultMap :: Gen (DM.DefaultMap KeyValueList Int Int)
arbitraryDefaultMap = DM.DefaultMap -- . KeyValueList <$> listOf ((,) <$> _asdf <*> _qwer)
  <$> (KeyValueList <$> arbitrary)
  <*> arbitrary

spec :: Spec
spec =
  describe "DefaultMap" $ do
    describe "==" $ do
      it "is reflexive" $ do
        forAll arbitraryDefaultMap $
          \dm -> dm == dm

    describe "constant" $ do
      it "contains only the default value" $ do
        forAll arbitrary $
          \(k :: Int, v :: Int) -> DM.lookup k (DM.constant v :: DM.DefaultMap KeyValueList Int Int) `shouldBe` v
