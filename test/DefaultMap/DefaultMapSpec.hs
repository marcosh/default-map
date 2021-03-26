{-# LANGUAGE ScopedTypeVariables #-}

module DefaultMap.DefaultMapSpec where

import qualified DefaultMap.DefaultMap as DM
import DefaultMap.KeyValue

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

-- QuickCheck
import Test.QuickCheck (arbitrary, forAll)

spec :: Spec
spec =
  describe "DefaultMap" $ do
    describe "constant" $ do
      it "contains only the default value" $ do
        forAll arbitrary $
          \(k :: Int, v :: Int) -> DM.lookup k (DM.constant v :: DM.DefaultMap KeyValueList Int Int) `shouldBe` v
