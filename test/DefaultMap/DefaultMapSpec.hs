{-# LANGUAGE ScopedTypeVariables #-}

module DefaultMap.DefaultMapSpec where

import qualified DefaultMap.DefaultMap as DM
import DefaultMap.DefaultMap (DefaultMap(DefaultMap))
import DefaultMap.KeyValue

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

-- QuickCheck
import Test.QuickCheck (Gen, arbitrary, forAll, suchThat)

arbitraryKeyValueList :: Gen (KeyValueList Int Int)
arbitraryKeyValueList = KeyValueList <$> arbitrary

arbitraryDefaultMap :: Gen (DM.DefaultMap KeyValueList Int Int)
arbitraryDefaultMap = DM.DefaultMap -- . KeyValueList <$> listOf ((,) <$> _asdf <*> _qwer)
  <$> arbitraryKeyValueList
  <*> arbitrary

spec :: Spec
spec =
  describe "DefaultMap" $ do
    describe "==" $ do
      it "is reflexive" $ do
        forAll arbitraryDefaultMap $
          \dm -> dm `shouldBe` dm

      it "does distinguish different defaults" $ do
        forAll (((,,) <$> arbitraryKeyValueList <*> arbitrary <*> arbitrary) `suchThat` \(_, d1, d2) -> d1 /= d2) $ do
          \(m :: KeyValueList Int Int, d1 :: Int, d2 :: Int) -> DefaultMap m d1 `shouldNotBe` DefaultMap m d2

      it "does distinguish different maps" $ do
        forAll (((,,) <$> arbitraryKeyValueList <*> arbitraryKeyValueList <*> arbitrary)
          `suchThat` \(m1, m2, _) -> m1 /= m2) $ do
            \(m1 :: KeyValueList Int Int, m2 :: KeyValueList Int Int, d :: Int) ->
              DefaultMap m1 d `shouldNotBe` DefaultMap m2 d

    describe "show" $ do
      it "uses the show instances of the map and the default value" $ do
        forAll arbitraryDefaultMap $
          \dm@(DefaultMap m d) -> show dm `shouldBe` "DefaultMap " ++ show m ++ " " ++ show d

    describe "constant" $ do
      it "contains only the default value" $ do
        forAll arbitrary $
          \(k :: Int, v :: Int) -> DM.lookup k (DM.constant v :: DefaultMap KeyValueList Int Int) `shouldBe` v
