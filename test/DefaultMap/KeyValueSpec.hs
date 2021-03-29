{-# LANGUAGE ScopedTypeVariables #-}

module DefaultMap.KeyValueSpec where

import DefaultMap.KeyValue

-- base
import Data.List

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

-- QuickCheck
import Test.QuickCheck (arbitrary, forAll)

spec :: Spec
spec =
  describe "KeyValue" $ do
    describe "KeyValueList" $ do
      describe "fromList" $ do
        it "" $ do
          forAll arbitrary $
            \(m :: [(Int, Int)]) -> fromList m `shouldBe` fromList (nubBy (\kv1 kv2 -> fst kv1 == fst kv2) m)

      describe "insertKVL" $ do
        it "deletes the duplicated key when inserting a new element" $ do
          insertKVL (1 :: Integer, "a") (fromList [(1, "b"), (2, "c"), (1, "d"), (3, "e")])
            `shouldBe` fromList [(1, "a"), (2, "c"), (3, "e")]
