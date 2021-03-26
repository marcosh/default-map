module DefaultMap.KeyValueSpec where

import DefaultMap.KeyValue

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "KeyValue" $ do
    describe "KeyValueList" $ do
      describe "deDuplicateKeys" $ do
        it "deletes the repeated keys on the right" $ do -- TODO: find a reasonable property for this test
          deDuplicateKeys [(1 :: Integer, "a"), (1, "b"), (2, "c"), (1, "d"), (2, "e"), (3, "f")]
            `shouldBe` KeyValueList [(1, "a"), (2, "c"), (3, "f")]
