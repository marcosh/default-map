{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultMap.KeyValue where

-- base
import qualified Data.List as L

-- containers
import qualified Data.Map.Internal as M
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as MS

-- hashable
import Data.Hashable ( Hashable )

-- unordered-containers
import qualified Data.HashMap.Internal as H
import qualified Data.HashMap.Lazy as HL
import qualified Data.HashMap.Strict as HS

class KeyValue c k v where
  empty :: c k v
  -- singleton :: k -> v -> c k v
  -- fromList :: [(k, v)] -> c k v
  -- insert :: k -> v -> c k v -> c k v
  -- delete :: k -> c k v -> c k v
  lookup :: k -> c k v -> Maybe v
  -- union :: c k v -> c k v -> c k v
  -- map :: (v1 -> v2) -> c k v1 -> c k v2
  -- foldr :: (v -> a -> a) -> a -> c k v -> a
  -- elems :: c k v -> [v]
  -- keys :: c k v -> [k]

-- list

newtype KeyValueList k v = KeyValueList [(k, v)]

instance Eq k => KeyValue KeyValueList k v where
  empty = KeyValueList []
  lookup k (KeyValueList m) = L.lookup k m

-- lazy maps

newtype LazyMap k v = LazyMap (M.Map k v)

instance Ord k => KeyValue LazyMap k v where
  empty = LazyMap ML.empty
  lookup k (LazyMap m) = ML.lookup k m

-- strict maps

newtype StrictMap k v = StrictMap (M.Map k v)

instance Ord k => KeyValue StrictMap k v where
  empty = StrictMap MS.empty
  lookup k (StrictMap m) = MS.lookup k m

-- lazy hashmaps

newtype LazyHash k v = LazyHash (H.HashMap k v)

instance (Eq k, Hashable k) => KeyValue LazyHash k v where
  empty = LazyHash HL.empty
  lookup k (LazyHash m) = HL.lookup k m

-- strict hashmaps

newtype StrictHash k v = StrictHash (H.HashMap k v)

instance (Eq k, Hashable k) => KeyValue StrictHash k v where
  empty = StrictHash HS.empty
  lookup k (StrictHash m) = HS.lookup k m
