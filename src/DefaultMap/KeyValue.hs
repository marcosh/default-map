{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultMap.KeyValue
  ( KeyValue(..)
  , KeyValueList
  , fromList
  , insertKVL
  , LazyMap(..)
  , StrictMap(..)
  , LazyHash(..)
  , StrictHash(..)
  ) where

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
  insert :: k -> v -> c k v -> c k v
  -- delete :: k -> c k v -> c k v
  lookup :: k -> c k v -> Maybe v
  -- union :: c k v -> c k v -> c k v
  -- map :: (v1 -> v2) -> c k v1 -> c k v2
  -- foldr :: (v -> a -> a) -> a -> c k v -> a
  -- elems :: c k v -> [v]
  -- keys :: c k v -> [k]

-- list

-- a key value list with no duplicate keys
newtype KeyValueList k v = KeyValueList [(k, v)]
  deriving (Eq, Show)

fromList :: Eq k => [(k, v)] -> KeyValueList k v
fromList = KeyValueList . L.nubBy (\kv1 kv2 -> fst kv1 == fst kv2)

insertKVL :: Eq k => (k, v) -> KeyValueList k v -> KeyValueList k v
insertKVL (k, v) (KeyValueList m) = KeyValueList $ (k, v) : removeKey k m
  where
    removeKey _ []       = []
    removeKey k' (kv:m') | k' == fst kv = m'
                         | otherwise    = kv:removeKey k' m'

instance Eq k => KeyValue KeyValueList k v where
  empty = KeyValueList []
  insert k v m = insertKVL (k, v) m
  lookup k (KeyValueList m) = L.lookup k m

-- lazy maps

newtype LazyMap k v = LazyMap (M.Map k v)
  deriving (Eq, Show)

instance Ord k => KeyValue LazyMap k v where
  empty = LazyMap ML.empty
  insert k v (LazyMap m) = LazyMap $ ML.insert k v m
  lookup k (LazyMap m) = ML.lookup k m

-- strict maps

newtype StrictMap k v = StrictMap (M.Map k v)
  deriving (Eq, Show)

instance Ord k => KeyValue StrictMap k v where
  empty = StrictMap MS.empty
  insert k v (StrictMap m) = StrictMap $ MS.insert k v m
  lookup k (StrictMap m) = MS.lookup k m

-- lazy hashmaps

newtype LazyHash k v = LazyHash (H.HashMap k v)
  deriving (Eq, Show)

instance (Eq k, Hashable k) => KeyValue LazyHash k v where
  empty = LazyHash HL.empty
  insert k v (LazyHash m) = LazyHash $ HL.insert k v m
  lookup k (LazyHash m) = HL.lookup k m

-- strict hashmaps

newtype StrictHash k v = StrictHash (H.HashMap k v)
  deriving (Eq, Show)

instance (Eq k, Hashable k) => KeyValue StrictHash k v where
  empty = StrictHash HS.empty
  insert k v (StrictHash m) = StrictHash $ HS.insert k v m
  lookup k (StrictHash m) = HS.lookup k m
