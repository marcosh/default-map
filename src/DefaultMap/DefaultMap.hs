{-# LANGUAGE GADTs #-}

module DefaultMap.DefaultMap where

import qualified DefaultMap.KeyValue as KV
import DefaultMap.KeyValue ( KeyValue )

-- base
import Data.Maybe (fromMaybe)

-- data type

data DefaultMap c k v where
  DefaultMap :: KeyValue c k v => c k v -> v -> DefaultMap c k v

instance (Eq (c k v), Eq v) => Eq (DefaultMap c k v) where
  (DefaultMap m1 d1) == (DefaultMap m2 d2) = m1 == m2 && d1 == d2

instance (Show (c k v), Show v) => Show (DefaultMap c k v) where
  show (DefaultMap m d) = "DefaultMap " ++ show m ++ " " ++ show d

-- construction

constant :: KeyValue c k v => v -> DefaultMap c k v
constant = DefaultMap KV.empty

-- insertion

insert :: KeyValue c k v => k -> v -> DefaultMap c k v -> DefaultMap c k v
insert k v (DefaultMap m d) = DefaultMap (KV.insert k v m) d

-- query

lookup :: KeyValue c k v => k -> DefaultMap c k v -> v
lookup k (DefaultMap m d) = fromMaybe d $ KV.lookup k m

-- -- base
-- import Data.Data (Data)
-- import Data.Maybe (fromMaybe)
-- import GHC.Generics (Generic)

-- -- containers
-- import qualified Data.Map.Lazy as M (Map, insert, lookup, mapKeys, empty)

-- -- deepseq
-- import Control.DeepSeq (NFData)

-- -- data type

-- data DefaultMap a b = DefaultMap (M.Map a b) b
--   deriving stock (Data, Eq, Functor, Generic, Read, Show)
--   deriving anyclass (NFData)
--   -- Semigroup, Monoid

-- -- construction

-- constant :: b -> DefaultMap a b
-- constant = DefaultMap M.empty

-- -- insertion

-- insert :: Ord a => a -> b -> DefaultMap a b -> DefaultMap a b
-- insert a b (DefaultMap m d) = DefaultMap (M.insert a b m) d

-- -- insertWith :: Ord a => (a -> a -> a) -> a -> b -> DefaultMap a b -> DefaultMap a b
-- -- insertWith f a b (DefaultMap m d) = _asdf



-- apply :: Ord a => DefaultMap a b -> a -> b
-- apply (DefaultMap m d) a = fromMaybe d (M.lookup a m)

-- mapKeys :: Ord b => (a -> b) -> DefaultMap a c -> DefaultMap b c
-- mapKeys f (DefaultMap m d) = DefaultMap (M.mapKeys f m) d

-- fold1 :: Semigroup b => DefaultMap a b -> b
-- fold1 (DefaultMap m d) = foldr (<>) d m
