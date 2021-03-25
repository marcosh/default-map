{-# LANGUAGE DeriveFunctor #-}

module AlmostConstantFunctions.AlmostConstantFunctions where

-- base
import Data.Maybe (fromMaybe)

-- containers
import qualified Data.Map.Lazy as M (Map, lookup, mapKeys)

data ACF a b = ACF (M.Map a b) b
  deriving (Eq, Show, Functor)

apply :: Ord a => ACF a b -> a -> b
apply (ACF m d) a = fromMaybe d (M.lookup a m)

mapKeys :: Ord b => (a -> b) -> ACF a c -> ACF b c
mapKeys f (ACF m d) = ACF (M.mapKeys f m) d

fold1 :: Semigroup b => ACF a b -> b
fold1 (ACF m d) = foldr (<>) d m
