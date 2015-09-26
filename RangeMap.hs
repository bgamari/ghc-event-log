{-# LANGUAGE ScopedTypeVariables #-}

module RangeMap
    ( -- * Ranges
      Range(..)
    , rangeSize
      -- * Range map
    , RangeMap
      -- * Construction
    , singleton
    , insert
      -- * Destruction
    , fromList
    , assocs
    , values
      -- * Extracting subsets
    , containing
    , contains
    , startsAbove
    , startsBelow
    , endsAbove
    , endsBelow
    ) where

import Data.Monoid
import Data.Foldable
import qualified Data.Map as M
import Prelude hiding (lookup)

-- | A range consisting of an inclusive lower and upper bound
data Range a = Rng !a !a
             deriving (Show, Read, Eq, Ord)

rangeSize :: Num a => Range a -> a
rangeSize (Rng a b) = b - a

-- | A map from ranges of keys @Range k@ to values @v@ supporting efficient range queries.
newtype RangeMap k v = RngMap (M.Map k (M.Map k v))

instance (Ord k, Show k, Show v) => Show (RangeMap k v) where
  showsPrec prec m =
    showString "RangeMap.fromList " . showsPrec prec (assocs m)

instance Functor (RangeMap k) where
  fmap f (RngMap m) = RngMap $ fmap (fmap f) m
  {-# INLINE fmap #-}

instance Foldable (RangeMap k) where
  foldMap f (RngMap m) = foldMap (foldMap f) m

instance Ord k => Monoid (RangeMap k v) where
  mempty = RngMap mempty
  {-# INLINE mempty #-}
  RngMap a `mappend` RngMap b = RngMap $ M.unionWith (<>) a b
  {-# INLINE mappend #-}

-- | Build a 'RangeMap' from a list
fromList :: Ord k => [(Range k, v)] -> RangeMap k v
fromList = foldMap (uncurry singleton)

-- | A singleton 'RangeMap'
singleton :: Range k -> v -> RangeMap k v
singleton (Rng a b) v = RngMap $ M.singleton a $ M.singleton b v
{-# INLINE singleton #-}

-- | Insert a range into a 'RangeMap'
insert :: Ord k => Range k -> v -> RangeMap k v -> RangeMap k v
insert (Rng a b) v (RngMap m) = RngMap $ M.insertWith (<>) a (M.singleton b v) m
{-# INLINE insert #-}

-- | The subset of the map containing the given range (inclusive on both ends)
containing :: Ord k => Range k -> RangeMap k v -> RangeMap k v
containing (Rng lower upper) = endsAbove upper . startsBelow lower
{-# INLINE containing #-}

-- | The subset of the map contained by the given range (inclusive on both ends)
contains :: Ord k => Range k -> RangeMap k v -> RangeMap k v
contains (Rng lower upper) = startsAbove lower . endsBelow upper
{-# INLINE contains #-}

-- | Return the subset of the map whose ranges begin above (or equal to) the given key
startsAbove :: Ord k => k -> RangeMap k v -> RangeMap k v
startsAbove k (RngMap m) = RngMap $ snd $ splitE k m
{-# INLINE startsAbove #-}

-- | Return the subset of the map whose ranges end above (or equal to) the given key
endsAbove :: Ord k => k -> RangeMap k v -> RangeMap k v
endsAbove k (RngMap m) = RngMap $ fmap (snd . splitE k) m
{-# INLINE endsAbove #-}

-- | Return the subset of the map whose ranges begin below (or equal to) the given key
startsBelow :: Ord k => k -> RangeMap k v -> RangeMap k v
startsBelow k (RngMap m) = RngMap $ fst $ splitE k m
{-# INLINE startsBelow #-}

-- | Return the subset of the map whose ranges end below (or equal to) given key
endsBelow :: Ord k => k -> RangeMap k v -> RangeMap k v
endsBelow k (RngMap m) = RngMap $ fmap (fst . splitE k) m
{-# INLINE endsBelow #-}

-- | Split a map at the given key. Both the left and right halves will contain the key.
splitE :: Ord k => k -> M.Map k v -> (M.Map k v, M.Map k v)
splitE k m = (insertX l, insertX r)
  where
    (l, x, r) = M.splitLookup k m
    insertX
      | Just v <- x = M.insert k v
      | otherwise   = id
{-# INLINE splitE #-}

assocs :: RangeMap k v -> [(Range k, v)]
assocs (RngMap m) =
  concatMap (\(a,m') -> map (\(b,v) -> (Rng a b, v)) $ M.assocs m') (M.assocs m)
{-# INLINE assocs #-}

values :: RangeMap k v -> [v]
values = map snd . assocs
{-# INLINE values #-}
