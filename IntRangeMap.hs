{-# LANGUAGE ScopedTypeVariables #-}

module IntRangeMap
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
import qualified Data.IntMap as M
import Prelude hiding (lookup)

-- | A range consisting of an inclusive lower and upper bound
data Range = Rng {-# UNPACK #-} !Int {-# UNPACK #-} !Int
           deriving (Show, Read, Eq, Ord)

rangeSize :: Range -> Int
rangeSize (Rng a b) = b - a

-- | A map from ranges of keys @Range k@ to values @v@ supporting efficient range queries.
newtype RangeMap v = RngMap (M.IntMap (M.IntMap v))

instance (Show v) => Show (RangeMap v) where
  showsPrec prec m =
    showString "RangeMap.fromList " . showsPrec prec (assocs m)

instance Functor RangeMap where
  fmap f (RngMap m) = RngMap $ fmap (fmap f) m
  {-# INLINE fmap #-}

instance Foldable RangeMap where
  foldMap f (RngMap m) = foldMap (foldMap f) m

instance Monoid (RangeMap v) where
  mempty = RngMap mempty
  {-# INLINE mempty #-}
  RngMap a `mappend` RngMap b = RngMap $ M.unionWith (<>) a b
  {-# INLINE mappend #-}

-- | Build a 'RangeMap' from a list
fromList :: [(Range, v)] -> RangeMap v
fromList = foldMap (uncurry singleton)

-- | A singleton 'RangeMap'
singleton :: Range -> v -> RangeMap v
singleton (Rng a b) v = RngMap $ M.singleton a $ M.singleton b v
{-# INLINE singleton #-}

-- | Insert a range into a 'RangeMap'
insert :: Range -> v -> RangeMap v -> RangeMap v
insert (Rng a b) v (RngMap m) = RngMap $ M.insertWith (<>) a (M.singleton b v) m
{-# INLINE insert #-}

-- | The subset of the map containing the given range (inclusive on both ends)
containing :: Range -> RangeMap v -> RangeMap v
containing (Rng lower upper) = endsAbove upper . startsBelow lower
{-# INLINE containing #-}

-- | The subset of the map contained by the given range (inclusive on both ends)
contains :: Range -> RangeMap v -> RangeMap v
contains (Rng lower upper) = startsAbove lower . endsBelow upper
{-# INLINE contains #-}

-- | Return the subset of the map whose ranges begin above (or equal to) the given key
startsAbove :: Int -> RangeMap v -> RangeMap v
startsAbove k (RngMap m) = RngMap $ snd $ splitE k m
{-# INLINE startsAbove #-}

-- | Return the subset of the map whose ranges end above (or equal to) the given key
endsAbove :: Int -> RangeMap v -> RangeMap v
endsAbove k (RngMap m) = RngMap $ fmap (snd . splitE k) m
{-# INLINE endsAbove #-}

-- | Return the subset of the map whose ranges begin below (or equal to) the given key
startsBelow :: Int -> RangeMap v -> RangeMap v
startsBelow k (RngMap m) = RngMap $ fst $ splitE k m
{-# INLINE startsBelow #-}

-- | Return the subset of the map whose ranges end below (or equal to) given key
endsBelow :: Int -> RangeMap v -> RangeMap v
endsBelow k (RngMap m) = RngMap $ fmap (fst . splitE k) m
{-# INLINE endsBelow #-}

-- | Split a map at the given key. Both the left and right halves will contain the key.
splitE :: Int -> M.IntMap v -> (M.IntMap v, M.IntMap v)
splitE k m = (insertX l, insertX r)
  where
    (l, x, r) = M.splitLookup k m
    insertX
      | Just v <- x = M.insert k v
      | otherwise   = id
{-# INLINE splitE #-}

assocs :: RangeMap v -> [(Range, v)]
assocs (RngMap m) =
  concatMap (\(a,m') -> map (\(b,v) -> (Rng a b, v)) $ M.assocs m') (M.assocs m)
{-# INLINE assocs #-}

values :: RangeMap v -> [v]
values = map snd . assocs
{-# INLINE values #-}
