
-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Partial maps and filters over 'MonadPlus' instances.
--
-- This is especially useful for sequential structures such as event lists, tracks etc.
--
-- Inspired by the following blog post:
--
--    * <http://conal.net/blog/posts/a-handy-generalized-filter>
--
-------------------------------------------------------------------------------------

module Control.Monad.Plus (
        -- * Basics
        module Control.Monad,
        msum,
        msum',      
        
        -- * Constructing
        mfold,
        mfromList,
        mfromMaybe,

        -- * Filtering
        mfilter',
        mpartition,

        -- ** Special filters
        mscatter,
        mscatter',
        mcatMaybes,
        mlefts,
        mrights,
        mpartitionEithers,

        -- * Special maps
        mmapMaybe,
        mconcatMap,
  ) where

import Control.Monad
import Data.List (partition)
import Data.Maybe (listToMaybe, maybeToList, catMaybes, mapMaybe)
import Data.Either (lefts, rights, partitionEithers)
import Data.Foldable (Foldable(..), toList)
import qualified Data.Foldable as Foldable

-- |
-- This generalizes the list-based 'concat' function. 
-- 
msum' :: (MonadPlus m, Foldable t) => t (m a) -> m a
msum' = Foldable.msum

-- | 
-- Fold a value into an arbitrary 'MonadPlus' type.
-- 
-- This function generalizes the 'toList' function.
-- 
mfold :: (MonadPlus m, Foldable t) => t a -> m a
mfold = mfromList . Foldable.toList

-- | 
-- Translate a list to an arbitrary 'MonadPlus' type.
--
-- This function generalizes the 'listToMaybe' function.
-- 
mfromList :: MonadPlus m => [a] -> m a
mfromList = msum . map return

-- | 
-- Translate maybe to an arbitrary 'MonadPlus' type.
-- 
-- This function generalizes the 'maybeToList' function.
-- 
mfromMaybe :: MonadPlus m => Maybe a -> m a
mfromMaybe = maybe mzero return

-- | 
-- 'mfilter'', applied to a predicate and a container, returns the container of
-- those elements that satisfy the predicate; i.e.,
--
-- > filter p xs = [ x | x <- xs, p x]
--
-- This function generalizes the 'filter' function.
-- (Identical to 'mfilter', it is just here for documentation purposes).
-- 
mfilter' :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter' = mfilter

-- | 
-- The 'partition' function takes a predicate a list and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p xs == (filter p xs, filter (not . p) xs)
--
-- This function generalizes the 'partition' function.
-- 
mpartition :: MonadPlus m => (a -> Bool) -> m a -> (m a, m a)
mpartition p a = (mfilter p a, mfilter (not . p) a)

-- | 
-- Pass through @Just@ occurrences.
-- 
-- This function generalizes the 'catMaybes' function.
-- 
mcatMaybes :: MonadPlus m => m (Maybe a) -> m a
mcatMaybes = (>>= mfromMaybe)

-- | 
-- Pass through @Just@ occurrences.
-- 
-- This function generalizes the 'catMaybes' function.
-- 
mscatter :: MonadPlus m => m [b] -> m b
mscatter = (>>= mfromList)

-- | 
-- Pass through @Just@ occurrences.
-- 
-- This function generalizes the 'catMaybes' function.
-- 
mscatter' :: (MonadPlus m, Foldable t) => m (t b) -> m b
mscatter' = (>>= mfromList . toList)

-- | 
-- Pass through @Left@ occurrences.
-- 
-- This function generalizes the 'lefts' function.
-- 
mlefts :: MonadPlus m => m (Either a b) -> m a
mlefts = mcatMaybes . liftM l
    where
        l (Left a)  = Just a
        l (Right a) = Nothing

-- | 
-- Pass through @Right@ occurrences.
-- 
-- This function generalizes the 'rights' function.
-- 
mrights :: MonadPlus m => m (Either a b) -> m b
mrights = mcatMaybes . liftM r
    where
        r (Left a)  = Nothing
        r (Right a) = Just a

-- | 
-- Separate @Left@ and @Right@ occurances.
-- 
-- This function generalizes the 'partitionEithers' function.
-- 
mpartitionEithers :: MonadPlus m => m (Either a b) -> (m a, m b)
mpartitionEithers a = (mlefts a, mrights a)




-- | 
-- Modify or discard a value.
-- 
-- This function generalizes the 'mapMaybe' function.
-- 
mmapMaybe :: MonadPlus m => (a -> Maybe b) -> m a -> m b
mmapMaybe f = mcatMaybes . liftM f

-- | 
-- Modify and return a number of values.
-- 
-- This function generalizes the 'concatMap' function.
-- 
mconcatMap :: MonadPlus m => (a -> [b]) -> m a -> m b
mconcatMap f = mscatter . liftM f

{-
mmapLefts :: MonadPlus m => (a -> Either b c) -> m a -> m b
mmapLefts f = mlefts . liftM f

mmapRights :: MonadPlus m => (a -> Either c b) -> m a -> m b
mmapRights f = mrights . liftM f
-}


