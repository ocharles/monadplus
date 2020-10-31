
{-# LANGUAGE DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

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
-- Partial maps and filters over 'MonadPlus' instances. The basic idea here is that
-- the monad interface together with the monoidal structure of 'MonadPlus' is enough
-- to implement partial maps and filters (i.e. 'mmapMaybe' and 'mfilter').
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
        Monad.msum,
        msum',      
        
        -- * Constructing
        mfold,
        mfromList,
        mfromMaybe,
        mreturn,

        -- * Filtering
        -- mfilter,
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
        mconcatMap',
        
        -- * Utility
        Partial(..),
        partial,
        predicate,
        always,
        never,
  ) where

import Control.Monad hiding (msum)
import Control.Applicative                   
import Control.Category (Category)
import qualified Control.Category as Category
import Data.Semigroup as Sem
import Data.Monoid
import Data.List (partition)
import Data.Maybe (listToMaybe, maybeToList, catMaybes, mapMaybe, fromMaybe)
import Data.Either (lefts, rights, partitionEithers)
import Data.Foldable (Foldable(..), toList)
import qualified Control.Monad as Monad
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
mfromList = Monad.msum . map return

-- | 
-- Translate maybe to an arbitrary 'MonadPlus' type.
-- 
-- This function generalizes the 'maybeToList' function.
-- 
mfromMaybe :: MonadPlus m => Maybe a -> m a
mfromMaybe = maybe mzero return

-- | 
-- Convert a partial function to a function returning an arbitrary
-- 'MonadPlus' type.
-- 
mreturn :: MonadPlus m => (a -> Maybe b) -> a -> m b
mreturn f = mfromMaybe . f

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
-- Pass through @Just@ elements.
-- 
-- This function generalizes the 'catMaybes' function.
-- 
mcatMaybes :: MonadPlus m => m (Maybe a) -> m a
mcatMaybes = (>>= mfromMaybe)

-- | 
-- Join list elements together.
-- 
-- This function generalizes the 'catMaybes' function.
-- 
mscatter :: MonadPlus m => m [b] -> m b
mscatter = (>>= mfromList)

-- | 
-- Join foldable elements together.
-- 
-- This function generalizes the 'catMaybes' function.
-- 
mscatter' :: (MonadPlus m, Foldable t) => m (t b) -> m b
mscatter' = (>>= mfold)

-- | 
-- Pass through @Left@ elements.
-- 
-- This function generalizes the 'lefts' function.
-- 
mlefts :: MonadPlus m => m (Either a b) -> m a
mlefts = mcatMaybes . liftM l
    where
        l (Left a)  = Just a
        l (Right a) = Nothing

-- | 
-- Pass through @Right@ elements.
-- 
-- This function generalizes the 'rights' function.
-- 
mrights :: MonadPlus m => m (Either a b) -> m b
mrights = mcatMaybes . liftM r
    where
        r (Left a)  = Nothing
        r (Right a) = Just a

-- | 
-- Separate @Left@ and @Right@ elements.
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
-- Modify, discard or spawn values.
-- 
-- This function generalizes the 'concatMap' function.
-- 
mconcatMap :: MonadPlus m => (a -> [b]) -> m a -> m b
mconcatMap f = mscatter . liftM f

-- | 
-- Modify, discard or spawn values.
-- 
-- This function generalizes the 'concatMap' function.
-- 
mconcatMap' :: (MonadPlus m, Foldable t) => (a -> t b) -> m a -> m b
mconcatMap' f = mscatter' . liftM f



-- |
-- Convert a predicate to a partial function.
--
partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

-- |
-- Convert a partial function to a predicate.
--
predicate :: (a -> Maybe a) -> a -> Bool
predicate f x = case f x of
    Just _  -> True
    Nothing -> False

-- |
-- Convert a total function to a partial function.
--  
always :: (a -> b) -> a -> Maybe b
always f = Just . f

-- |
-- Make a partial function that always rejects its input.
--  
never :: a -> Maybe c
never = const Nothing

-- |
-- Wrapper for partial functions with 'MonadPlus' instance.
--
newtype Partial a b = Partial { getPartial :: a -> Maybe b }
    
instance Functor (Partial r) where
    fmap f (Partial g) = Partial (fmap f . g)

instance Monad (Partial r) where
    return x = Partial (\_ -> Just x)
    Partial f >>= k = Partial $ \r -> do { x <- f r; getPartial (k x) r }
    -- f >>= k = (join' . fmap k) f
        -- where
            -- join' g = Partial $ \x -> do { h <- getPartial g x; getPartial h x }

instance MonadPlus (Partial r) where
    mzero = Partial (const Nothing)
    Partial f `mplus` Partial g = Partial $ \x -> f x `mplus` g x

instance Applicative (Partial r) where
    pure x = Partial (\_ -> Just x)
    Partial f <*> Partial g = Partial $ \x -> f x <*> g x

instance Alternative (Partial r) where
    empty = Partial (const Nothing)
    Partial f <|> Partial g = Partial $ \x -> f x <|> g x

instance Category Partial where
    id  = Partial (always id)
    Partial f . Partial g = Partial $ \x -> do
        y <- g x
        f y

instance Sem.Semigroup (Partial a b) where
    (<>) = mplus

instance Monoid (Partial a b) where
    mempty  = mzero
    mappend = (<>)


