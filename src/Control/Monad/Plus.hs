
module Control.Monad.Plus (
        MonadPlus(..),
        msum,
        mfilter,
        mremove,
        mpartition,
        mfromMaybe,
        mcatMaybes,
        mmapMaybe,
  ) where

import Control.Monad

-- | 
-- Generalizes the 'listToMaybe' function.
-- 
mfromList :: MonadPlus m => [a] -> m a
mfromList = msum . map return

-- | 
-- Generalizes the 'remove' function.
-- 
mremove :: MonadPlus m => (a -> Bool) -> m a -> m a
mremove p = mfilter (not . p)

-- | 
-- Generalizes the 'partition' function.
-- 
mpartition :: MonadPlus m => (a -> Bool) -> m a -> (m a, m a)
mpartition p a = (mfilter p a, mremove p a)

-- | 
-- Pass through @Just@ occurrences.
-- Generalizes the 'catMaybes' function.
-- 
mcatMaybes :: MonadPlus m => m (Maybe a) -> m a
mcatMaybes = (>>= maybe mzero return)

{-
    mcatMaybes a = 
    a >>= maybe mzero return
-}

-- | 
-- Translate maybe to an arbitrary 'MonadPlus' type.
-- Generalizes the 'maybeToList' function.
-- 
mfromMaybe :: MonadPlus m => Maybe a -> m a
mfromMaybe = maybe mzero return

-- | 
-- Modify or discard a value.
-- Generalizes the 'mapMaybe' function.
-- 
mmapMaybe :: MonadPlus m => (a -> Maybe b) -> m a -> m b
mmapMaybe f = mcatMaybes . liftM f

mlefts :: MonadPlus m => m (Either a b) -> m a
mlefts = mcatMaybes . liftM l
    where
        l (Left a)  = Just a
        l (Right a) = Nothing

mrights :: MonadPlus m => m (Either a b) -> m b
mrights = mcatMaybes . liftM r
    where
        r (Left a)  = Nothing
        r (Right a) = Just a

mpartitionEithers :: MonadPlus m => m (Either a b) -> (m a, m b)
mpartitionEithers a = (mlefts a, mrights a)


