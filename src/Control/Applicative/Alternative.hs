
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
-- Partial maps and filters over 'Alternative' instances.
--
-- This is considerably weaker than 'MonadPlus', as we have no possibility of removing
-- intermediate structure, as in 'mcatMaybes'.
--
-------------------------------------------------------------------------------------

module Control.Applicative.Alternative (
        -- * Basics
        module Control.Applicative,
        asum,
        asum',      
        
        -- * Constructing
        afold,
        afromList,
        afromMaybe,

        -- -- * Filtering
        -- afilter',
        -- apartition,
        -- 
        -- -- ** Special filters
        -- ascatter,
        -- ascatter',
        -- acatMaybes,
        -- alefts,
        -- arights,
        -- apartitionEithers,
        -- 
        -- -- * Special maps
        -- amapMaybe,
        -- aconcatMap,
  ) where

import Control.Applicative
import Data.Foldable (Foldable(..), toList, asum)
import qualified Data.Foldable as Foldable

-- |
-- This generalizes the list-based 'concat' function. 
-- 
asum' :: (Alternative f, Foldable t) => t (f a) -> f a
asum' = Foldable.asum

-- | 
-- Fold a value into an arbitrary 'MonadPlus' type.
-- 
-- This function generalizes the 'toList' function.
-- 
afold :: (Alternative f, Foldable t) => t a -> f a
afold = afromList . Foldable.toList

-- | 
-- This function generalizes the 'listToMaybe' function.
-- 
afromList :: Alternative f => [a] -> f a
afromList = asum . map pure

-- | 
-- Translate maybe to an arbitrary 'Alternative' type.
-- 
-- This function generalizes the 'maybeToList' function.
-- 
afromMaybe :: Alternative f => Maybe a -> f a
afromMaybe = maybe empty pure 
