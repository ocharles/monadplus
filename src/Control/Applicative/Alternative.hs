
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
        Foldable.asum,

        -- * Constructing
        afold,
        afromList,
        afromMaybe,
  ) where

import Control.Applicative
import Data.Foldable (Foldable(..))

import qualified Data.Foldable as Foldable

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
afromList = Foldable.asum . map pure

-- |
-- Translate maybe to an arbitrary 'Alternative' type.
--
-- This function generalizes the 'maybeToList' function.
--
afromMaybe :: Alternative f => Maybe a -> f a
afromMaybe = maybe empty pure
