{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Control.Eff.Extra.Fold
  ( foldEffM
  ) where

import           Data.OpenUnion     (Union)
import           Control.Eff        (Eff)
import qualified Control.Eff.Extend as EE

-- | Handle all effects at once. Requires poking at Eff's internals.
-- | Probably has quadratic performance, but whatever.
foldEffM :: Monad m => (forall a. Union r a -> m a) -> Eff r b -> m b
foldEffM handleAction = \case
  EE.Val x -> pure x
  EE.E action cont ->
    handleAction action >>= \x -> foldEffM handleAction (EE.qApp cont x)
