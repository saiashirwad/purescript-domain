module Domain.Runtime.Projection
  ( replay
  , dedupe
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Domain (Envelope, Projection)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref

-- | Feed a committed history to a projection. The caller decides whether the
-- | history is a live delivery or a rebuild from the log.
replay
  :: forall m id e
   . Monad m
  => Projection m id e
  -> Array (Envelope id e)
  -> m Unit
replay projection = traverse_ projection.on

-- | Wrap a projection with a sequence watermark per source stream. A repeated
-- | delivery of an event that has already been applied is ignored. The wrapper
-- | assumes one consumer per projection, which is the normal projection model.
dedupe
  :: forall m id e
   . MonadEffect m
  => Projection m id e
  -> m (Projection m id e)
dedupe projection = do
  applied <- liftEffect (Ref.new Map.empty)
  pure
    { on: \envelope -> do
        seen <- liftEffect (Ref.read applied)
        case Map.lookup envelope.stream seen of
          Just sequence | envelope.seq <= sequence -> pure unit
          _ -> do
            projection.on envelope
            liftEffect
              ( Ref.modify_
                  (Map.insert envelope.stream envelope.seq)
                  applied
              )
    }
