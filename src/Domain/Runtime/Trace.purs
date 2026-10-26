module Domain.Runtime.Trace
  ( collect
  , recorded
  ) where

import Prelude

import Data.Array as Array
import Domain.Trace (Span, Tracer)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- | A tracer that keeps every span in memory. Tests use it for golden traces;
-- | a production host swaps in an exporter.
collect
  :: forall m
   . MonadEffect m
  => m { tracer :: Tracer m, spans :: Ref (Array Span) }
collect = do
  spans <- liftEffect (Ref.new [])
  pure
    { tracer:
        { emit: \span ->
            liftEffect (Ref.modify_ (\seen -> Array.snoc seen span) spans)
        }
    , spans
    }

recorded :: forall m. MonadEffect m => Ref (Array Span) -> m (Array Span)
recorded = liftEffect <<< Ref.read
