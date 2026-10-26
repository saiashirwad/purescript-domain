module Domain.Trace
  ( Span
  , Tracer
  , noTrace
  , attribute
  ) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.Map as Map
import Data.Time.Duration (Milliseconds)

-- | A single timed operation. `attributes` is deliberately stringly keyed:
-- | traces cross a process boundary, and a string map is what every backend
-- | accepts. Domain types stay typed on the inside.
type Span =
  { name :: String
  , startedAt :: Instant
  , duration :: Milliseconds
  , attributes :: Map String String
  }

type Tracer m =
  { emit :: Span -> m Unit
  }

noTrace :: forall m. Applicative m => Tracer m
noTrace = { emit: \_ -> pure unit }

attribute :: String -> String -> Map String String -> Map String String
attribute key value = Map.insert key value
