module Domain.Runtime
  ( Runtime
  , fromInMemory
  , fromActor
  ) where

import Prelude

import Domain
  ( Aggregate
  , CommandId
  , Envelope
  , Receipt
  , Stream
  )
import Data.Either (Either)
import Domain.Runtime.Actor as Actor
import Domain.Runtime.InMemory as InMemory
import Effect.Aff (Aff)

-- | The verbs the bank needs from a host. The model depends on this record,
-- | not on a concrete store, so the in-memory host and the per-key actor host
-- | are two interpreters for the same domain.
type Runtime m id s c e r b =
  { send
      :: Aggregate id s c e r b
      -> CommandId
      -> id
      -> c
      -> m (Either r (Receipt id e b))
  , load :: Aggregate id s c e r b -> id -> m (Stream id s e)
  , history :: Aggregate id s c e r b -> id -> m (Array (Envelope id e))
  , shutdown :: m Unit
  }

fromInMemory
  :: forall id s c e r b
   . InMemory.InMemory id s c e r b
  -> Runtime Aff id s c e r b
fromInMemory runtime =
  { send: InMemory.send runtime
  , load: InMemory.load runtime
  , history: InMemory.history runtime
  , shutdown: pure unit
  }

fromActor
  :: forall id s c e r b
   . Actor.Actor id s c e r b
  -> Runtime Aff id s c e r b
fromActor actor =
  { send: Actor.send actor
  , load: Actor.load actor
  , history: Actor.history actor
  , shutdown: Actor.shutdown actor
  }
