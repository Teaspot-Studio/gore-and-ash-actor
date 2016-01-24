{-|
Module      : Game.GoreAndAsh.Actor
Description : Module that contains actor API for Gore&Ash
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The core module contains API for actor based aproach of game development. 
The module doesn't depends on others core modules and could be place in any place in 
game monad stack.

The module is pure within first phase (see 'ModuleStack' docs) but requires 'MonadThrow'
instance of end monad, therefore currently only 'IO' end monad can handler the module.

Example of embedding:

@
-- | Application monad is monad stack build from given list of modules over base monad (IO)
type AppStack = ModuleStack [ActorT, ... other modules ... ] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState 

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, ActorMonad, ... other modules monads ... )
  
instance GameModule AppMonad AppState where 
  type ModuleState AppMonad = AppState
  runModule (AppMonad m) (AppState s) = do 
    (a, s') <- runModule m s 
    return (a, AppState s')
  newModuleState = AppState <$> newModuleState
  withModule _ = withModule (Proxy :: Proxy AppStack)
  cleanupModule (AppState s) = cleanupModule s 

-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b
-- | Action that makes indexed app wire
type AppActor i a b = GameActor AppMonad i a b
@

Actor ('GameActor') is wire with its unique id. For instance you want actor for your player:

@
data Player = Player {
  playerId :: !PlayerId
, playerPos :: !(Double, Double)
} deriving (Generic)

instance NFData Player 

newtype PlayerId = PlayerId { unPlayerId :: Int } deriving (Eq, Show, Generic) 
instance NFData PlayerId 
instance Hashable PlayerId 
instance Serialize PlayerId

data PlayerMessage =
    -- | The player was shot by specified player
    PlayerShotMessage !PlayerId 
  deriving (Typeable, Generic)

instance NFData PlayerMessage 

instance ActorMessage PlayerId where
  type ActorMessageType PlayerId = PlayerMessage
  toCounter = unPlayerId
  fromCounter = PlayerId
@

Now you can create statefull actor:

@
playerActor :: ActorMonad m => (PlayerId -> Player) -> AppActor m PlayerId Game Player 
playerActor initialPlayer = makeActor $ \i -> stateWire (initialPlayer i) $ mainController i
  where
  mainController i = proc (g, p) -> do
    emsg <- actorMessages i isPlayerShotMessage -< ()
    -- do with emsg something
    returnA -< p
@

And you can have dynamic collection of actors:

@
processPlayers :: ActorMonad m => AppWire m Game [Player]
processPlayer = proc g -> do 
  addEvent <- periodic 4 -< newPlayer
  remEvent <- never -< ()
  dynCollection [] -< (g, addEvent, remEvent)
@
-}
module Game.GoreAndAsh.Actor(
  -- * Low level
    ActorState
  , ActorT 
  , ActorMonad(..)
  , ActorException(..)
  -- * Actor API
  , GameWireIndexed(..)
  , GameActor
  , ActorMessage(..)
  , postActorAction
  , preActorAction
  , makeActor
  , makeFixedActor
  , runActor
  , runActor'
  -- ** Helpers for libraries
  , getActorFingerprint
  , actorFingerprint
  -- * Message API
  , actorSend
  , actorSendMany
  , actorSendDyn
  , actorSendManyDyn
  , actorProcessMessages
  , actorProcessMessagesM
  , actorMessages
  -- * Dynamic collections
  , DynCollection(..)
  , ElementWithId(..)
  , dynCollection
  , dDynCollection
  ) where

import Game.GoreAndAsh.Actor.API as X
import Game.GoreAndAsh.Actor.Collection as X
import Game.GoreAndAsh.Actor.Indexed as X
import Game.GoreAndAsh.Actor.Message as X
import Game.GoreAndAsh.Actor.Module as X
import Game.GoreAndAsh.Actor.State as X