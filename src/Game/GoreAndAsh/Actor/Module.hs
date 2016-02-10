{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.Actor.Module
Description : Monad transformer for actor core module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains monad transformer for actor core module and 
instance of 'GameModule' for it.
-}
module Game.GoreAndAsh.Actor.Module(
    ActorT(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict
import Data.Proxy 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor.State

-- | Monad transformer of actor core module.
--
-- [@s@] - State of next core module in modules chain;
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
-- 
-- @
-- type AppStack = ModuleStack [ActorT, ... other modules ... ] IO
--
-- newtype AppMonad a = AppMonad (AppStack a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, ActorMonad)
-- @
--
-- The module is pure within first phase (see 'ModuleStack' docs) but requires 'MonadThrow'
-- instance of end monad, therefore currently only 'IO' end monad can handler the module.
newtype ActorT s m a = ActorT { runActorT :: StateT (ActorState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (ActorState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance GameModule m s => GameModule (ActorT s m) (ActorState s) where 
  type ModuleState (ActorT s m) = ActorState s
  runModule (ActorT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (actorNextState s)
    let s'' = moveSendedMessages s'
    return (a, s'' { 
       actorNextState = nextState 
      })

  newModuleState = emptyActorState <$> newModuleState

  withModule _ = withModule (Proxy :: Proxy m)
  cleanupModule _ = return ()