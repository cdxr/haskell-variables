module Control.Concurrent.State where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State

import Control.Concurrent.STM

import Data.Tuple ( swap )

import Control.Concurrent.MVar


runStateMVar :: MVar s -> StateT s IO a -> IO a
runStateMVar v m = modifyMVar v $ liftM swap . runStateT m


runStateTMVar :: TMVar s -> StateT s STM a -> STM a
runStateTMVar v m = do
    (a, s) <- runStateT m =<< takeTMVar v
    a <$ putTMVar v s
