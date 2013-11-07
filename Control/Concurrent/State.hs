{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Concurrent.State where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.State

import Data.Tuple ( swap )

import Data.IORef
import Data.STRef

import qualified Data.StateVar as StateVar
import Data.StateVar hiding ( get )

import Control.Concurrent.MVar
import Control.Concurrent.STM



class (Monad m) => Variable m v | v -> m where
    writeVar :: v s -> s -> m ()
    writeVar v s = modifyVar v $ const ((), s)

    readVar :: v s -> m s
    readVar v = modifyVar v $ \s -> (s, s)

    modifyVar  :: v s -> (s -> (a, s)) -> m a
    modifyVar v f = do
        (a, s) <- f `liftM` readVar v
        writeVar v s
        return a

    modifyVar' :: v s -> (s -> (a, s)) -> m a
    modifyVar' v f = modifyVar v $ \s0 ->
        let (a, s) = f s0
        in s `seq` (a, s)


class (Variable m v) => Locked m v | v -> m where
    atomicVar  :: v s -> (s -> m (a, s)) -> m a
    atomicVar v f = do
        (a, s) <- f =<< readVar v
        writeVar v s
        return a

    atomicVar' :: v s -> (s -> m (a, s)) -> m a
    atomicVar' v f = atomicVar v $ \s0 -> do
        (a, s) <- f s0
        return $! seq s (a, s)


modifyVarDefault :: (Locked m v) => v s -> (s -> (a, s)) -> m a
modifyVarDefault v f = atomicVar v (return . f)


-- | defined with atomicModifyIORef
instance Variable IO IORef where
    modifyVar v f = atomicModifyIORef v $ swap . f

instance Variable IO StateVar where
    readVar = StateVar.get
    writeVar = (StateVar.$=)


instance Variable IO MVar where
    modifyVar = modifyVarDefault

instance Locked IO MVar where
    atomicVar v f = modifyMVar v $ liftM swap . f


instance Variable (ST s) (STRef s) where
    readVar = readSTRef
    writeVar = writeSTRef

-- | trivially atomic due to ST
instance Locked (ST s) (STRef s) where


instance Variable STM TVar where
    readVar = readTVar
    writeVar = writeTVar

-- | trivially atomic due to STM
instance Locked STM TVar where


instance Variable STM TMVar where
    readVar = takeTMVar
    writeVar = putTMVar

-- | trivially atomic due to STM
instance Locked STM TMVar where


varState :: (Variable m v) => v s -> State  s   a -> m a
varState v = modifyVar v . runState

varState' :: (Variable m v) => v s -> State  s   a -> m a
varState' v = modifyVar' v . runState

varStateT :: (Locked m v) => v s -> StateT s m a -> m a
varStateT v = atomicVar v . runStateT


(@:) :: (Variable m v) => v s -> State s a -> m a
(@:) = varState
infix 2 @:

(@!) :: (Variable m v) => v s -> State s a -> m a
(@!) = varState'
infix 2 @!

(@@) :: (Locked m v) => v s -> StateT s m a -> m a
(@@) = varStateT
infix 2 @@
