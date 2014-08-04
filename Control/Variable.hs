{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Variable where

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


-- | The class @Variable m v@ defines operations for reading and writing to
-- a variable of type @v@ in the monad @m@.
class (Monad m) => Variable m v | v -> m where
    -- | @writeVar v s@ stores the value @s@ in @v@.
    writeVar :: v s -> s -> m ()
    writeVar v s = modifyVar v $ const ((), s)

    -- | @readVar v@ retrieves the value stored in @v@.
    readVar :: v s -> m s
    readVar v = modifyVar v $ \s -> (s, s)

    -- | @modifyVar v f@ applies @f@ to the value stored in @v@, storing
    -- the new value in @v@.
    modifyVar  :: v s -> (s -> (a, s)) -> m a
    modifyVar v f = do
        (a, s) <- f `liftM` readVar v
        writeVar v s
        return a

    -- | A version of `modifyVar` that applies the function strictly.
    modifyVar' :: v s -> (s -> (a, s)) -> m a
    modifyVar' v f = modifyVar v $ \s0 ->
        let (a, s) = f s0
        in s `seq` (a, s)

    {-# MINIMAL writeVar, readVar | modifyVar #-}


-- | The class @Locked m v@ is defined for instances of @Variable m v@ that
-- admit a proper definition of @atomicVar@. An instance of @Locked IO v@
-- is only correct if @v@ implements proper locking behavior, e.g. @MVar@.
--
-- This class is trivially satisfied by any instance @Variable STM v@ or
-- @Variable ST v@.
--
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

-- | A default definition of @modifyVar@ for instances of @Locked m v@.
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


-- | @varState v m@ runs @m@ over the value stored in @v@, storing the resulting
-- value in @v@.
varState :: (Variable m v) => v s -> State  s   a -> m a
varState v = modifyVar v . runState

-- | A strict version of `varState`.
varState' :: (Variable m v) => v s -> State  s   a -> m a
varState' v = modifyVar' v . runState

-- | @varStateT v m@ runs @m@ over the value stored in @v@, storing the
-- resulting value in @v@.
--
-- Note that @m@ may perform arbitrary computations in the base monad of
-- the variable. Therefore, the variable has the constraint @Locked m v@
-- which should only be defined for variables with the proper locking
-- semantics.
--
varStateT :: (Locked m v) => v s -> StateT s m a -> m a
varStateT v = atomicVar v . runStateT


-- | An infix version of `varState`.
(@:) :: (Variable m v) => v s -> State s a -> m a
(@:) = varState
infix 2 @:

-- | An infix version of `varState'`.
(@!) :: (Variable m v) => v s -> State s a -> m a
(@!) = varState'
infix 2 @!

-- | An infix version of `varStateT`.
(@@) :: (Locked m v) => v s -> StateT s m a -> m a
(@@) = varStateT
infix 2 @@
