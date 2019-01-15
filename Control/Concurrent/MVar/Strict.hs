{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.MVar.Strict
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Synchronising, strict variables
--
-- Values placed in an MVar are evaluated to head normal form
-- before being placed in the MVar, preventing a common source of
-- space-leaks involving synchronising variables.
--
-----------------------------------------------------------------------------

module Control.Concurrent.MVar.Strict
        (
          -- * @MVar@s
          MVar          -- abstract
        , newEmptyMVar  -- :: IO (MVar a)
        , newMVar       -- :: a -> IO (MVar a)
        , takeMVar      -- :: MVar a -> IO a
        , putMVar       -- :: MVar a -> a -> IO ()
        , readMVar      -- :: MVar a -> IO a
        , swapMVar      -- :: MVar a -> a -> IO a
        , tryTakeMVar   -- :: MVar a -> IO (Maybe a)
        , tryPutMVar    -- :: MVar a -> a -> IO Bool
        , tryReadMVar   -- :: MVar a -> IO (Maybe a)
        , isEmptyMVar   -- :: MVar a -> IO Bool
        , withMVar      -- :: MVar a -> (a -> IO b) -> IO b
        , modifyMVar_   -- :: MVar a -> (a -> IO a) -> IO ()
        , modifyMVar    -- :: MVar a -> (a -> IO (a,b)) -> IO b
        , addMVarFinalizer -- :: MVar a -> IO () -> IO ()
    ) where

import Control.Concurrent.MVar
  ( MVar, newEmptyMVar, readMVar, takeMVar
  , tryTakeMVar, isEmptyMVar, addMVarFinalizer, withMVar
  )
#if MIN_VERSION_base(4,7,0)
import Control.Concurrent.MVar (tryReadMVar)
#endif
import qualified Control.Concurrent.MVar as MV

import Control.Exception as Exception
-- import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Monad ((>=>))

-- Note: we use 'force' in many places one might expect to find `rnf` instead, and
-- @force x `seq`@ where one might expect to see @deepseq@. This ensures that the
-- value is forced to WHNF whether or not its NFData instance does so, and also
-- ensures that the compiler knows this is the case.

-- |Put a value into an 'MVar'.  If the 'MVar' is currently full,
-- 'putMVar' will wait until it becomes empty.
--
-- There are two further important properties of 'putMVar':
--
--   * 'putMVar' is single-wakeup.  That is, if there are multiple
--     threads blocked in 'putMVar', and the 'MVar' becomes empty,
--     only one thread will be woken up.  The runtime guarantees that
--     the woken thread completes its 'putMVar' operation.
--
--   * When multiple threads are blocked on an 'MVar', they are
--     woken up in FIFO order.  This is useful for providing
--     fairness properties of abstractions built using 'MVar's.
--
putMVar  :: NFData a => MVar a -> a -> IO ()
putMVar !mv x = evaluate (force x) >>= MV.putMVar mv

-- | A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
-- attempts to put the value @a@ into the 'MVar', returning 'True' if
-- it was successful, or 'False' otherwise.
--
tryPutMVar  :: NFData a => MVar a -> a -> IO Bool
tryPutMVar !mv x = evaluate (force x) >>= MV.tryPutMVar mv

#if !MIN_VERSION_base(4,7,0)
-- |A non-blocking version of 'readMVar'.  The 'tryReadMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.
tryReadMVar :: MVar a -> IO (Maybe a)
-- This is a best-effort compatibility shim for really old GHC versions.
-- It's not really what you'd call *right*.
tryReadMVar !m = uninterruptibleMask $ \_ -> do
  mv <- tryTakeMVar m
  case mv of
    Nothing -> return Nothing
    Just v -> MV.tryPutMVar m v >> return mv
#endif

-- |Create an 'MVar' which contains the supplied value.
newMVar :: NFData a => a -> IO (MVar a)
newMVar value = evaluate (force value) >>= MV.newMVar

{-|
  Take a value from an 'MVar', put a new value into the 'MVar' and
  return the value taken. Note that there is a race condition whereby
  another process can put something in the 'MVar' after the take
  happens but before the put does.
-}
swapMVar :: NFData a => MVar a -> a -> IO a
swapMVar !mvar new = do
    -- Force this first to avoid holding the MVar too long and to ensure that
    -- the MVar isn't left empty if forcing fails.
    new' <- evaluate (force new)
    mask $ \_ -> do
      old <- takeMVar mvar
      MV.putMVar mvar new'
      return old

{-|
  A safe wrapper for modifying the contents of an 'MVar'.  Like 'withMVar', 
  'modifyMVar' will replace the original contents of the 'MVar' if an
  exception is raised during the operation.
-}
{-# INLINE modifyMVar_ #-}
modifyMVar_ :: NFData a => MVar a -> (a -> IO a) -> IO ()
modifyMVar_ m io = MV.modifyMVar_ m $ io >=> evaluate . force

{-|
  A slight variation on 'modifyMVar_' that allows a value to be
  returned (@b@) in addition to the modified value of the 'MVar'.
-}
{-# INLINE modifyMVar #-}
modifyMVar :: NFData a => MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io = MV.modifyMVar m $ io >=> \pq -> evaluate (force (fst pq) `seq` pq)
