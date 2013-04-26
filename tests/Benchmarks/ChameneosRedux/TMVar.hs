module TMVar (
	-- * TMVars
	TMVar,
	newTMVar,
	newEmptyTMVar,
	newTMVarIO,
	newEmptyTMVarIO,
	takeTMVar,
	putTMVar,
	readTMVar,
	swapTMVar,
	tryTakeTMVar,
	tryPutTMVar,
	isEmptyTMVar
  ) where

import LwConc.Substrate

newtype TMVar a = TMVar (PVar (Maybe a))
{- ^
A 'TMVar' is a synchronising variable, used
for communication between concurrent threads.  It can be thought of
as a box, which may be empty or full.
-}

-- |Create a 'TMVar' which contains the supplied value.
newTMVar :: a -> PTM (TMVar a)
newTMVar a = do
  t <- newPVar (Just a)
  return (TMVar t)

-- |@IO@ version of 'newTMVar'.  This is useful for creating top-level
-- 'TMVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTMVarIO :: a -> IO (TMVar a)
newTMVarIO a = do
  t <- newPVarIO (Just a)
  return (TMVar t)

-- |Create a 'TMVar' which is initially empty.
newEmptyTMVar :: PTM (TMVar a)
newEmptyTMVar = do
  t <- newPVar Nothing
  return (TMVar t)

-- |@IO@ version of 'newEmptyTMVar'.  This is useful for creating top-level
-- 'TMVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = do
  t <- newPVarIO Nothing
  return (TMVar t)

-- |Return the contents of the 'TMVar'.  If the 'TMVar' is currently
-- empty, the transaction will 'retry'.  After a 'takeTMVar',
-- the 'TMVar' is left empty.
takeTMVar :: TMVar a -> PTM a
takeTMVar (TMVar t) = do
  m <- readPVar t
  case m of
    Nothing -> retry
    Just a  -> do writePVar t Nothing; return a

-- | A version of 'takeTMVar' that does not 'retry'.  The 'tryTakeTMVar'
-- function returns 'Nothing' if the 'TMVar' was empty, or @'Just' a@ if
-- the 'TMVar' was full with contents @a@.  After 'tryTakeTMVar', the
-- 'TMVar' is left empty.
tryTakeTMVar :: TMVar a -> PTM (Maybe a)
tryTakeTMVar (TMVar t) = do
  m <- readPVar t
  case m of
    Nothing -> return Nothing
    Just a  -> do writePVar t Nothing; return (Just a)

-- |Put a value into a 'TMVar'.  If the 'TMVar' is currently full,
-- 'putTMVar' will 'retry'.
putTMVar :: TMVar a -> a -> PTM ()
putTMVar (TMVar t) a = do
  m <- readPVar t
  case m of
    Nothing -> do writePVar t (Just a); return ()
    Just _  -> retry

-- | A version of 'putTMVar' that does not 'retry'.  The 'tryPutTMVar'
-- function attempts to put the value @a@ into the 'TMVar', returning
-- 'True' if it was successful, or 'False' otherwise.
tryPutTMVar :: TMVar a -> a -> PTM Bool
tryPutTMVar (TMVar t) a = do
  m <- readPVar t
  case m of
    Nothing -> do writePVar t (Just a); return True
    Just _  -> return False

{-|
  This is a combination of 'takeTMVar' and 'putTMVar'; ie. it takes the value
  from the 'TMVar', puts it back, and also returns it.
-}
readTMVar :: TMVar a -> PTM a
readTMVar (TMVar t) = do
  m <- readPVar t
  case m of
    Nothing -> retry
    Just a  -> return a

-- |Swap the contents of a 'TMVar' for a new value.
swapTMVar :: TMVar a -> a -> PTM a
swapTMVar (TMVar t) new = do
  m <- readPVar t
  case m of
    Nothing -> retry
    Just old -> do writePVar t (Just new); return old

-- |Check whether a given 'TMVar' is empty.
--
-- Notice that the boolean value returned  is just a snapshot of
-- the state of the 'TMVar'. By the time you get to react on its result,
-- the 'TMVar' may have been filled (or emptied) - so be extremely
-- careful when using this operation.   Use 'tryTakeTMVar' instead if possible.
isEmptyTMVar :: TMVar a -> PTM Bool
isEmptyTMVar (TMVar t) = do
  m <- readPVar t
  case m of
    Nothing -> return True
    Just _  -> return False
