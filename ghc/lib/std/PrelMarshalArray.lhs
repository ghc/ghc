% -----------------------------------------------------------------------------
% $Id: PrelMarshalArray.lhs,v 1.1 2001/01/11 17:25:57 simonmar Exp $
%
% (c) The FFI task force, 2000
%

Marshalling support: routines allocating, storing, and retrieving Haskell
lists that are represented as arrays in the foreign language

\begin{code}
module PrelMarshalArray (

  -- allocation
  --
  mallocArray,   -- :: Storable a => Int -> IO (Ptr a)
  mallocArray0,  -- :: Storable a => Int -> IO (Ptr a)

  allocaArray,   -- :: Storable a => Int -> (Ptr a -> IO b) -> IO b
  allocaArray0,  -- :: Storable a => Int -> (Ptr a -> IO b) -> IO b

  reallocArray,  -- :: Storable a => Ptr a -> Int -> IO (Ptr a)
  reallocArray0, -- :: Storable a => Ptr a -> Int -> IO (Ptr a)

  -- marshalling
  --
  peekArray,     -- :: Storable a =>         Int -> Ptr a -> IO [a]
  peekArray0,    -- :: (Storable a, Eq a) => a   -> Ptr a -> IO [a]

  pokeArray,     -- :: Storable a =>      Ptr a -> [a] -> IO ()
  pokeArray0,    -- :: Storable a => a -> Ptr a -> [a] -> IO ()

  -- combined allocation and marshalling
  --
  newArray,      -- :: Storable a =>      [a] -> IO (Ptr a)
  newArray0,     -- :: Storable a => a -> [a] -> IO (Ptr a)

  withArray,     -- :: Storable a =>      [a] -> (Ptr a -> IO b) -> IO b
  withArray0,    -- :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b

  -- copying (argument order: destination, source)
  --
  copyArray,     -- :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
  moveArray,     -- :: Storable a => Ptr a -> Ptr a -> Int -> IO ()

  -- indexing
  --
  advancePtr     -- :: Storable a => Ptr a -> Int -> Ptr a
) where

import Monad	    (zipWithM_)

import PrelPtr	        (Ptr, plusPtr)
import PrelStorable     (Storable(sizeOf, peekElemOff, pokeElemOff))
import PrelMarshalAlloc (mallocBytes, allocaBytes, reallocBytes)
import PrelMarshalUtils (copyBytes, moveBytes)


-- allocation
-- ----------

-- allocate storage for the given number of elements of a storable type
--
mallocArray :: Storable a => Int -> IO (Ptr a)
mallocArray  = doMalloc undefined
  where
    doMalloc            :: Storable a => a -> Int -> IO (Ptr a)
    doMalloc dummy size  = mallocBytes (size * sizeOf dummy)

-- like `mallocArray', but add an extra element to signal the end of the array
--
mallocArray0      :: Storable a => Int -> IO (Ptr a)
mallocArray0 size  = mallocArray (size + 1)

-- temporarily allocate space for the given number of elements
--
-- * see `MarshalAlloc.alloca' for the storage lifetime constraints
--
allocaArray :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray  = doAlloca undefined
  where
    doAlloca            :: Storable a => a -> Int -> (Ptr a -> IO b) -> IO b
    doAlloca dummy size  = allocaBytes (size * sizeOf dummy)

-- like `allocaArray', but add an extra element to signal the end of the array
--
allocaArray0      :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 size  = allocaArray (size + 1)

-- adjust the size of an array
--
reallocArray :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray  = doRealloc undefined
  where
    doRealloc                :: Storable a => a -> Ptr a -> Int -> IO (Ptr a)
    doRealloc dummy ptr size  = reallocBytes ptr (size * sizeOf dummy)

-- adjust the size of an array while adding an element for the end marker
--
reallocArray0          :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 ptr size  = reallocArray ptr (size + 1)


-- marshalling
-- -----------

-- convert an array of given length into a Haskell list
--
peekArray          :: Storable a => Int -> Ptr a -> IO [a]
peekArray size ptr  = mapM (peekElemOff ptr) [0..size-1]

-- convert an array terminated by the given end marker into a Haskell list
--
peekArray0            :: (Storable a, Eq a) => a -> Ptr a -> IO [a]
peekArray0 marker ptr  = loop 0
  where
    loop i = do
        val <- peekElemOff ptr i
        if val == marker then return [] else do
            rest <- loop (i+1)
            return (val:rest)

-- write the list elements consecutive into memory
--
pokeArray          :: Storable a => Ptr a -> [a] -> IO ()
pokeArray ptr vals  = zipWithM_ (pokeElemOff ptr) [0..] vals

-- write the list elements consecutive into memory and terminate them with the
-- given marker element
--
pokeArray0		   :: Storable a => a -> Ptr a -> [a] -> IO ()
pokeArray0 marker ptr vals  = do
  pokeArray ptr vals
  pokeElemOff ptr (length vals) marker


-- combined allocation and marshalling
-- -----------------------------------

-- write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values
--
newArray      :: Storable a => [a] -> IO (Ptr a)
newArray vals  = do
  ptr <- mallocArray (length vals)
  pokeArray ptr vals
  return ptr

-- write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values, where the end is fixed by the given end maker
--
newArray0             :: Storable a => a -> [a] -> IO (Ptr a)
newArray0 marker vals  = do
  ptr <- mallocArray0 (length vals)
  pokeArray0 marker ptr vals
  return ptr

-- temporarily store a list of storable values in memory
--
withArray        :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArray vals f  = allocaArray (length vals) $ \ptr -> do
  pokeArray ptr vals
  f ptr

-- `like withArray', but a terminator indicates where the array ends
--
withArray0               :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b
withArray0 marker vals f  = allocaArray0 (length vals) $ \ptr -> do
  pokeArray0 marker ptr vals
  f ptr


-- copying
-- -------

-- copies the given number of elements from the second array (source) into the
-- first array (destination); the copied areas may *not* overlap
--
copyArray :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
copyArray  = doCopy undefined
  where
    doCopy                     :: Storable a => a -> Ptr a -> Ptr a -> Int -> IO ()
    doCopy dummy dest src size  = copyBytes dest src (size * sizeOf dummy)

-- copies the given number of elements from the second array (source) into the
-- first array (destination); the copied areas *may* overlap
--
moveArray :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
moveArray  = doMove undefined
  where
    doMove                     :: Storable a => a -> Ptr a -> Ptr a -> Int -> IO ()
    doMove dummy dest src size  = moveBytes dest src (size * sizeOf dummy)


-- indexing
-- --------

-- advance a pointer into an array by the given number of elements
--
advancePtr :: Storable a => Ptr a -> Int -> Ptr a
advancePtr  = doAdvance undefined
  where
    doAdvance             :: Storable a => a -> Ptr a -> Int -> Ptr a
    doAdvance dummy ptr i  = ptr `plusPtr` (i * sizeOf dummy)

\end{code}
