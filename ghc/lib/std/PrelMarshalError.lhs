% -----------------------------------------------------------------------------
% $Id: PrelMarshalError.lhs,v 1.3 2002/02/04 09:05:46 chak Exp $
%
% (c) The FFI task force, [2000..2002]
%

Marshalling support: Handling of common error conditions

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelMarshalError (

  -- I/O errors
  -- ----------

  IOErrorType,            -- abstract data type

  mkIOError,	          -- :: IOErrorType 
		          -- -> String 
		          -- -> Maybe FilePath 
		          -- -> Maybe Handle
		          -- -> IOError
  
  alreadyExistsErrorType, -- :: IOErrorType 
  doesNotExistErrorType,  -- :: IOErrorType 
  alreadyInUseErrorType,  -- :: IOErrorType 
  fullErrorType,	  -- :: IOErrorType 
  eofErrorType,	          -- :: IOErrorType 
  illegalOperationType,   -- :: IOErrorType 
  permissionErrorType,    -- :: IOErrorType 
  userErrorType,	  -- :: IOErrorType 

  annotateIOError,    	  -- :: IOError 
		      	  -- -> String 
		      	  -- -> Maybe FilePath 
		      	  -- -> Maybe Handle 
		      	  -- -> IOError 

  -- Result value checks
  -- -------------------

  -- throw an exception on specific return values
  --
  throwIf,       -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO a
  throwIf_,      -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO ()
  throwIfNeg,    -- :: (Ord a, Num a) 
	         -- =>                (a -> String) -> IO a       -> IO a
  throwIfNeg_,   -- :: (Ord a, Num a)
	         -- =>                (a -> String) -> IO a       -> IO ()
  throwIfNull,   -- ::                String        -> IO (Ptr a) -> IO (Ptr a)

  -- discard return value
  --
  void           -- IO a -> IO ()
) where

import PrelPtr
import PrelIOBase
import PrelMaybe
import PrelNum
import PrelBase


-- I/O errors
-- ----------

-- construct an IO error
--
mkIOError :: IOErrorType -> String -> Maybe FilePath -> Maybe Handle -> IOError
mkIOError errTy loc path hdl =
  IOException $ IOError hdl errTy loc "" path

-- pre-defined error types corresponding to the predicates in the standard
-- library `IO'
--
alreadyExistsErrorType, doesNotExistErrorType, alreadyInUseErrorType,
  fullErrorType, eofErrorType, illegalOperationType, permissionErrorType, 
  userErrorType :: IOErrorType 
alreadyExistsErrorType = AlreadyExists
doesNotExistErrorType  = NoSuchThing
alreadyInUseErrorType  = ResourceBusy
fullErrorType	       = ResourceExhausted
eofErrorType	       = EOF
illegalOperationType   = IllegalOperation
permissionErrorType    = PermissionDenied
userErrorType	       = OtherError

-- add location information and possibly a path and handle to an existing I/O
-- error 
--
-- * if no file path or handle is given, the corresponding value that's in the
--   error is left unaltered
--
annotateIOError :: IOError 
		-> String 
		-> Maybe FilePath 
		-> Maybe Handle 
		-> IOError 
annotateIOError (IOException (IOError hdl errTy _ str path)) loc opath ohdl = 
  IOException (IOError (hdl `mplus` ohdl) errTy loc str (path `mplus` opath))
  where
    Nothing `mplus` ys = ys
    xs      `mplus` _  = xs
annotateIOError exc				             _   _     _    = 
  exc


-- Result value checks
-- -------------------

-- guard an IO operation and throw an exception if the result meets the given
-- predicate 
--
-- * the second argument computes an error message from the result of the IO
--   operation
--
throwIf                 :: (a -> Bool) -> (a -> String) -> IO a -> IO a
throwIf pred msgfct act  = 
  do
    res <- act
    (if pred res then ioError . userError . msgfct else return) res

-- like `throwIf', but discarding the result
--
throwIf_                 :: (a -> Bool) -> (a -> String) -> IO a -> IO ()
throwIf_ pred msgfct act  = void $ throwIf pred msgfct act

-- guards against negative result values
--
throwIfNeg :: (Ord a, Num a) => (a -> String) -> IO a -> IO a
throwIfNeg  = throwIf (< 0)

-- like `throwIfNeg', but discarding the result
--
throwIfNeg_ :: (Ord a, Num a) => (a -> String) -> IO a -> IO ()
throwIfNeg_  = throwIf_ (< 0)

-- guards against null pointers
--
throwIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwIfNull  = throwIf (== nullPtr) . const

-- discard the return value of an IO action
--
void     :: IO a -> IO ()
void act  = act >> return ()

\end{code}
