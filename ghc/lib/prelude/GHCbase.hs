{- The GHCbase module includes all the basic
   (next-level-above-primitives) GHC-specific code;
   used to define Prelude.hs, and also other "packagings"
   of Glasgow extensions.
   
   Users should not import it directly.
-}
module GHCbase where

import Array		( array, bounds, assocs )
import Char		(isDigit,isUpper,isSpace,isAlphanum,isAlpha,isOctDigit,isHexDigit)
import Ix
import Ratio
import qualified GHCps	( packString, packCBytes, comparePS, unpackPS )
import qualified GHCio  ( IOError )
import qualified Monad
import GHCerr

infixr 0 `seq`, `par`, `fork`

{- =============================================================
There's a lot in GHCbase.  It's set out as follows:

* Classes (CCallable, CReturnable, ...)

* Types and their instances

* ST, PrimIO, and IO monads

* Basic arrays

* Variables

* Thread waiting

* Other support functions

============================================================= -}

{- =============================================================
** CLASSES
-}

class CCallable   a
class CReturnable a

{- =============================================================
** TYPES and their instances
-}
data Addr = A# Addr# deriving (Eq, Ord) -- Glasgow extension
instance CCallable Addr
instance CReturnable Addr

---------------------------------------------------------------
data Word = W# Word# deriving (Eq, Ord) -- Glasgow extension
instance CCallable Word
instance CReturnable Word

---------------------------------------------------------------
data PackedString
  = PS	ByteArray#  -- the bytes
	Int#	    -- length (*not* including NUL at the end)
	Bool	    -- True <=> contains a NUL
  | CPS	Addr#	    -- pointer to the (null-terminated) bytes in C land
	Int#	    -- length, as per strlen
		    -- definitely doesn't contain a NUL

instance Eq PackedString where
    x == y  = compare x y == EQ
    x /= y  = compare x y /= EQ

instance Ord PackedString where
    compare = GHCps.comparePS
    x <= y  = compare x y /= GT
    x <	 y  = compare x y == LT
    x >= y  = compare x y /= LT
    x >	 y  = compare x y == GT
    max x y = case (compare x y) of { LT -> y ; EQ -> x ; GT -> x }
    min x y = case (compare x y) of { LT -> x ; EQ -> x ; GT -> y }

--instance Read PackedString: ToDo

instance Show PackedString where
    showsPrec p ps r = showsPrec p (GHCps.unpackPS ps) r
    showList = showList__ (showsPrec 0) 

---------------------------------------------------------------
data State a = S# (State# a)

data ForeignObj = ForeignObj ForeignObj#
instance CCallable   ForeignObj

#ifndef __PARALLEL_HASKELL__
data StablePtr a = StablePtr (StablePtr# a)
instance CCallable   (StablePtr a)
instance CReturnable (StablePtr a)
#endif

eqForeignObj   :: ForeignObj -> ForeignObj -> Bool
makeForeignObj :: Addr       -> Addr       -> PrimIO ForeignObj

makeForeignObj (A# obj) (A# finaliser) = ST $ \ (S# s#) ->
    case makeForeignObj# obj finaliser s# of
      StateAndForeignObj# s1# fo# -> (ForeignObj fo#, S# s1#)

eqForeignObj mp1 mp2
  = unsafePerformPrimIO (_ccall_ eqForeignObj mp1 mp2) /= (0::Int)

instance Eq ForeignObj where 
    p == q = eqForeignObj p q
    p /= q = not (eqForeignObj p q)

#ifndef __PARALLEL_HASKELL__

-- Nota Bene: it is important {\em not\/} to inline calls to
-- @makeStablePtr#@ since the corresponding macro is very long and we'll
-- get terrible code-bloat.

makeStablePtr  :: a -> PrimIO (StablePtr a)
deRefStablePtr :: StablePtr a -> PrimIO a
freeStablePtr  :: StablePtr a -> PrimIO ()

performGC      :: PrimIO ()

{-# INLINE deRefStablePtr #-}
{-# INLINE freeStablePtr #-}
{-# INLINE performGC #-}

makeStablePtr f = ST $ \ (S# rw1#) ->
    case makeStablePtr# f rw1# of
      StateAndStablePtr# rw2# sp# -> (StablePtr sp#, S# rw2#)

deRefStablePtr (StablePtr sp#) = ST $ \ (S# rw1#) ->
    case deRefStablePtr# sp# rw1# of
      StateAndPtr# rw2# a -> (a, S# rw2#)

freeStablePtr sp = _ccall_ freeStablePointer sp

performGC = _ccall_GC_ StgPerformGarbageCollection

#endif /* !__PARALLEL_HASKELL__ */

---------------------------------------------------------------
data Return2GMPs     = Return2GMPs     Int# Int# ByteArray# Int# Int# ByteArray#
data ReturnIntAndGMP = ReturnIntAndGMP Int# Int# Int# ByteArray#

data StateAndPtr#    s elt = StateAndPtr#    (State# s) elt 

data StateAndChar#   s     = StateAndChar#   (State# s) Char# 
data StateAndInt#    s     = StateAndInt#    (State# s) Int# 
data StateAndWord#   s     = StateAndWord#   (State# s) Word#
data StateAndFloat#  s     = StateAndFloat#  (State# s) Float# 
data StateAndDouble# s     = StateAndDouble# (State# s) Double#  
data StateAndAddr#   s     = StateAndAddr#   (State# s) Addr#

#ifndef __PARALLEL_HASKELL__
data StateAndStablePtr# s a = StateAndStablePtr# (State# s) (StablePtr# a)
#endif
data StateAndForeignObj# s  = StateAndForeignObj# (State# s) ForeignObj#

data StateAndArray#            s elt = StateAndArray#        (State# s) (Array# elt) 
data StateAndMutableArray#     s elt = StateAndMutableArray# (State# s) (MutableArray# s elt)
data StateAndByteArray#        s = StateAndByteArray#        (State# s) ByteArray# 
data StateAndMutableByteArray# s = StateAndMutableByteArray# (State# s) (MutableByteArray# s)

data StateAndSynchVar# s elt = StateAndSynchVar# (State# s) (SynchVar# s elt)

---------------------------------------------------------------
data Lift a = Lift a
{-# GENERATE_SPECS data a :: Lift a #-}

{- =============================================================
** ST, PrimIO, and IO monads
-}

---------------------------------------------------------------
--The state-transformer proper
-- By default the monad is strict; too many people got bitten by
-- space leaks when it was lazy.

newtype ST s a = ST (State s -> (a, State s))

runST (ST m)
  = case m (S# realWorld#) of
      (r,_) -> r

instance Monad (ST s) where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    return x = ST $ \ s@(S# _) -> (x, s)
    m >> k   =  m >>= \ _ -> k

    (ST m) >>= k
      = ST $ \ s ->
	case (m s) of {(r, new_s) ->
	case (k r) of { ST k2 ->
	(k2 new_s) }}

{-# INLINE returnST #-}

-- here for backward compatibility:
returnST :: a -> ST s a
thenST	 :: ST s a -> (a -> ST s b) -> ST s b
seqST	 :: ST s a -> ST s b -> ST s b

returnST = return
thenST   = (>>=)
seqST	 = (>>)

-- not sure whether to 1.3-ize these or what...
{-# INLINE returnStrictlyST #-}
{-# INLINE thenStrictlyST #-}
{-# INLINE seqStrictlyST #-}

{-# GENERATE_SPECS returnStrictlyST a #-}
returnStrictlyST :: a -> ST s a

{-# GENERATE_SPECS thenStrictlyST a b #-}
thenStrictlyST :: ST s a -> (a -> ST s b) -> ST s b

{-# GENERATE_SPECS seqStrictlyST a b #-}
seqStrictlyST :: ST s a -> ST s b -> ST s b

returnStrictlyST a = ST $ \ s@(S# _) -> (a, s)

thenStrictlyST (ST m) k = ST $ \ s ->	-- @(S# _)   Omitted SLPJ [May95] no need to evaluate the state
    case (m s) of { (r, new_s) ->
    case (k r) of { ST k2     ->
    (k2 new_s) }}

seqStrictlyST (ST m) (ST k) = ST $ \ s ->	-- @(S# _)   Omitted SLPJ [May95] no need to evaluate the state
    case (m s) of { (_, new_s) ->
    (k new_s) }

-- BUILT-IN: runST (see Builtin.hs)

unsafeInterleaveST :: ST s a -> ST s a    -- ToDo: put in state-interface.tex
unsafeInterleaveST (ST m) = ST $ \ s ->
    let
	(r, new_s) = m s
    in
    (r, s)

fixST :: (a -> ST s a) -> ST s a
fixST k = ST $ \ s ->
    let (ST k_r)  = k r
	ans       = k_r s
	(r,new_s) = ans
    in
    ans

-- more backward compatibility stuff:
listST		:: [ST s a] -> ST s [a]
mapST		:: (a -> ST s b) -> [a] -> ST s [b]
mapAndUnzipST	:: (a -> ST s (b,c)) -> [a] -> ST s ([b],[c])

listST		= accumulate
mapST		= mapM
mapAndUnzipST	= Monad.mapAndUnzipL

forkST :: ST s a -> ST s a

#ifndef __CONCURRENT_HASKELL__
forkST x = x
#else

forkST (ST action) = ST $ \ s ->
   let
    (r, new_s) = action s
   in
    new_s `fork__` (r, s)
 where
    fork__ x y = case (fork# x) of { 0# -> parError; _ -> y }

#endif {- concurrent -}

----------------------------------------------------------------------------
type PrimIO a = ST RealWorld a

fixPrimIO :: (a -> PrimIO a) -> PrimIO a
fixPrimIO = fixST

stToIO	   :: ST RealWorld a -> IO a
primIOToIO :: PrimIO a       -> IO a
ioToST	   :: IO a -> ST RealWorld a
ioToPrimIO :: IO a -> PrimIO       a

primIOToIO = stToIO -- for backwards compatibility
ioToPrimIO = ioToST

stToIO (ST m) = IO $ ST $ \ s ->
    case (m s) of { (r, new_s) ->
    (Right r, new_s) }

ioToST (IO (ST io)) = ST $ \ s ->
    case (io s) of { (r, new_s) ->
    case r of
      Right a -> (a, new_s)
      Left  e -> error ("I/O Error (ioToST): " ++ showsPrec 0 e "\n")
    }

{-# GENERATE_SPECS unsafePerformPrimIO a #-}
unsafePerformPrimIO	:: PrimIO a -> a
unsafeInterleavePrimIO	:: PrimIO a -> PrimIO a
forkPrimIO		:: PrimIO a -> PrimIO a

unsafePerformPrimIO	= runST
unsafeInterleavePrimIO	= unsafeInterleaveST
forkPrimIO		= forkST

-- the following functions are now there for backward compatibility mostly:

{-# GENERATE_SPECS returnPrimIO a #-}
returnPrimIO    :: a -> PrimIO a

{-# GENERATE_SPECS thenPrimIO b #-}
thenPrimIO      :: PrimIO a -> (a -> PrimIO b) -> PrimIO b

{-# GENERATE_SPECS seqPrimIO b #-}
seqPrimIO	:: PrimIO a -> PrimIO b -> PrimIO b

listPrimIO	:: [PrimIO a] -> PrimIO [a]
mapPrimIO	:: (a -> PrimIO b) -> [a] -> PrimIO [b]
mapAndUnzipPrimIO :: (a -> PrimIO (b,c)) -> [a] -> PrimIO ([b],[c])

{-# INLINE returnPrimIO #-}
{-# INLINE thenPrimIO   #-}
{-# INLINE seqPrimIO  #-}

returnPrimIO  	  = return
thenPrimIO    	  = (>>=)
seqPrimIO     	  = (>>)
listPrimIO    	  = accumulate
mapPrimIO     	  = mapM
mapAndUnzipPrimIO = Monad.mapAndUnzipL

---------------------------------------------------------
newtype IO a = IO (PrimIO (Either GHCio.IOError a))

instance  Functor IO where
   map f x = x >>= (return . f)

instance  Monad IO  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      =  m >>= \ _ -> k
    return x	= IO $ ST $ \ s@(S# _) -> (Right x, s)

    (IO (ST m)) >>= k
      = IO $ ST $ \ s ->
	let  (r, new_s) = m s  in
	case r of
	  Left err -> (Left err, new_s)
	  Right  x -> case (k x) of { IO (ST k2) ->
		      k2 new_s }

instance  Show (IO a)  where
    showsPrec p f  = showString "<<IO action>>"
    showList	   = showList__ (showsPrec 0)

fixIO :: (a -> IO a) -> IO a
    -- not required but worth having around

fixIO k = IO $ ST $ \ s ->
    let
	(IO (ST k_loop)) = k loop
	result           = k_loop s
	(Right loop, _)  = result
    in
    result

{- =============================================================
** BASIC ARRAY (and ByteArray) SUPPORT
-}

type IPr = (Int, Int)

data Ix ix => Array      ix elt = Array     (ix,ix) (Array# elt)
data Ix ix => ByteArray ix      = ByteArray (ix,ix) ByteArray#

instance CCallable (ByteArray ix)

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a'  	        =  assocs a == assocs a'
    a /= a'  	        =  assocs a /= assocs a'

instance  (Ix a, Ord b) => Ord (Array a b)  where
    compare a b = compare (assocs a) (assocs b)

instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    shows (bounds a) . showChar ' ' .
		    shows (assocs a)                  )
    showList = showList__ (showsPrec 0)

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
	   (\r -> [(array b as, u) | ("array",s) <- lex r,
				     (b,t)       <- reads s,
				     (as,u)      <- reads t   ])
    readList = readList__ (readsPrec 0)

-----------------------------------------------------------------
-- Mutable arrays
{-
Idle ADR question: What's the tradeoff here between flattening these
datatypes into @MutableArray ix ix (MutableArray# s elt)@ and using
it as is?  As I see it, the former uses slightly less heap and
provides faster access to the individual parts of the bounds while the
code used has the benefit of providing a ready-made @(lo, hi)@ pair as
required by many array-related functions.  Which wins? Is the
difference significant (probably not).

Idle AJG answer: When I looked at the outputted code (though it was 2
years ago) it seems like you often needed the tuple, and we build
it frequently. Now we've got the overloading specialiser things
might be different, though.
-}

data Ix ix => MutableArray     s ix elt = MutableArray     (ix,ix) (MutableArray# s elt)
data Ix ix => MutableByteArray s ix     = MutableByteArray (ix,ix) (MutableByteArray# s)

instance CCallable (MutableByteArray s ix)

newArray :: Ix ix => (ix,ix) -> elt -> ST s (MutableArray s ix elt)
newCharArray, newIntArray, newAddrArray, newFloatArray, newDoubleArray
	 :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 

{-# SPECIALIZE newArray      :: IPr       -> elt -> ST s (MutableArray s Int elt),
				(IPr,IPr) -> elt -> ST s (MutableArray s IPr elt)
  #-}
{-# SPECIALIZE newCharArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newIntArray    :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newAddrArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newFloatArray  :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newDoubleArray :: IPr -> ST s (MutableByteArray s Int) #-}

newArray ixs@(ix_start, ix_end) init = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else (index ixs ix_end) + 1) of { I# x -> x }
	-- size is one bigger than index of last elem
    in
    case (newArray# n# init s#)     of { StateAndMutableArray# s2# arr# ->
    (MutableArray ixs arr#, S# s2#)}

newCharArray ixs@(ix_start, ix_end) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case (newCharArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    (MutableByteArray ixs barr#, S# s2#)}

newIntArray ixs@(ix_start, ix_end) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case (newIntArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    (MutableByteArray ixs barr#, S# s2#)}

newAddrArray ixs@(ix_start, ix_end) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case (newAddrArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    (MutableByteArray ixs barr#, S# s2#)}

newFloatArray ixs@(ix_start, ix_end) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case (newFloatArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    (MutableByteArray ixs barr#, S# s2#)}

newDoubleArray ixs@(ix_start, ix_end) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case (newDoubleArray# n# s#)  of { StateAndMutableByteArray# s2# barr# ->
    (MutableByteArray ixs barr#, S# s2#)}

boundsOfArray     :: Ix ix => MutableArray s ix elt -> (ix, ix)  
boundsOfByteArray :: Ix ix => MutableByteArray s ix -> (ix, ix)

{-# SPECIALIZE boundsOfArray     :: MutableArray s Int elt -> IPr #-}
{-# SPECIALIZE boundsOfByteArray :: MutableByteArray s Int -> IPr #-}

boundsOfArray     (MutableArray     ixs _) = ixs
boundsOfByteArray (MutableByteArray ixs _) = ixs

readArray   	:: Ix ix => MutableArray s ix elt -> ix -> ST s elt 

readCharArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Char 
readIntArray    :: Ix ix => MutableByteArray s ix -> ix -> ST s Int
readAddrArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Addr
readFloatArray  :: Ix ix => MutableByteArray s ix -> ix -> ST s Float
readDoubleArray :: Ix ix => MutableByteArray s ix -> ix -> ST s Double

{-# SPECIALIZE readArray       :: MutableArray s Int elt -> Int -> ST s elt,
				  MutableArray s IPr elt -> IPr -> ST s elt
  #-}
{-# SPECIALIZE readCharArray   :: MutableByteArray s Int -> Int -> ST s Char #-}
{-# SPECIALIZE readIntArray    :: MutableByteArray s Int -> Int -> ST s Int #-}
{-# SPECIALIZE readAddrArray   :: MutableByteArray s Int -> Int -> ST s Addr #-}
--NO:{-# SPECIALIZE readFloatArray  :: MutableByteArray s Int -> Int -> ST s Float #-}
{-# SPECIALIZE readDoubleArray :: MutableByteArray s Int -> Int -> ST s Double #-}

readArray (MutableArray ixs arr#) n = ST $ \ (S# s#) ->
    case (index ixs n)	    	of { I# n# ->
    case readArray# arr# n# s#	of { StateAndPtr# s2# r ->
    (r, S# s2#)}}

readCharArray (MutableByteArray ixs barr#) n = ST $ \ (S# s#) ->
    case (index ixs n)	    	    	of { I# n# ->
    case readCharArray# barr# n# s#	of { StateAndChar# s2# r# ->
    (C# r#, S# s2#)}}

readIntArray (MutableByteArray ixs barr#) n = ST $ \ (S# s#) ->
    case (index ixs n)	    	    	of { I# n# ->
    case readIntArray# barr# n# s#	of { StateAndInt# s2# r# ->
    (I# r#, S# s2#)}}

readAddrArray (MutableByteArray ixs barr#) n = ST $ \ (S# s#) ->
    case (index ixs n)	    	    	of { I# n# ->
    case readAddrArray# barr# n# s#	of { StateAndAddr# s2# r# ->
    (A# r#, S# s2#)}}

readFloatArray (MutableByteArray ixs barr#) n = ST $ \ (S# s#) ->
    case (index ixs n)	    	    	of { I# n# ->
    case readFloatArray# barr# n# s#	of { StateAndFloat# s2# r# ->
    (F# r#, S# s2#)}}

readDoubleArray (MutableByteArray ixs barr#) n = ST $ \ (S# s#) ->
    case (index ixs n) 	    	    	of { I# n# ->
    case readDoubleArray# barr# n# s#	of { StateAndDouble# s2# r# ->
    (D# r#, S# s2#)}}

--Indexing of ordinary @Arrays@ is standard Haskell and isn't defined here.
indexCharArray   :: Ix ix => ByteArray ix -> ix -> Char 
indexIntArray    :: Ix ix => ByteArray ix -> ix -> Int
indexAddrArray   :: Ix ix => ByteArray ix -> ix -> Addr
indexFloatArray  :: Ix ix => ByteArray ix -> ix -> Float
indexDoubleArray :: Ix ix => ByteArray ix -> ix -> Double

{-# SPECIALIZE indexCharArray   :: ByteArray Int -> Int -> Char #-}
{-# SPECIALIZE indexIntArray    :: ByteArray Int -> Int -> Int #-}
{-# SPECIALIZE indexAddrArray   :: ByteArray Int -> Int -> Addr #-}
--NO:{-# SPECIALIZE indexFloatArray  :: ByteArray Int -> Int -> Float #-}
{-# SPECIALIZE indexDoubleArray :: ByteArray Int -> Int -> Double #-}

indexCharArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexCharArray# barr# n# 	of { r# ->
    (C# r#)}}

indexIntArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexIntArray# barr# n# 	of { r# ->
    (I# r#)}}

indexAddrArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexAddrArray# barr# n# 	of { r# ->
    (A# r#)}}

indexFloatArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexFloatArray# barr# n# 	of { r# ->
    (F# r#)}}

indexDoubleArray (ByteArray ixs barr#) n
  = case (index ixs n) 	    	    	of { I# n# ->
    case indexDoubleArray# barr# n# 	of { r# ->
    (D# r#)}}

--Indexing off @Addrs@ is similar, and therefore given here.
indexCharOffAddr   :: Addr -> Int -> Char
indexIntOffAddr    :: Addr -> Int -> Int
indexAddrOffAddr   :: Addr -> Int -> Addr
indexFloatOffAddr  :: Addr -> Int -> Float
indexDoubleOffAddr :: Addr -> Int -> Double

indexCharOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexCharOffAddr# addr# n# 	of { r# ->
    (C# r#)}}

indexIntOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexIntOffAddr# addr# n# 	of { r# ->
    (I# r#)}}

indexAddrOffAddr (A# addr#) n
  = case n  	    	    	    	of { I# n# ->
    case indexAddrOffAddr# addr# n# 	of { r# ->
    (A# r#)}}

indexFloatOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexFloatOffAddr# addr# n# 	of { r# ->
    (F# r#)}}

indexDoubleOffAddr (A# addr#) n
  = case n  	    	 	    	of { I# n# ->
    case indexDoubleOffAddr# addr# n# 	of { r# ->
    (D# r#)}}

writeArray  	 :: Ix ix => MutableArray s ix elt -> ix -> elt -> ST s () 
writeCharArray   :: Ix ix => MutableByteArray s ix -> ix -> Char -> ST s () 
writeIntArray    :: Ix ix => MutableByteArray s ix -> ix -> Int  -> ST s () 
writeAddrArray   :: Ix ix => MutableByteArray s ix -> ix -> Addr -> ST s () 
writeFloatArray  :: Ix ix => MutableByteArray s ix -> ix -> Float -> ST s () 
writeDoubleArray :: Ix ix => MutableByteArray s ix -> ix -> Double -> ST s () 

{-# SPECIALIZE writeArray  	:: MutableArray s Int elt -> Int -> elt -> ST s (),
				   MutableArray s IPr elt -> IPr -> elt -> ST s ()
  #-}
{-# SPECIALIZE writeCharArray   :: MutableByteArray s Int -> Int -> Char -> ST s () #-}
{-# SPECIALIZE writeIntArray    :: MutableByteArray s Int -> Int -> Int  -> ST s () #-}
{-# SPECIALIZE writeAddrArray   :: MutableByteArray s Int -> Int -> Addr -> ST s () #-}
--NO:{-# SPECIALIZE writeFloatArray  :: MutableByteArray s Int -> Int -> Float -> ST s () #-}
{-# SPECIALIZE writeDoubleArray :: MutableByteArray s Int -> Int -> Double -> ST s () #-}

writeArray (MutableArray ixs arr#) n ele = ST $ \ (S# s#) ->
    case index ixs n		    of { I# n# ->
    case writeArray# arr# n# ele s# of { s2# ->
    ((), S# s2#)}}

writeCharArray (MutableByteArray ixs barr#) n (C# ele) = ST $ \ (S# s#) ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeCharArray# barr# n# ele s#    of { s2#   ->
    ((), S# s2#)}}

writeIntArray (MutableByteArray ixs barr#) n (I# ele) = ST $ \ (S# s#) ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeIntArray# barr# n# ele s#     of { s2#   ->
    ((), S# s2#)}}

writeAddrArray (MutableByteArray ixs barr#) n (A# ele) = ST $ \ (S# s#) ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeAddrArray# barr# n# ele s#    of { s2#   ->
    ((), S# s2#)}}

writeFloatArray (MutableByteArray ixs barr#) n (F# ele) = ST $ \ (S# s#) ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeFloatArray# barr# n# ele s#   of { s2#   ->
    ((), S# s2#)}}

writeDoubleArray (MutableByteArray ixs barr#) n (D# ele) = ST $ \ (S# s#) ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeDoubleArray# barr# n# ele s#  of { s2#   ->
    ((), S# s2#)}}

freezeArray	  :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)
freezeCharArray   :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeIntArray    :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeAddrArray   :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeFloatArray  :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeDoubleArray :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

{-# SPECIALISE freezeArray :: MutableArray s Int elt -> ST s (Array Int elt),
			      MutableArray s IPr elt -> ST s (Array IPr elt)
  #-}
{-# SPECIALISE freezeCharArray :: MutableByteArray s Int -> ST s (ByteArray Int) #-}

freezeArray (MutableArray ixs@(ix_start, ix_end) arr#) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else (index ixs ix_end) + 1) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndArray# s2# frozen# ->
    (Array ixs frozen#, S# s2#)}
  where
    freeze  :: MutableArray# s ele	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndArray# s ele

    freeze arr# n# s#
      = case newArray# n# init s#	      of { StateAndMutableArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#     of { StateAndMutableArray# s3# newarr2# ->
	unsafeFreezeArray# newarr2# s3#
	}}
      where
	init = error "freezeArray: element not copied"

	copy :: Int# -> Int#
	     -> MutableArray# s ele -> MutableArray# s ele
	     -> State# s
	     -> StateAndMutableArray# s ele

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableArray# s# to#
	  | True
	    = case readArray#  from# cur#     s#  of { StateAndPtr# s1# ele ->
	      case writeArray# to#   cur# ele s1# of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeCharArray (MutableByteArray ixs@(ix_start, ix_end) arr#) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (ByteArray ixs frozen#, S# s2#) }
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newCharArray# n# s#)    	   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | True
	    = case (readCharArray#  from# cur#     s#)  of { StateAndChar# s1# ele ->
	      case (writeCharArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeIntArray (MutableByteArray ixs@(ix_start, ix_end) arr#) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (ByteArray ixs frozen#, S# s2#) }
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newIntArray# n# s#)    	   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | True
	    = case (readIntArray#  from# cur#     s#)  of { StateAndInt# s1# ele ->
	      case (writeIntArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeAddrArray (MutableByteArray ixs@(ix_start, ix_end) arr#) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (ByteArray ixs frozen#, S# s2#) }
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newAddrArray# n# s#)    	   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | True
	    = case (readAddrArray#  from# cur#     s#)  of { StateAndAddr# s1# ele ->
	      case (writeAddrArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeFloatArray (MutableByteArray ixs@(ix_start, ix_end) arr#) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (ByteArray ixs frozen#, S# s2#) }
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newFloatArray# n# s#)    	   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | True
	    = case (readFloatArray#  from# cur#     s#)  of { StateAndFloat# s1# ele ->
	      case (writeFloatArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeDoubleArray (MutableByteArray ixs@(ix_start, ix_end) arr#) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (ByteArray ixs frozen#, S# s2#) }
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newDoubleArray# n# s#)   	   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | True
	    = case (readDoubleArray#  from# cur#     s#)  of { StateAndDouble# s1# ele ->
	      case (writeDoubleArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

unsafeFreezeArray     :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)  
unsafeFreezeByteArray :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

{-# SPECIALIZE unsafeFreezeByteArray :: MutableByteArray s Int -> ST s (ByteArray Int)
  #-}

unsafeFreezeArray (MutableArray ixs arr#) = ST $ \ (S# s#) ->
    case unsafeFreezeArray# arr# s# of { StateAndArray# s2# frozen# ->
    (Array ixs frozen#, S# s2#) }

unsafeFreezeByteArray (MutableByteArray ixs arr#) = ST $ \ (S# s#) ->
    case unsafeFreezeByteArray# arr# s# of { StateAndByteArray# s2# frozen# ->
    (ByteArray ixs frozen#, S# s2#) }


--This takes a immutable array, and copies it into a mutable array, in a
--hurry.

{-# SPECIALISE thawArray :: Array Int elt -> ST s (MutableArray s Int elt),
			    Array IPr elt -> ST s (MutableArray s IPr elt)
  #-}

thawArray (Array ixs@(ix_start, ix_end) arr#) = ST $ \ (S# s#) ->
    let n# = case (if null (range ixs)
		  then 0
		  else (index ixs ix_end) + 1) of { I# x -> x }
    in
    case thaw arr# n# s# of { StateAndMutableArray# s2# thawed# ->
    (MutableArray ixs thawed#, S# s2#)}
  where
    thaw  :: Array# ele			-- the thing
	    -> Int#			-- size of thing to be thawed
	    -> State# s			-- the Universe and everything
	    -> StateAndMutableArray# s ele

    thaw arr# n# s#
      = case newArray# n# init s#	      of { StateAndMutableArray# s2# newarr1# ->
	copy 0# n# arr# newarr1# s2# }
      where
	init = error "thawArray: element not copied"

	copy :: Int# -> Int#
	     -> Array# ele 
	     -> MutableArray# s ele
	     -> State# s
	     -> StateAndMutableArray# s ele

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableArray# s# to#
	  | True
	    = case indexArray#  from# cur#       of { Lift ele ->
	      case writeArray# to#   cur# ele s# of { s1# ->
	      copy (cur# +# 1#) end# from# to# s1#
	      }}

sameMutableArray     :: MutableArray s ix elt -> MutableArray s ix elt -> Bool
sameMutableByteArray :: MutableByteArray s ix -> MutableByteArray s ix -> Bool

sameMutableArray (MutableArray _ arr1#) (MutableArray _ arr2#)
  = sameMutableArray# arr1# arr2#

sameMutableByteArray (MutableByteArray _ arr1#) (MutableByteArray _ arr2#)
  = sameMutableByteArray# arr1# arr2#

{- =============================================================
** VARIABLES, including MVars and IVars
-}

--************************************************************************
-- Variables

type MutableVar s a = MutableArray s Int a

newVar   :: a -> ST s (MutableVar s a)
readVar  :: MutableVar s a -> ST s a
writeVar :: MutableVar s a -> a -> ST s ()
sameVar  :: MutableVar s a -> MutableVar s a -> Bool

newVar init = ST $ \ (S# s#) ->
    case (newArray# 1# init s#)     of { StateAndMutableArray# s2# arr# ->
    (MutableArray vAR_IXS arr#, S# s2#) }
  where
    vAR_IXS = error "newVar: Shouldn't access `bounds' of a MutableVar\n"

readVar (MutableArray _ var#) = ST $ \ (S# s#) ->
    case readArray# var# 0# s#	of { StateAndPtr# s2# r ->
    (r, S# s2#) }

writeVar (MutableArray _ var#) val = ST $ \ (S# s#) ->
    case writeArray# var# 0# val s# of { s2# ->
    ((), S# s2#) }

sameVar (MutableArray _ var1#) (MutableArray _ var2#)
  = sameMutableArray# var1# var2#

--%************************************************************************
--%*									*
--\subsection[PreludeGlaST-mvars]{M-Structures}
--%*									*
--%************************************************************************
{-
M-Vars are rendezvous points for concurrent threads.  They begin
empty, and any attempt to read an empty M-Var blocks.  When an M-Var
is written, a single blocked thread may be freed.  Reading an M-Var
toggles its state from full back to empty.  Therefore, any value
written to an M-Var may only be read once.  Multiple reads and writes
are allowed, but there must be at least one read between any two
writes.
-}

data MVar a = MVar (SynchVar# RealWorld a)

newEmptyMVar  :: IO (MVar a)

newEmptyMVar = IO $ ST $ \ (S# s#) ->
    case newSynchVar# s# of
        StateAndSynchVar# s2# svar# -> (Right (MVar svar#), S# s2#)

takeMVar :: MVar a -> IO a

takeMVar (MVar mvar#) = IO $ ST $ \ (S# s#) ->
    case takeMVar# mvar# s# of
        StateAndPtr# s2# r -> (Right r, S# s2#)

putMVar  :: MVar a -> a -> IO ()

putMVar (MVar mvar#) x = IO $ ST $ \ (S# s#) ->
    case putMVar# mvar# x s# of
        s2# -> (Right (), S# s2#)

newMVar :: a -> IO (MVar a)

newMVar value =
    newEmptyMVar	>>= \ mvar ->
    putMVar mvar value	>>
    return mvar

readMVar :: MVar a -> IO a

readMVar mvar =
    takeMVar mvar	>>= \ value ->
    putMVar mvar value	>>
    return value

swapMVar :: MVar a -> a -> IO a

swapMVar mvar new =
    takeMVar mvar	>>= \ old ->
    putMVar mvar new	>>
    return old

--%************************************************************************
--%*									*
--\subsection[PreludeGlaST-ivars]{I-Structures}
--%*									*
--%************************************************************************
{-
I-Vars are write-once variables.  They start out empty, and any threads that 
attempt to read them will block until they are filled.  Once they are written, 
any blocked threads are freed, and additional reads are permitted.  Attempting 
to write a value to a full I-Var results in a runtime error.
-}
data IVar a = IVar (SynchVar# RealWorld a)

newIVar :: IO (IVar a)

newIVar = IO $ ST $ \ (S# s#) ->
    case newSynchVar# s# of
        StateAndSynchVar# s2# svar# -> (Right (IVar svar#), S# s2#)

readIVar :: IVar a -> IO a

readIVar (IVar ivar#) = IO $ ST $ \ (S# s#) ->
    case readIVar# ivar# s# of
        StateAndPtr# s2# r -> (Right r, S# s2#)

writeIVar :: IVar a -> a -> IO ()

writeIVar (IVar ivar#) x = IO $ ST $ \ (S# s#) ->
    case writeIVar# ivar# x s# of
        s2# -> (Right (), S# s2#)

{- =============================================================
** THREAD WAITING
-}

{-
@threadDelay@ delays rescheduling of a thread until the indicated
number of microseconds have elapsed.  Generally, the microseconds are
counted by the context switch timer, which ticks in virtual time;
however, when there are no runnable threads, we don't accumulate any
virtual time, so we start ticking in real time.  (The granularity is
the effective resolution of the context switch timer, so it is
affected by the RTS -C option.)

@threadWait@ delays rescheduling of a thread until input on the
specified file descriptor is available for reading (just like select).
-}

threadDelay, threadWaitRead, threadWaitWrite :: Int -> IO ()

threadDelay (I# x#) = IO $ ST $ \ (S# s#) ->
    case delay# x# s# of
      s2# -> (Right (), S# s2#)

threadWaitRead (I# x#) = IO $ ST $ \ (S# s#) -> 
    case waitRead# x# s# of
      s2# -> (Right (), S# s2#)

threadWaitWrite (I# x#) = IO $ ST $ \ (S# s#) ->
    case waitWrite# x# s# of
      s2# -> (Right (), S# s2#)

{- =============================================================
** OTHER SUPPORT FUNCTIONS

   3 flavors, basically: string support, error/trace-ish, and read/show-ish.
-}
seq, par, fork :: Eval a => a -> b -> b

{-# INLINE seq  #-}
{-# INLINE par  #-}
{-# INLINE fork #-}

#ifdef __CONCURRENT_HASKELL__
seq  x y = case (seq#  x) of { 0# -> parError; _ -> y }
par  x y = case (par#  x) of { 0# -> parError; _ -> y }
fork x y = case (fork# x) of { 0# -> parError; _ -> y }
#else
seq  x y = y
par  x y = y
fork x y = y
#endif

-- string-support functions:
---------------------------------------------------------------

--------------------------------------------------------------------------

packStringForC__ :: [Char]          -> ByteArray# -- calls injected by compiler
unpackPS__       :: Addr#           -> [Char] -- calls injected by compiler
unpackPS2__      :: Addr# -> Int#   -> [Char] -- calls injected by compiler
unpackAppendPS__ :: Addr# -> [Char] -> [Char] -- ditto?
unpackFoldrPS__  :: Addr# -> (Char  -> a -> a) -> a -> a -- ditto?

packStringForC__ str = case (GHCps.packString str) of { PS bytes _ _ -> bytes}

unpackPS__ addr -- calls injected by compiler
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | True		   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackAppendPS__ addr rest
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = rest
      | True		   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackFoldrPS__ addr f z 
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = z
      | True		   = C# ch `f` unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackPS2__ addr len -- calls injected by compiler
  -- this one is for literal strings with NULs in them; rare.
  = GHCps.unpackPS (GHCps.packCBytes (I# len) (A# addr))

---------------------------------------------------------------
-- injected literals:
---------------------------------------------------------------
integer_0, integer_1, integer_2, integer_m1 :: Integer

integer_0 = 0; integer_1 = 1; integer_2 = 2; integer_m1 = -1

---------------------------------------------------------------
-- error/trace-ish functions:
---------------------------------------------------------------

errorIO :: PrimIO () -> a

errorIO (ST io)
  = case (errorIO# io) of
      _ -> bottom
  where
    bottom = bottom -- Never evaluated

error__ :: (Addr{-FILE *-} -> PrimIO ()) -> String -> a

error__ msg_hdr s
#ifdef __PARALLEL_HASKELL__
  = errorIO (msg_hdr sTDERR{-msg hdr-}	>>
	     _ccall_ fflush sTDERR	>>
	     fputs sTDERR s		>>
	     _ccall_ fflush sTDERR	>>
	     _ccall_ stg_exit (1::Int)
	    )
#else
  = errorIO (msg_hdr sTDERR{-msg hdr-}	>>
	     _ccall_ fflush sTDERR	>>
	     fputs sTDERR s		>>
	     _ccall_ fflush sTDERR	>>
	     _ccall_ getErrorHandler	>>= \ errorHandler ->
	     if errorHandler == (-1::Int) then
		_ccall_ stg_exit (1::Int)
	     else
		_casm_ ``%r = (StgStablePtr)(%0);'' errorHandler
						>>= \ osptr ->
		_ccall_ decrementErrorCount     >>= \ () ->
		deRefStablePtr osptr            >>= \ oact ->
		oact
	    )
#endif {- !parallel -}
  where
    sTDERR = (``stderr'' :: Addr)

---------------

fputs :: Addr{-FILE*-} -> String -> PrimIO Bool

fputs stream [] = return True

fputs stream (c : cs)
  = _ccall_ stg_putc c stream >> -- stg_putc expands to putc
    fputs stream cs		 -- (just does some casting stream)

---------------------------------------------------------------
-- ******** defn of `_trace' using Glasgow IO *******

{-# GENERATE_SPECS _trace a #-}

trace :: String -> a -> a

trace string expr
  = unsafePerformPrimIO (
	((_ccall_ PreTraceHook sTDERR{-msg-}):: PrimIO ())  >>
	fputs sTDERR string				    >>
	((_ccall_ PostTraceHook sTDERR{-msg-}):: PrimIO ()) >>
	returnPrimIO expr )
  where
    sTDERR = (``stderr'' :: Addr)

---------------------------------------------------------------
-- read/show-ish functions:
---------------------------------------------------------------
{-# GENERATE_SPECS readList__ a #-}
readList__ :: ReadS a -> ReadS [a]

readList__ readx
  = readParen False (\r -> [pr | ("[",s)  <- lex r, pr <- readl s])
  where readl  s = [([],t)   | ("]",t)  <- lex s] ++
		   [(x:xs,u) | (x,t)    <- readx s,
			       (xs,u)   <- readl2 t]
	readl2 s = [([],t)   | ("]",t)  <- lex s] ++
		   [(x:xs,v) | (",",t)  <- lex s,
			       (x,u)    <- readx t,
			       (xs,v)   <- readl2 u]

{-# GENERATE_SPECS showList__ a #-}
showList__ :: (a -> ShowS) ->  [a] -> ShowS

showList__ showx []     = showString "[]"
showList__ showx (x:xs) = showChar '[' . showx x . showl xs
  where
    showl []     = showChar ']'
    showl (x:xs) = showString ", " . showx x . showl xs

showSpace :: ShowS
showSpace = {-showChar ' '-} \ xs -> ' ' : xs

-- ******************************************************************

-- This lexer is not completely faithful to the Haskell lexical syntax.
-- Current limitations:
--    Qualified names are not handled properly
--    A `--' does not terminate a symbol
--    Octal and hexidecimal numerics are not recognized as a single token

lex                   :: ReadS String
lex ""                = [("","")]
lex (c:s) | isSpace c = lex (dropWhile isSpace s)
lex ('\'':s)          = [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
                                              ch /= "'"                ]
lex ('"':s)           = [('"':str, t)      | (str,t) <- lexString s]
                        where
                        lexString ('"':s) = [("\"",s)]
                        lexString s = [(ch++str, u)
                                              | (ch,t)  <- lexStrItem s,
                                                (str,u) <- lexString t  ]

                        lexStrItem ('\\':'&':s) = [("\\&",s)]
                        lexStrItem ('\\':c:s) | isSpace c
                            = [("\\&",t) | '\\':t <- [dropWhile isSpace s]]
                        lexStrItem s            = lexLitChar s

lex (c:s) | isSingle c = [([c],s)]
          | isSym c    = [(c:sym,t)       | (sym,t) <- [span isSym s]]
          | isAlpha c  = [(c:nam,t)       | (nam,t) <- [span isIdChar s]]
          | isDigit c  = [(c:ds++fe,t)    | (ds,s)  <- [span isDigit s],
                                            (fe,t)  <- lexFracExp s     ]
          | otherwise  = []    -- bad character
             where
              isSingle c =  c `elem` ",;()[]{}_`"
              isSym c    =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
              isIdChar c =  isAlphanum c || c `elem` "_'"

              lexFracExp ('.':s) = [('.':ds++e,u) | (ds,t) <- lexDigits s,
                                                    (e,u)  <- lexExp t]
              lexFracExp s       = [("",s)]

              lexExp (e:s) | e `elem` "eE"
                       = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
                                                 (ds,u) <- lexDigits t] ++
                         [(e:ds,t)   | (ds,t) <- lexDigits s]
              lexExp s = [("",s)]

lexDigits               :: ReadS String 
lexDigits               =  nonnull isDigit

nonnull                 :: (Char -> Bool) -> ReadS String
nonnull p s             =  [(cs,t) | (cs@(_:_),t) <- [span p s]]

lexLitChar              :: ReadS String
lexLitChar ('\\':s)     =  [('\\':esc, t) | (esc,t) <- lexEsc s]
        where
        lexEsc (c:s)     | c `elem` "abfnrtv\\\"'" = [([c],s)]
        lexEsc s@(d:_)   | isDigit d               = lexDigits s
        lexEsc _                                   = []
lexLitChar (c:s)        =  [([c],s)]
lexLitChar ""           =  []


match			:: (Eq a) => [a] -> [a] -> ([a],[a])
match (x:xs) (y:ys) | x == y  =  match xs ys
match xs     ys		      =  (xs,ys)

asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI", 
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", 
	    "SP"] 

readLitChar 		:: ReadS Char

readLitChar ('\\':s)	=  readEsc s
	where
	readEsc ('a':s)	 = [('\a',s)]
	readEsc ('b':s)	 = [('\b',s)]
	readEsc ('f':s)	 = [('\f',s)]
	readEsc ('n':s)	 = [('\n',s)]
	readEsc ('r':s)	 = [('\r',s)]
	readEsc ('t':s)	 = [('\t',s)]
	readEsc ('v':s)	 = [('\v',s)]
	readEsc ('\\':s) = [('\\',s)]
	readEsc ('"':s)	 = [('"',s)]
	readEsc ('\'':s) = [('\'',s)]
	readEsc ('^':c:s) | c >= '@' && c <= '_'
			 = [(chr (ord c - ord '@'), s)]
	readEsc s@(d:_) | isDigit d
			 = [(chr n, t) | (n,t) <- readDec s]
	readEsc ('o':s)  = [(chr n, t) | (n,t) <- readOct s]
	readEsc ('x':s)	 = [(chr n, t) | (n,t) <- readHex s]
	readEsc s@(c:_) | isUpper c
			 = let table = ('\DEL', "DEL") : zip ['\NUL'..] asciiTab
			   in case [(c,s') | (c, mne) <- table,
					     ([],s') <- [match mne s]]
			      of (pr:_) -> [pr]
				 []	-> []
	readEsc _	 = []
readLitChar (c:s)	=  [(c,s)]

showLitChar 		   :: Char -> ShowS
showLitChar c | c > '\DEL' =  showChar '\\' . protectEsc isDigit (shows (ord c))
showLitChar '\DEL'	   =  showString "\\DEL"
showLitChar '\\'	   =  showString "\\\\"
showLitChar c | c >= ' '   =  showChar c
showLitChar '\a'	   =  showString "\\a"
showLitChar '\b'	   =  showString "\\b"
showLitChar '\f'	   =  showString "\\f"
showLitChar '\n'	   =  showString "\\n"
showLitChar '\r'	   =  showString "\\r"
showLitChar '\t'	   =  showString "\\t"
showLitChar '\v'	   =  showString "\\v"
showLitChar '\SO'	   =  protectEsc (== 'H') (showString "\\SO")
showLitChar c		   =  showString ('\\' : asciiTab!!ord c)

protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s

-- ******************************************************************

{-# GENERATE_SPECS readDec a{Int#,Int,Integer} #-}
readDec :: (Integral a) => ReadS a
readDec = readInt 10 isDigit (\d -> ord d - ord_0)

{-# GENERATE_SPECS readOct a{Int#,Int,Integer} #-}
readOct :: (Integral a) => ReadS a
readOct = readInt 8 isOctDigit (\d -> ord d - ord_0)

{-# GENERATE_SPECS readHex a{Int#,Int,Integer} #-}
readHex :: (Integral a) => ReadS a
readHex = readInt 16 isHexDigit hex
	    where hex d = ord d - (if isDigit d then ord_0
				   else ord (if isUpper d then 'A' else 'a') - 10)

{-# GENERATE_SPECS readInt a{Int#,Int,Integer} #-}
readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
    [(foldl1 (\n d -> n * radix + d) (map (fromInt . digToInt) ds), r)
	| (ds,r) <- nonnull isDig s ]

showInt n r
  = case quotRem n 10 of		     { (n', d) ->
    case (chr (ord_0 + fromIntegral d)) of { C# c# -> -- stricter than necessary
    let
	r' = C# c# : r
    in
    if n' == 0 then r' else showInt n' r'
    }}

-- ******************************************************************

{-# GENERATE_SPECS readSigned a{Int#,Double#,Int,Integer,Double} #-}
readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
		     where read' r  = read'' r ++
				      [(-x,t) | ("-",s) <- lex r,
						(x,t)   <- read'' s]
			   read'' r = [(n,s)  | (str,s) <- lex r,
		      				(n,"")  <- readPos str]


{-# SPECIALIZE showSigned :: (Int     -> ShowS) -> Int -> Int     -> ShowS = showSigned_Int,
			     (Integer -> ShowS) -> Int -> Integer -> ShowS = showSigned_Integer #-}
{-# GENERATE_SPECS showSigned a{Double#,Double} #-}
showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x = if x < 0 then showParen (p > 6)
						 (showChar '-' . showPos (-x))
				  else showPos x

showSigned_Int :: (Int -> ShowS) -> Int -> Int -> ShowS
showSigned_Int _ p n r
  = -- from HBC version; support code follows
    if n < 0 && p > 6 then '(':itos n++(')':r) else itos n ++ r

showSigned_Integer :: (Integer -> ShowS) -> Int -> Integer -> ShowS
showSigned_Integer _ p n r
  = -- from HBC version; support code follows
    if n < 0 && p > 6 then '(':jtos n++(')':r) else jtos n ++ r

-- ******************************************************************

itos# :: Int# -> String
itos# n =
    if n `ltInt#` 0# then
	if negateInt# n `ltInt#` 0# then
	    -- n is minInt, a difficult number
	    itos# (n `quotInt#` 10#) ++ itos' (negateInt# (n `remInt#` 10#)) []
	else
	    '-':itos' (negateInt# n) []
    else 
	itos' n []
  where
    itos' :: Int# -> String -> String
    itos' n cs = 
	if n `ltInt#` 10# then
	    C# (chr# (n `plusInt#` ord# '0'#)) : cs
	else 
	    itos' (n `quotInt#` 10#) (C# (chr# (n `remInt#` 10# `plusInt#` ord# '0'#)) : cs)

itos :: Int -> String
itos (I# n) = itos# n

jtos :: Integer -> String
jtos n 
  = if n < 0 then
        '-' : jtos' (-n) []
    else 
	jtos' n []

jtos' :: Integer -> String -> String
jtos' n cs
  = if n < 10 then
	chr (fromInteger (n + ord_0)) : cs
    else 
	jtos' (n `quot` 10) (chr (fromInteger (n `rem` 10 + ord_0)) : cs)

chr = (toEnum   :: Int  -> Char)
ord = (fromEnum :: Char -> Int)

ord_0 :: Num a => a
ord_0 = fromInt (ord '0')

-- ******************************************************************

-- The functions readFloat and showFloat below use rational arithmetic
-- to insure correct conversion between the floating-point radix and
-- decimal.  It is often possible to use a higher-precision floating-
-- point type to obtain the same results.

{-# GENERATE_SPECS readFloat a{Double#,Double} #-}
readFloat :: (RealFloat a) => ReadS a
readFloat r = [(fromRational x, t) | (x, t) <- readRational r]

readRational :: ReadS Rational -- NB: doesn't handle leading "-"

readRational r
  = [ ( (n%1)*10^^(k-d), t ) | (n,d,s) <- readFix r,
			       (k,t)   <- readExp s]
              where readFix r = [(read (ds++ds'), length ds', t)
					| (ds,'.':s) <- lexDigits r,
					  (ds',t)    <- lexDigits s ]

		    readExp (e:s) | e `elem` "eE" = readExp' s
                    readExp s			  = [(0,s)]

                    readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
                    readExp' ('+':s) = readDec s
                    readExp' s	     = readDec s

readRational__ :: String -> Rational -- we export this one (non-std)
				    -- NB: *does* handle a leading "-"
readRational__ top_s
  = case top_s of
      '-' : xs -> - (read_me xs)
      xs       -> read_me xs
  where
    read_me s
      = case [x | (x,t) <- readRational s, ("","") <- lex t] of
	  [x] -> x
	  []  -> error ("readRational__: no parse:"        ++ top_s)
	  _   -> error ("readRational__: ambiguous parse:" ++ top_s)

-- The number of decimal digits m below is chosen to guarantee 
-- read (show x) == x.  See
--	Matula, D. W.  A formalization of floating-point numeric base
--	conversion.  IEEE Transactions on Computers C-19, 8 (1970 August),
--	681-692.
 
zeros = repeat '0'

{-# GENERATE_SPECS showFloat a{Double#,Double} #-}
showFloat:: (RealFloat a) => a -> ShowS
showFloat x =
    if x == 0 then showString ("0." ++ take (m-1) zeros)
	      else if e >= m-1 || e < 0 then showSci else showFix
    where
    showFix	= showString whole . showChar '.' . showString frac
		  where (whole,frac) = splitAt (e+1) (show sig)
    showSci	= showChar d . showChar '.' . showString frac
		      . showChar 'e' . shows e
    		  where (d:frac) = show sig
    (m, sig, e) = if b == 10 then (w,  	s,   n+w-1)
		  	     else (m', sig', e'   )
    m'		= ceiling
		      ((fromInt w * log (fromInteger b)) / log 10 :: Double)
		  + 1
    (sig', e')	= if	  sig1 >= 10^m'     then (round (t/10), e1+1)
		  else if sig1 <  10^(m'-1) then (round (t*10), e1-1)
		  			    else (sig1,		 e1  )
    sig1	= round t
    t		= s%1 * (b%1)^^n * 10^^(m'-e1-1)
    e1		= floor (logBase 10 x)
    (s, n)	= decodeFloat x
    b		= floatRadix x
    w		= floatDigits x

---------------------------------------------------------
-- definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

plusInt	(I# x) (I# y) = I# (plusInt# x y)
minusInt(I# x) (I# y) = I# (minusInt# x y)
timesInt(I# x) (I# y) = I# (timesInt# x y)
quotInt	(I# x) (I# y) = I# (quotInt# x y)
remInt	(I# x) (I# y) = I# (remInt# x y)
negateInt (I# x)      = I# (negateInt# x)
gtInt	(I# x) (I# y) = gtInt# x y
geInt	(I# x) (I# y) = geInt# x y
eqInt	(I# x) (I# y) = eqInt# x y
neInt	(I# x) (I# y) = neInt# x y
ltInt	(I# x) (I# y) = ltInt# x y
leInt	(I# x) (I# y) = leInt# x y

-- definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

plusFloat   (F# x) (F# y) = F# (plusFloat# x y)
minusFloat  (F# x) (F# y) = F# (minusFloat# x y)
timesFloat  (F# x) (F# y) = F# (timesFloat# x y)
divideFloat (F# x) (F# y) = F# (divideFloat# x y)
negateFloat (F# x)        = F# (negateFloat# x)

gtFloat	    (F# x) (F# y) = gtFloat# x y
geFloat	    (F# x) (F# y) = geFloat# x y
eqFloat	    (F# x) (F# y) = eqFloat# x y
neFloat	    (F# x) (F# y) = neFloat# x y
ltFloat	    (F# x) (F# y) = ltFloat# x y
leFloat	    (F# x) (F# y) = leFloat# x y

float2Int   (F# x) = I# (float2Int# x)
int2Float   (I# x) = F# (int2Float# x)

expFloat    (F# x) = F# (expFloat# x)
logFloat    (F# x) = F# (logFloat# x)
sqrtFloat   (F# x) = F# (sqrtFloat# x)
sinFloat    (F# x) = F# (sinFloat# x)
cosFloat    (F# x) = F# (cosFloat# x)
tanFloat    (F# x) = F# (tanFloat# x)
asinFloat   (F# x) = F# (asinFloat# x)
acosFloat   (F# x) = F# (acosFloat# x)
atanFloat   (F# x) = F# (atanFloat# x)
sinhFloat   (F# x) = F# (sinhFloat# x)
coshFloat   (F# x) = F# (coshFloat# x)
tanhFloat   (F# x) = F# (tanhFloat# x)

powerFloat  (F# x) (F# y) = F# (powerFloat# x y)

-- definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

plusDouble   (D# x) (D# y) = D# (plusDouble# x y)
minusDouble  (D# x) (D# y) = D# (minusDouble# x y)
timesDouble  (D# x) (D# y) = D# (timesDouble# x y)
divideDouble (D# x) (D# y) = D# (divideDouble# x y)
negateDouble (D# x)        = D# (negateDouble# x)

gtDouble    (D# x) (D# y) = gtDouble# x y
geDouble    (D# x) (D# y) = geDouble# x y
eqDouble    (D# x) (D# y) = eqDouble# x y
neDouble    (D# x) (D# y) = neDouble# x y
ltDouble    (D# x) (D# y) = ltDouble# x y
leDouble    (D# x) (D# y) = leDouble# x y

double2Int   (D# x) = I# (double2Int#   x)
int2Double   (I# x) = D# (int2Double#   x)
double2Float (D# x) = F# (double2Float# x)
float2Double (F# x) = D# (float2Double# x)

expDouble    (D# x) = D# (expDouble# x)
logDouble    (D# x) = D# (logDouble# x)
sqrtDouble   (D# x) = D# (sqrtDouble# x)
sinDouble    (D# x) = D# (sinDouble# x)
cosDouble    (D# x) = D# (cosDouble# x)
tanDouble    (D# x) = D# (tanDouble# x)
asinDouble   (D# x) = D# (asinDouble# x)
acosDouble   (D# x) = D# (acosDouble# x)
atanDouble   (D# x) = D# (atanDouble# x)
sinhDouble   (D# x) = D# (sinhDouble# x)
coshDouble   (D# x) = D# (coshDouble# x)
tanhDouble   (D# x) = D# (tanhDouble# x)

powerDouble  (D# x) (D# y) = D# (powerDouble# x y)

---------------------------------------------------------
{-
[In response to a request by simonpj, Joe Fasel writes:]

A quite reasonable request!  This code was added to the Prelude just
before the 1.2 release, when Lennart, working with an early version
of hbi, noticed that (read . show) was not the identity for
floating-point numbers.	 (There was a one-bit error about half the time.)
The original version of the conversion function was in fact simply
a floating-point divide, as you suggest above.	The new version is,
I grant you, somewhat denser.

How's this?

Joe
-}

{-# GENERATE_SPECS fromRational__ a{Double#,Double} #-}
fromRational__ :: (RealFloat a) => Rational -> a
fromRational__ x = x'
	where x' = f e

--		If the exponent of the nearest floating-point number to x 
--		is e, then the significand is the integer nearest xb^(-e),
--		where b is the floating-point radix.  We start with a good
--		guess for e, and if it is correct, the exponent of the
--		floating-point number we construct will again be e.  If
--		not, one more iteration is needed.

	      f e   = if e' == e then y else f e'
		      where y	   = encodeFloat (round (x * (1 % b)^^e)) e
			    (_,e') = decodeFloat y
	      b	    = floatRadix x'

--		We obtain a trial exponent by doing a floating-point
--		division of x's numerator by its denominator.  The
--		result of this division may not itself be the ultimate
--		result, because of an accumulation of three rounding
--		errors.

	      (s,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
					/ fromInteger (denominator x))

-------------------------------------------------------------------------
-- from/by Lennart, 94/09/26

-- Convert a Rational to a string that looks like a floating point number,
-- but without converting to any floating type (because of the possible overflow).
showRational :: Int -> Rational -> String
showRational n r =
    if r == 0 then
    	"0.0"
    else
	let (r', e) = normalize r
	in  prR n r' e

startExpExp = 4 :: Int

-- make sure 1 <= r < 10
normalize :: Rational -> (Rational, Int)
normalize r = if r < 1 then
		  case norm startExpExp (1 / r) 0 of (r', e) -> (10 / r', -e-1)
	      else
		  norm startExpExp r 0
	where norm :: Int -> Rational -> Int -> (Rational, Int)
	      -- Invariant: r*10^e == original r
	      norm 0  r e = (r, e)
	      norm ee r e =
		let n = 10^ee
		    tn = 10^n
		in  if r >= tn then norm ee (r/tn) (e+n) else norm (ee-1) r e

drop0 "" = ""
drop0 (c:cs) = c : reverse (dropWhile (=='0') (reverse cs))

prR :: Int -> Rational -> Int -> String
prR n r e | r <  1  = prR n (r*10) (e-1)		-- final adjustment
prR n r e | r >= 10 = prR n (r/10) (e+1)
prR n r e0 =
	let s = show ((round (r * 10^n))::Integer)
	    e = e0+1
	in  if e > 0 && e < 8 then
		take e s ++ "." ++ drop0 (drop e s)
	    else if e <= 0 && e > -3 then
	        "0." ++ take (-e) (repeat '0') ++ drop0 s
	    else
	        head s : "."++ drop0 (tail s) ++ "e" ++ show e0
