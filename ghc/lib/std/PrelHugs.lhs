%
% (c) The AQUA Project, Glasgow University, 1994-2000
%

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelHugs (
   hugsprimPmInt,
   hugsprimPmInteger,
   hugsprimPmDouble,
   hugsprimPmSub,
   hugsprimPmFromInteger,
   hugsprimPmSubtract,
   hugsprimPmLe,
   hugsprimRunIO_toplevel,
   hugsprimEqChar,
   fromDouble,
   hugsprimMkIO,
   hugsprimCreateAdjThunk,
   hugsprimUnpackString,
   hugsprimPmFail,
   hugsprimCompAux,
   hugsprimError,
   hugsprimShowField,
   hugsprimReadField
)
where
import PrelGHC
import PrelBase
import PrelNum
import PrelReal(Integral)
import Prelude(fromIntegral)
import IO(putStr,hFlush,stdout,stderr)
import PrelException(catch,catchException)
import PrelIOBase(IO(..),unsafePerformIO)
import PrelShow(show,shows,showString,showChar,Show,ShowS)
import PrelRead(Read,ReadS,lex,reads)
import PrelFloat(Double)
import PrelReal(Fractional,fromRational,toRational)
import PrelAddr(Addr)
import PrelErr(error)
import PrelPack(unpackCString)

-- Stuff needed by Hugs for desugaring.  Do not mess with these!
-- They need to correspond exactly to versions written in 
-- the Hugs standalone Prelude.

-- hugs doesn't know about RealWorld and so throws this
-- away if the original type signature is used
-- hugsprimMkIO :: (RealWorld -> (a,RealWorld)) -> IO a
--
-- The first arg is an IO value created by Hugs, without the
-- newtype ST wrapper.  What we do here place a wrapper around
-- it, so that it can be called from GHC-land, which uses a
-- different IO representation.
--
-- This is all very delicate and relies crucially on the non-inlined
-- connectWorlds fn to create an artificial dependency of the hugs_ioaction
-- on the grealworld.  That's needed to stop the simplifier floating
-- the case outside of the \ grealworld.
hugsprimMkIO :: (rw -> (a,rw)) -> IO a
hugsprimMkIO hugs_ioaction 
   = IO ( \ grealworld -> case hugs_ioaction 
                                  (connectWorlds grealworld) of
                             (res, hrealworld') -> (# grealworld, res #)
        )

{-# NOINLINE connectWorlds #-}
connectWorlds :: State# RealWorld -> a    -- really, -> Hugs' RealWorld
connectWorlds hrealworld
   = error "connectWorlds: hugs entered the RealWorld"




hugsprimCreateAdjThunk :: (a -> b) -> String -> Char -> IO Addr
hugsprimCreateAdjThunk fun typestr callconv
   = error "hugsprimCreateAdjThunk in combined mode: unimplemented"

fromDouble :: Fractional a => Double -> a
fromDouble n = fromRational (toRational n)

hugsprimEqChar       :: Char -> Char -> Bool
hugsprimEqChar c1 c2  = c1 == c2

hugsprimPmInt        :: Num a => Int -> a -> Bool
hugsprimPmInt n x     = fromInt n == x

hugsprimPmInteger    :: Num a => Integer -> a -> Bool
hugsprimPmInteger n x = fromInteger n == x

hugsprimPmDouble     :: Fractional a => Double -> a -> Bool
hugsprimPmDouble n x  = fromDouble n == x

-- The following primitives are only needed if (n+k) patterns are enabled:
hugsprimPmSub           :: Integral a => Int -> a -> a
hugsprimPmSub n x        = x - fromInt n

hugsprimPmFromInteger   :: Integral a => Integer -> a
hugsprimPmFromInteger    = fromIntegral

hugsprimPmSubtract      :: Integral a => a -> a -> a
hugsprimPmSubtract x y   = x - y

hugsprimPmLe            :: Integral a => a -> a -> Bool
hugsprimPmLe x y         = x <= y

hugsprimUnpackString :: Addr -> String
hugsprimUnpackString a = unpackCString a

-- ToDo: make the message more informative.
hugsprimPmFail       :: a
hugsprimPmFail        = error "Pattern Match Failure"

hugsprimCompAux      :: Ord a => a -> a -> Ordering -> Ordering
hugsprimCompAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT

hugsprimError        :: String -> a
hugsprimError s       = error s

hugsprimShowField    :: Show a => String -> a -> ShowS
hugsprimShowField m v = showString m . showChar '=' . shows v

hugsprimReadField    :: Read a => String -> ReadS a
hugsprimReadField m s0 = [ r | (t,  s1) <- lex s0, t == m,
                               ("=",s2) <- lex s1,
                               r        <- reads s2 ]


-- used when Hugs invokes top level function
{-
hugsprimRunIO_toplevel :: IO a -> ()
hugsprimRunIO_toplevel m
   = protect 5 (fst (unST composite_action realWorld))
     where
        composite_action
           = do writeIORef prelCleanupAfterRunAction Nothing
                m 
                cleanup_handles <- readIORef prelCleanupAfterRunAction
                case cleanup_handles of
                   Nothing -> return ()
                   Just xx -> xx

        realWorld = error "primRunIO: entered the RealWorld"
        protect :: Int -> () -> ()
        protect 0 comp
           = comp
        protect n comp
           = primCatch (protect (n-1) comp)
                       (\e -> fst (unST (putStr (show e ++ "\n")) realWorld))
-}

hugsprimRunIO_toplevel :: IO a -> ()
hugsprimRunIO_toplevel m
    = unsafePerformIO (
         catchException (m >> hFlush stderr >> hFlush stdout)
                        (\e -> putStr ("error: " ++ show e ++ "\n"))
      )
\end{code}