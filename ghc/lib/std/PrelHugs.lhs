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
   hugsprimUnpackString
)
where
import PrelGHC
import PrelBase
import PrelNum
import PrelReal(Integral)
import Prelude(fromIntegral)
import IO(putStr,hFlush,stdout,stderr)
import PrelException(catch)
import PrelIOBase(IO,unsafePerformIO)
import PrelShow(show)
import PrelFloat(Double)
import PrelReal(Fractional,fromRational,toRational)
import PrelAddr(Addr)
import PrelErr(error)
import PrelPack(unpackCString)

-- Stuff needed by Hugs for desugaring.  Do not mess with these!
-- They need to correspond exactly to versions written in 
-- the Hugs standalone Prelude.

--hugs doesn't know about RealWorld and so throws this
--away if the original type signature is used
--hugsprimMkIO :: (RealWorld -> (a,RealWorld)) -> IO a
hugsprimMkIO :: (rw -> (a,rw)) -> IO a
hugsprimMkIO
   = error "hugsprimMkIO in combined mode: unimplemented"

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
        catch (m >> hFlush stderr >> hFlush stdout)
              (\e -> putStr (show e ++ "\n"))
     )


\end{code}