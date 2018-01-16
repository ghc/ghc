{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
-- | During testing, we compare the output of each invocation of the lifted
--   combinators in "Data.Array.Parallel.PArray" with the reference implementations. 
--
--   This module helps convert the to and from the array representation
--   used by the reference implementation.

--   TODO: we could use this to trace the lengths of the vectors being used, 
--         as well as the types that each opeartor is being called at.
--
module Data.Array.Parallel.PArray.Reference.Convert
        ( Similar(..), PprPhysical1 (..)
        , withRef1, withRef2
        , toRef1,   toRef2,   toRef3)
where
import Data.Array.Parallel.Pretty
import qualified Data.Array.Parallel.Array      as A
import qualified Data.Vector                    as V
import Data.Vector                              (Vector)
import Prelude hiding (length)
import System.IO
import System.IO.Unsafe
import Control.Monad

-- Config ---------------------------------------------------------------------
debugLiftedTrace        :: Bool
debugLiftedTrace        = False

debugLiftedCompare      :: Bool
debugLiftedCompare      = False

class Similar a where
 similar :: a -> a -> Bool

class PprPhysical1 a where
 pprp1  :: a -> Doc

 pprp1v :: Vector a -> Doc
 pprp1v vec
        = brackets 
        $ hcat
        $ punctuate (text ", ") 
        $ V.toList $ V.map pprp1 vec
        

-- withRef --------------------------------------------------------------------
-- | Compare the result of some array operator against a reference.

--  Careful:
--   * We don't want to inline the whole body of this function into
--     every use site, or we'll get code explosion. When debugging is off we
--     want this wrapper to be inlined and eliminated as cheaply as possible.
--   * We also do this with 'unsafePerformIO' instead of trace, because
--     with trace, if the computation contructing the string throws an exception
--     then we get no output. For debugging we want to see what function was
--     entered before we try to print the result (which might be badly formed),
--
withRef1 :: ( A.Array r a
            , A.Array c a, PprPhysical1 (c a)
            , Similar a,   PprPhysical1 a)
         => String              -- name of operator
         -> r a                 -- result using reference implementation
         -> c a                 -- result using vseg implementation
         -> c a

{-# INLINE withRef1 #-}
withRef1 name arrRef arrImpl
 = if debugLiftedCompare || debugLiftedTrace
        then withRef1' name arrRef arrImpl
        else arrImpl
        
{-# NOINLINE withRef1' #-}
withRef1' name arrRef arrImpl
 = unsafePerformIO
 $ do   when debugLiftedTrace
         $ do putStrLn  $ "* " ++ name
              putStrLn  $ render (nest 4 $ pprp1 arrImpl)
              hFlush stdout
        
        when ( debugLiftedCompare 
             && or [ not $ A.valid arrImpl
                   , not $ A.length arrRef == A.length arrImpl
                   , not $ V.and $ V.zipWith similar
                                (A.toVectors1 arrRef)
                                (A.toVectors1 arrImpl)])
         $ error $ render $ vcat
                [ text "withRef1: failure " <> text name
                , nest 4 $ pprp1v $ A.toVectors1 arrRef
                , nest 4 $ pprp1  $ arrImpl ]

        return arrImpl


-- | Compare the nested result of some array operator against a reference.
withRef2 :: ( A.Array r (r a)
            , A.Array r a
            , A.Array c (c a), PprPhysical1 (c (c a))
            , A.Array c a,     PprPhysical1 (c a)
            , Similar a,       PprPhysical1 a)
         => String      -- name of operator.
         -> r (r a)     -- result using reference implementaiton.
         -> c (c a)     -- result using vseg implementation.
         -> c (c a)

{-# INLINE withRef2 #-}
withRef2 name arrRef arrImpl
 = if debugLiftedCompare || debugLiftedTrace
         then withRef2' name arrRef arrImpl
         else arrImpl

{-# NOINLINE withRef2' #-}
withRef2' name arrRef arrImpl
 = unsafePerformIO
 $ do   when debugLiftedTrace
         $ do putStrLn  $ "* " ++ name
              putStrLn  $ render (nest 4 $ pprp1 arrImpl)
              hFlush stdout

        when ( debugLiftedCompare
             && or [ not $ A.valid arrImpl
                   , not $ A.length arrRef == A.length arrImpl
                   , not $ V.and $ V.zipWith 
                                (\xs ys -> V.and $ V.zipWith similar xs ys)
                                (A.toVectors2 arrRef)
                                (A.toVectors2 arrImpl) ])
         $ error $ render $ vcat
                [ text "withRef2: failure " <> text name
                , nest 4 $ pprp1 arrImpl ]

        return arrImpl


-- toRef ----------------------------------------------------------------------
-- | Convert an array to the reference version.
toRef1  :: ( A.Array c a
           , A.Array r a)
        => c a -> r a

toRef1  = A.fromVectors1 . A.toVectors1
{-# NOINLINE toRef1 #-}
--  NOINLINE because it's only for debugging.


-- | Convert a nested array to the reference version.
toRef2 :: ( A.Array c (c a)
          , A.Array c a
          , A.Array r (r a)
          , A.Array r a)
       => c (c a)
       -> r (r a)

toRef2  = A.fromVectors2 . A.toVectors2
{-# NOINLINE toRef2 #-}
--  NOINLINE because it's only for debugging.


-- | Convert a doubly nested array to the reference version.
toRef3 :: ( A.Array c (c (c a))
          , A.Array c (c a)
          , A.Array c a
          , A.Array r (r (r a))
          , A.Array r (r a)
          , A.Array r a)
       => c (c (c a))
       -> r (r (r a))

toRef3  = A.fromVectors3 . A.toVectors3
{-# NOINLINE toRef3 #-}
--  NOINLINE because it's only for debugging.
