{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Data.USSegd.DT
where
import Data.Array.Parallel.Unlifted.Distributed.Data.USegd              ()
import Data.Array.Parallel.Unlifted.Distributed.Data.Vector             ()
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DT
import Data.Array.Parallel.Unlifted.Sequential.USSegd                   (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd                    (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   (Vector)
import Data.Array.Parallel.Pretty
import Control.Monad
import Prelude                                                          as P
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd         as USSegd

instance DT USSegd where
  data Dist USSegd   
        = DUSSegd  !(Dist (Vector Int))         -- segment starts
                   !(Dist (Vector Int))         -- segment sources
                   !(Dist USegd)                -- distributed usegd

  data MDist USSegd s 
        = MDUSSegd !(MDist (Vector Int) s)      -- segment starts
                   !(MDist (Vector Int) s)      -- segment sources
                   !(MDist USegd        s)      -- distributed usegd

  indexD str (DUSSegd starts sources usegds) i
   = USSegd.mkUSSegd
        (indexD (str ++ "/indexD[USSegd]") starts i)
        (indexD (str ++ "/indexD[USSegd]") sources i)
        (indexD (str ++ "/indexD[USSegd]") usegds i)
  {-# INLINE_DIST indexD #-}

  newMD g
   = liftM3 MDUSSegd (newMD g) (newMD g) (newMD g)
  {-# INLINE_DIST newMD #-}

  readMD (MDUSSegd starts sources usegds) i
   = liftM3 USSegd.mkUSSegd (readMD starts i) (readMD sources i) (readMD usegds i)
  {-# INLINE_DIST readMD #-}

  writeMD (MDUSSegd starts sources usegds) i ussegd
   = do writeMD starts  i (USSegd.takeStarts  ussegd)
        writeMD sources i (USSegd.takeSources ussegd)
        writeMD usegds  i (USSegd.takeUSegd   ussegd)
  {-# INLINE_DIST writeMD #-}

  unsafeFreezeMD (MDUSSegd starts sources usegds)
   = liftM3 DUSSegd (unsafeFreezeMD starts)
                    (unsafeFreezeMD sources)
                    (unsafeFreezeMD usegds)
  {-# INLINE_DIST unsafeFreezeMD #-}

  deepSeqD ussegd z
   = deepSeqD (USSegd.takeStarts  ussegd)
   $ deepSeqD (USSegd.takeSources ussegd)
   $ deepSeqD (USSegd.takeUSegd   ussegd) z
  {-# INLINE_DIST deepSeqD #-}

  sizeD  (DUSSegd  _ _ usegd) 
   = sizeD usegd
  {-# INLINE_DIST sizeD #-}

  sizeMD (MDUSSegd _ _ usegd) 
   = sizeMD usegd
  {-# INLINE_DIST sizeMD #-}

  measureD ussegd 
   = "USSegd "  P.++ show (USSegd.takeStarts    ussegd)
   P.++ " "     P.++ show (USSegd.takeSources   ussegd)
   P.++ " "     P.++ measureD (USSegd.takeUSegd ussegd)
  {-# NOINLINE measureD #-}
  --  NOINLINE because this is only used for debugging.


instance PprPhysical (Dist USSegd) where
 pprp (DUSSegd starts sources usegds)
  =  text "DUSSegd"
  $$ (nest 7 $ vcat
        [ text "starts:  " <+> pprp starts
        , text "sources: " <+> pprp sources
        , text "usegds:  " <+> pprp usegds])
 {-# NOINLINE pprp #-}
 --  NOINLINE because this is only used for debugging.

