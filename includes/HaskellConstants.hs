
import Data.Word
import Data.Int

-- This magical #include brings in all the everybody-knows-these magic
-- constants unfortunately, we need to be *explicit* about which one
-- we want; if we just hope a -I... will get the right one, we could
-- be in trouble.

{-
Pull in the autoconf defines (HAVE_FOO), but don't include
ghcconfig.h, because that will include ghcplatform.h which has the
wrong platform settings for the compiler (it has the platform
settings for the target plat instead).
-}
#include "../includes/ghcautoconf.h"

#include "stg/HaskellMachRegs.h"

#include "rts/Constants.h"
#include "MachDeps.h"
#include "../includes/dist-derivedconstants/header/DerivedConstants.h"

-- import Util

-- All pretty arbitrary:

mAX_TUPLE_SIZE :: Int
mAX_TUPLE_SIZE = 62 -- Should really match the number
                    -- of decls in Data.Tuple

mAX_CONTEXT_REDUCTION_DEPTH :: Int
mAX_CONTEXT_REDUCTION_DEPTH = 200
  -- Increase to 200; see Trac #5395

wORD64_SIZE :: Int
wORD64_SIZE = 8

-- Define a fixed-range integral type equivalent to the target Int/Word

#if SIZEOF_HSWORD == 4
type TargetInt  = Int32
type TargetWord = Word32
#elif SIZEOF_HSWORD == 8
type TargetInt  = Int64
type TargetWord = Word64
#else
#error unknown SIZEOF_HSWORD
#endif

tARGET_MAX_CHAR :: Int
tARGET_MAX_CHAR = 0x10ffff

