{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.CString
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC C strings definitions (previously in GHC.Base).
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.CString (
        unpackCString#, unpackAppendCString#, unpackFoldrCString#,
        unpackCStringUtf8#, unpackNBytes#
    ) where

import GHC.Types
import GHC.Prim
import GHC.PrimWrappers

-----------------------------------------------------------------------------
-- Unpacking C strings}
-----------------------------------------------------------------------------

-- This code is needed for virtually all programs, since it's used for
-- unpacking the strings of error messages.

-- Used to be in GHC.Base, but was moved to ghc-prim because the new generics
-- stuff uses Strings in the representation, so to give representations for
-- ghc-prim types we need unpackCString#

unpackCString# :: Addr# -> [Char]
{-# NOINLINE unpackCString# #-}
    -- There's really no point in inlining this, ever, as the loop doesn't
    -- specialise in an interesting But it's pretty small, so there's a danger
    -- that it'll be inlined at every literal, which is a waste
unpackCString# addr
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | True               = C# ch : unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh

unpackAppendCString# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString# #-}
     -- See the NOINLINE note on unpackCString#
unpackAppendCString# addr rest
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = rest
      | True               = C# ch : unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh

unpackFoldrCString# :: Addr# -> (Char  -> a -> a) -> a -> a

-- Usually the unpack-list rule turns unpackFoldrCString# into unpackCString#

-- It also has a BuiltInRule in PrelRules.lhs:
--      unpackFoldrCString# "foo" c (unpackFoldrCString# "baz" c n)
--        =  unpackFoldrCString# "foobaz" c n

{-# NOINLINE unpackFoldrCString# #-}
-- At one stage I had NOINLINE [0] on the grounds that, unlike
-- unpackCString#, there *is* some point in inlining
-- unpackFoldrCString#, because we get better code for the
-- higher-order function call.  BUT there may be a lot of
-- literal strings, and making a separate 'unpack' loop for
-- each is highly gratuitous.  See nofib/real/anna/PrettyPrint.

unpackFoldrCString# addr f z
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = z
      | True               = C# ch `f` unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh

unpackCStringUtf8# :: Addr# -> [Char]
unpackCStringUtf8# addr
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'#   = []
      | ch `leChar#` '\x7F'# = C# ch : unpack (nh +# 1#)
      | ch `leChar#` '\xDF'# =
          C# (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#))) :
          unpack (nh +# 2#)
      | ch `leChar#` '\xEF'# =
          C# (chr# (((ord# ch                                  -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# addr (nh +# 2#)) -# 0x80#))) :
          unpack (nh +# 3#)
      | True                 =
          C# (chr# (((ord# ch                                  -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharOffAddr# addr (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# addr (nh +# 3#)) -# 0x80#))) :
          unpack (nh +# 4#)
      where
        !ch = indexCharOffAddr# addr nh

unpackNBytes# :: Addr# -> Int# -> [Char]
unpackNBytes# _addr 0#   = []
unpackNBytes#  addr len# = unpack [] (len# -# 1#)
    where
     unpack acc i#
      | i# <# 0#  = acc
      | True      =
         case indexCharOffAddr# addr i# of
            ch -> unpack (C# ch : acc) (i# -# 1#)

