{-# LANGUAGE MagicHash, NoImplicitPrelude, BangPatterns #-}
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

-----------------------------------------------------------------------------
-- Unpacking C strings
-----------------------------------------------------------------------------

-- This code is needed for virtually all programs, since it's used for
-- unpacking the strings of error messages.

-- Used to be in GHC.Base, but was moved to ghc-prim because the new generics
-- stuff uses Strings in the representation, so to give representations for
-- ghc-prim types we need unpackCString#

{-
Note [Inlining unpackCString#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's really no point in ever inlining things like unpackCString# as the loop
doesn't specialise in an interesting way and we can't deforest the list
constructors (we'd want to use unpackFoldrCString# for this). Moreover, it's
pretty small, so there's a danger that it'll be inlined at every literal, which
is a waste.

Moreover, inlining early may interfere with a variety of rules that are supposed
to match unpackCString#,

 * BuiltInRules in PrelRules.hs; e.g.
       eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2)
          = s1 == s2

 * unpacking rules; e.g. in GHC.Base,
       unpackCString# a
          = build (unpackFoldrCString# a)

 * stream fusion rules; e.g. in the `text` library,
       unstream (S.map safe (S.streamList (GHC.unpackCString# a)))
          = unpackCString# a
-}

unpackCString# :: Addr# -> [Char]
{-# NOINLINE unpackCString# #-}
unpackCString# addr
  = unpack 0#
  where
    unpack nh
      | isTrue# (ch `eqChar#` '\0'#) = []
      | True                         = C# ch : unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh

unpackAppendCString# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString# #-}
     -- See the NOINLINE note on unpackCString#
unpackAppendCString# addr rest
  = unpack 0#
  where
    unpack nh
      | isTrue# (ch `eqChar#` '\0'#) = rest
      | True                         = C# ch : unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh

unpackFoldrCString# :: Addr# -> (Char  -> a -> a) -> a -> a

-- Usually the unpack-list rule turns unpackFoldrCString# into unpackCString#

-- It also has a BuiltInRule in PrelRules.hs:
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
      | isTrue# (ch `eqChar#` '\0'#) = z
      | True                         = C# ch `f` unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh

-- There's really no point in inlining this for the same reasons as
-- unpackCString. See Note [Inlining unpackCString#] above for details.
unpackCStringUtf8# :: Addr# -> [Char]
{-# NOINLINE unpackCStringUtf8# #-}
unpackCStringUtf8# addr
  = unpack 0#
  where
    unpack nh
      | isTrue# (ch `eqChar#` '\0'#  ) = []
      | isTrue# (ch `leChar#` '\x7F'#) = C# ch : unpack (nh +# 1#)
      | isTrue# (ch `leChar#` '\xDF'#) =
          C# (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#))) :
          unpack (nh +# 2#)
      | isTrue# (ch `leChar#` '\xEF'#) =
          C# (chr# (((ord# ch                                  -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# addr (nh +# 2#)) -# 0x80#))) :
          unpack (nh +# 3#)
      | True                           =
          C# (chr# (((ord# ch                                  -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharOffAddr# addr (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# addr (nh +# 3#)) -# 0x80#))) :
          unpack (nh +# 4#)
      where
        !ch = indexCharOffAddr# addr nh

-- There's really no point in inlining this for the same reasons as
-- unpackCString. See Note [Inlining unpackCString#] above for details.
unpackNBytes# :: Addr# -> Int# -> [Char]
{-# NOINLINE unpackNBytes# #-}
unpackNBytes# _addr 0#   = []
unpackNBytes#  addr len# = unpack [] (len# -# 1#)
    where
     unpack acc i#
      | isTrue# (i# <# 0#)  = acc
      | True                =
         case indexCharOffAddr# addr i# of
            ch -> unpack (C# ch : acc) (i# -# 1#)

