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

{- Note [Inlining unpackCString#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's really no point in ever inlining things like unpackCString# as the loop
doesn't specialise in an interesting way and we can't deforest the list
constructors (we'd want to use unpackFoldrCString# for this). Moreover, it's
pretty small, so there's a danger that it'll be inlined at every literal, which
is a waste.

Moreover, inlining early may interfere with a variety of rules that are supposed
to match unpackCString#,

 * BuiltInRules in GHC.Core.Opt.ConstantFold; e.g.
       eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2)
          = s1 == s2

 * unpacking rules; e.g. in GHC.Base,
       unpackCString# a
          = build (unpackFoldrCString# a)

 * stream fusion rules; e.g. in the `text` library,
       unstream (S.map safe (S.streamList (GHC.unpackCString# a)))
          = unpackCString# a

Moreover, we want to make it CONLIKE, so that:

* the rules in GHC.Core.Opt.ConstantFold will fire when the string is let-bound.
  E.g. the eqString rule in GHC.Core.Opt.ConstantFold
   eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2

* exprIsConApp_maybe will see the string when we have
     let x = unpackCString# "foo"#
     ...(case x of algs)...

All of this goes for unpackCStringUtf8# too.
-}

{- Note [unpackCString# iterating over addr]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When unpacking unpackCString# and friends repeatedly return a cons cell
containing:
* The current character we just unpacked.
* A thunk to unpack the rest of the string.

In order to minimize the size of the thunk we do not index of
the start of the string, offsetting into it, but instead increment
the addr and always use offset 0#.

This works since these two expressions will read from the same address.
* `indexCharOffAddr# a i`
* `indexCharOffAddr (a `plusAddr#` i) 0#`

This way we avoid the need for the thunks to close over both the start of
the string and the current offset, saving a word for each character unpacked.
-}

unpackCString# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString# #-}
unpackCString# addr
    | isTrue# (ch `eqChar#` '\0'#) = []
    | True                         = C# ch : unpackCString# (addr `plusAddr#` 1#)
      where
        -- See Note [unpackCString# iterating over addr]
        !ch = indexCharOffAddr# addr 0#


unpackAppendCString# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString# #-}
     -- See the NOINLINE note on unpackCString#
unpackAppendCString# addr rest
    | isTrue# (ch `eqChar#` '\0'#) = rest
    | True                         = C# ch : unpackAppendCString# (addr `plusAddr#` 1#) rest
      where
        -- See Note [unpackCString# iterating over addr]
        !ch = indexCharOffAddr# addr 0#

unpackFoldrCString# :: Addr# -> (Char  -> a -> a) -> a -> a

-- Usually the unpack-list rule turns unpackFoldrCString# into unpackCString#

-- It also has a BuiltInRule in GHC.Core.Opt.ConstantFold:
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
  | isTrue# (ch `eqChar#` '\0'#) = z
  | True                         = C# ch `f` unpackFoldrCString# (addr `plusAddr#` 1#) f z
  where
    -- See Note [unpackCString# iterating over addr]
    !ch = indexCharOffAddr# addr 0#

-- There's really no point in inlining this for the same reasons as
-- unpackCString. See Note [Inlining unpackCString#] above for details.
unpackCStringUtf8# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCStringUtf8# #-}
unpackCStringUtf8# addr
    | isTrue# (ch `eqChar#` '\0'#  ) = []
    | isTrue# (ch `leChar#` '\x7F'#) = C# ch : unpackCStringUtf8# (addr `plusAddr#` 1#)
    | isTrue# (ch `leChar#` '\xDF'#) =
        let !c = C# (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                            (ord# (indexCharOffAddr# (addr `plusAddr#` 1#) 0#) -# 0x80#)))
        in c : unpackCStringUtf8# (addr `plusAddr#` 2#)
    | isTrue# (ch `leChar#` '\xEF'#) =
        let !c = C# (chr# (((ord# ch                                             -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                           ((ord# (indexCharOffAddr# (addr `plusAddr#` 1#) 0#) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                            (ord# (indexCharOffAddr# (addr `plusAddr#` 2#) 0#) -# 0x80#)))
        in c : unpackCStringUtf8# (addr `plusAddr#` 3#)
    | True                           =
        let !c = C# (chr# (((ord# ch                                  -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                           ((ord# (indexCharOffAddr# (addr `plusAddr#` 1#) 0#) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                           ((ord# (indexCharOffAddr# (addr `plusAddr#` 2#) 0#) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                            (ord# (indexCharOffAddr# (addr `plusAddr#` 3#) 0#) -# 0x80#)))
        in c : unpackCStringUtf8# (addr `plusAddr#` 4#)
      where
        -- See Note [unpackCString# iterating over addr]
        !ch = indexCharOffAddr# addr 0#

-- There's really no point in inlining this for the same reasons as
-- unpackCString. See Note [Inlining unpackCString#] above for details.
unpackNBytes# :: Addr# -> Int# -> [Char]
{-# NOINLINE unpackNBytes# #-}
unpackNBytes# _addr 0#   = []
unpackNBytes#  addr len# = unpack [] (len# -# 1#)
    where
     unpack :: [Char] -> Int# -> [Char]
     unpack acc i#
      | isTrue# (i# <# 0#)  = acc
      | True                =
         case indexCharOffAddr# addr i# of
            ch -> unpack (C# ch : acc) (i# -# 1#)

