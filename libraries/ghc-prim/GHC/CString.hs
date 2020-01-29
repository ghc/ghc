{-# LANGUAGE MagicHash, NoImplicitPrelude, BangPatterns #-}
{-# OPTIONS -fregs-graph #-} -- Needed for elemCString#, see #17823

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
        -- * Ascii variants
        unpackCString#, unpackAppendCString#, unpackFoldrCString#,
        elemCString#,

        -- * Utf variants
        unpackCStringUtf8#, unpackAppendCStringUtf8#, unpackFoldrCStringUtf8#,
        elemCStringUtf8#,

        -- * Other
        unpackNBytes#,
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

 * BuiltInRules in GHC.Core.Op.ConstantFold; e.g.
       eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2)
          = s1 == s2

 * unpacking rules; e.g. in GHC.Base,
       unpackCString# a
          = build (unpackFoldrCString# a)

 * stream fusion rules; e.g. in the `text` library,
       unstream (S.map safe (S.streamList (GHC.unpackCString# a)))
          = unpackCString# a

Moreover, we want to make it CONLIKE, so that:

* the rules in GHC.Core.Op.ConstantFold will fire when the string is let-bound.
  E.g. the eqString rule in GHC.Core.Op.ConstantFold
   eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2

* exprIsConApp_maybe will see the string when we have
     let x = unpackCString# "foo"#
     ...(case x of algs)...

All of this goes for unpackCStringUtf8# too.
-}

{- Note [NOINLINE for unpackFoldrCString]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Usually the unpack-list rule turns unpackFoldrCString# into unpackCString#
It also has a BuiltInRule in PrelRules.hs:
     unpackFoldrCString# "foo" c (unpackFoldrCString# "baz" c n)
       =  unpackFoldrCString# "foobaz" c n
At one stage I had NOINLINE [0] on the grounds that, unlike
unpackCString#, there *is* some point in inlining
unpackFoldrCString#, because we get better code for the
higher-order function call.  BUT there may be a lot of
literal strings, and making a separate 'unpack' loop for
each is highly gratuitous.  See nofib/real/anna/PrettyPrint.

  Note [unpackCString# iterating over addr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

This has the additional advantage the we can guarantee that  only the
increment will happen in the loop.
If we use the offset start off with the increment and an addition
to get the real address. Which might not be optimized aways.

  Note [Compiling `elem` on Strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The pattern f c = c `elem` "<someString>" is quite common in
parsers and the like and can be quite performance critical.

The fastest way to process this pattern is to transform it
into a case. That is we transform:

  x `elem` "xy"
=>
  case x of
    'x' -> True
    'y' -> True
    _   -> False

For this reason there is a BUILTIN rule "ElemLitString" which
performs this rewrite early in the pipeline in case "xy" is built
from unpacking a string literal of small size.

For long strings however we avoid doing this translation as
it would impact code size negatively.

However we do not want to fall back to allocating a String and
then calling elem onto it. So towards the end of the pipeline
we use rewrite it into a more efficient form working of the
unboxed string literal.

Implementing this in terms of unpackFoldrCString# is not possible
without allocating. As the folding function would have to capture
the element we look for.

Instead we rewrite to a spezialized version of elem, `elemCString#`
which works over unboxed string literals.

  Note [Inlining of elemCString#]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We inline both elemCString*# variants in the hope to cancel out the unboxing
of the otherwise lazy character argument.

For elemCString# we allow inlining the inner loop as it's tiny comming in
at less than 40bytes on x64. This comes out to about the same size of the
size overhead we pay for a non-tail call. Not to speak of eliminating the
runtime overhead of the call.

For elemCStringUtf8# this is different. The whole unicode logic makes it large
enough to make inlining a bad choice. So we use the magic `noinline` to avoid
the inner loop containing the utf8 logic to be inlined. Given the additional
overhead of unicode decoding the call overhead is also less significant so it's
not as big of a loss.

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

-- See Note [NOINLINE for unpackFoldrCString]
{-# NOINLINE unpackFoldrCString# #-}
unpackFoldrCString# :: Addr# -> (Char  -> a -> a) -> a -> a
unpackFoldrCString# addr f z
  | isTrue# (ch `eqChar#` '\0'#) = z
  | True                         = C# ch `f` unpackFoldrCString# (addr `plusAddr#` 1#) f z
  where
    -- See Note [unpackCString# iterating over addr]
    !ch = indexCharOffAddr# addr 0#

-- See Note [Compiling `elem` on Strings]
-- See Note [Inlining of elemCString#]
{-# INLINE elemCString# #-}
elemCString# :: Char -> Addr# -> Bool
elemCString# c base_addr =
    let !ch = (indexCharOffAddr# base_addr 0#)
    in  -- We check for end-of-string first to preserve laziness
        -- of the Char argument.
        case ch of
            '\0'#   -> False
            _       -> case c of
                        C# c' -> check c' ch base_addr
  where
    -- Invariant: ch != '\NULL'
    check :: Char# -> Char# -> Addr# -> Bool
    check c_ub ch addr
      | isTrue# (ch `eqChar#` c_ub ) = True
      | True =
          let !addr' = (addr `plusAddr#` 1#)
          in case indexCharOffAddr# addr' 0# of
              '\0'#   -> False
              next_ch -> check c_ub next_ch addr'

-- There's really no point in inlining this for the same reasons as
-- unpackCString. See Note [Inlining unpackCString#] above for details.
unpackCStringUtf8# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCStringUtf8# #-}
unpackCStringUtf8# addr
    | isTrue# (ch `eqChar#` '\0'#  ) = []
    | True =
        let !byte_count = getByteCount ch
            !utf_ch = unpackUtf8Char# byte_count ch addr
            !addr' = addr `plusBytes` byte_count
        in  C# utf_ch : unpackCStringUtf8# addr'
      where
        -- See Note [unpackCString# iterating over addr]
        !ch = indexCharOffAddr# addr 0#


unpackAppendCStringUtf8# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCStringUtf8# #-}
     -- See the NOINLINE note on unpackCString#
unpackAppendCStringUtf8# addr rest
    | isTrue# (ch `eqChar#` '\0'#) = rest
    | True =
        let !byte_count = getByteCount ch
            !utf_ch = unpackUtf8Char# byte_count ch addr
            !addr' = (addr `plusBytes` byte_count)
        in  C# utf_ch : unpackAppendCStringUtf8# addr' rest
      where
        -- See Note [unpackCString# iterating over addr]
        !ch = indexCharOffAddr# addr 0#

-- See Note [NOINLINE for unpackFoldrCString]
{-# NOINLINE unpackFoldrCStringUtf8# #-}
unpackFoldrCStringUtf8# :: Addr# -> (Char -> a -> a) -> a -> a
unpackFoldrCStringUtf8# addr f z
    | isTrue# (ch `eqChar#` '\0'#) = z
    | True =
        let !byte_count = getByteCount ch
            !utf_ch = unpackUtf8Char# byte_count ch addr
            !addr' = (addr `plusBytes` byte_count)
        in C# utf_ch `f` unpackFoldrCStringUtf8# addr' f z
  where
    -- See Note [unpackCString# iterating over addr]
    !ch = indexCharOffAddr# addr 0#

-- See Note [Inlining of elemCString#]
-- See Note [Compiling `elem` on Strings]
{-# INLINE elemCStringUtf8# #-}
elemCStringUtf8# :: Char -> Addr# -> Bool
elemCStringUtf8# c base_addr =
    let !ch = (indexCharOffAddr# base_addr 0#)
    in  -- We check for end-of-string first to preserve laziness
        -- of the Char argument.
        case ch of
            '\0'#   -> False
            _       -> case c of
                        C# c' -> elemCStringUtf8_check# c' ch base_addr

-- Local to elemCStringUtf8#, defined at the top to avoid unfolding it
-- into use sites.
-- Invariant: ch != '\NULL'
{-# NOINLINE elemCStringUtf8_check# #-}
elemCStringUtf8_check# :: Char# -> Char# -> Addr# -> Bool
elemCStringUtf8_check# c_ub ch addr =
    let !byte_count = getByteCount ch
        !utf_ch = unpackUtf8Char# byte_count ch addr
        !addr' = (addr `plusBytes` byte_count)
    in if (isTrue# (utf_ch `eqChar#` c_ub))
        then True
        else case (indexCharOffAddr# addr' 0#) of
                '\0'# -> False
                ch'   -> elemCStringUtf8_check# c_ub ch' addr'

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



------------------------------
--- UTF8 decoding utilities
------------------------------
--
-- These functions make explizit the logic that was originally
-- part of unpackCStringUtf8. Since we want the same support for ascii
-- and non-ascii a variety of functions needs the same logic. Instead
-- of C&P'in the decoding logic all over we have it here once, and then
-- force GHC to inline it.
--
-- All the overhead of the Bytes argument and calls goes away once all is
-- said and done. And what remains is readable code in Haskell land and
-- performant code in the resulting binary.

data Bytes = One | Two | Three | Four

{-# INLINE getByteCount #-}
getByteCount :: Char# -> Bytes
getByteCount ch
    | isTrue# (ch `leChar#` '\x7F'#) = One
    | isTrue# (ch `leChar#` '\xDF'#) = Two
    | isTrue# (ch `leChar#` '\xEF'#) = Three
    | True                           = Four

{-# INLINE plusBytes #-}
plusBytes :: Addr# -> Bytes -> Addr#
plusBytes addr bytes =
  case bytes of
    One   -> addr `plusAddr#` 1#
    Two   -> addr `plusAddr#` 2#
    Three -> addr `plusAddr#` 3#
    Four  -> addr `plusAddr#` 4#

-- | Take the current address, read unicode char of the given size.
-- We obviously want the number of bytes, but we have to read one
-- byte to determine the number of bytes for the current codepoint
-- so we might as well reuse it and avoid a read.
--
-- Side Note: We don't dare to decode all 4 possibilities at once.
-- Reading past the end of the addr might trigger an exception.
-- For this reason we really have to check the width first and only
-- decode after.
{-# INLINE unpackUtf8Char# #-}
unpackUtf8Char# :: Bytes -> Char# -> Addr# -> Char#
unpackUtf8Char# bytes ch addr =
  case bytes of
    One -> ch
    Two ->   (chr# (((ord# ch                                           -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# (addr `plusAddr#` 1#) 0#) -# 0x80#)))
    Three -> (chr# (((ord# ch                                           -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharOffAddr# (addr `plusAddr#` 1#) 0#) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# (addr `plusAddr#` 2#) 0#) -# 0x80#)))
    Four ->  (chr# (((ord# ch                                           -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexCharOffAddr# (addr `plusAddr#` 1#) 0#) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharOffAddr# (addr `plusAddr#` 2#) 0#) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharOffAddr# (addr `plusAddr#` 3#) 0#) -# 0x80#)))
