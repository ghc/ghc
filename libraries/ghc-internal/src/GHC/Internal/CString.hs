{-# LANGUAGE MagicHash, NoImplicitPrelude, BangPatterns, UnliftedFFITypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.CString
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/ghc-internal/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC C strings definitions (previously in GHC.Internal.Base).
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Internal.CString (
        -- * Ascii variants
        unpackCString#, unpackAppendCString#, unpackFoldrCString#,
        cstringLength#,

        -- * Utf variants
        unpackCStringUtf8#, unpackAppendCStringUtf8#, unpackFoldrCStringUtf8#,

        -- * Other
        unpackNBytes#,
    ) where

import GHC.Internal.Types hiding (One)
import GHC.Internal.Prim

{-
Note [String literals in GHC]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
String literals get quite a bit of special handling in GHC.  This Note
summarises the moving parts.

* Desugaring: see GHC.HsToCore.Match.Literal.dsLit, which in
  turn calls GHC.Core.Make.mkStringExprFS.

  The desugarer desugars the Haskell literal "foo" into Core
     GHC.Internal.CString.unpackCString# "foo"#
  where "foo"# is primitive string literal (of type Addr#).

  When the string cannot be encoded as a C string, we use UTF8:
     GHC.Internal.CString.unpackCStringUtf8# "foo"#

* The library module ghc-internal:GHC.Internal.CString has a bunch of functions that
  work over primitive strings, including GHC.Internal.CString.unpackCString#

* GHC.Core.Op.ConstantFold has some RULES that optimise certain string
  operations on literal strings. For example:

    + Constant folding the desugared form of ("foo" ++ "bar")
      into ("foobar")
    + Comparing strings
    + and more

* GHC.Internal.Base has a number of regular rules for String literals.

  + a rule "eqString": (==) @String = eqString
    where GHC.Internal.Base.eqString :: String -> String -> Bool

    ConstantFold has a RULE for eqString on literals:
     eqString (Lit "foo"#) (Lit "bar"#) --> False

    This allows compile time evaluation of things like "foo" == "bar"

  + A bunch of rules to promote fusion:

    "unpack"       [~1] forall a   . unpackCString# a             = build (unpackFoldrCString# a)
    "unpack-list"  [1]  forall a   . unpackFoldrCString# a (:) [] = unpackCString# a
    "unpack-append"     forall a n . unpackFoldrCString# a (:) n  = unpackAppendCString# a n

    And UTF8 variants of these rules.

* We allow primitive (unlifted) literal strings to be top-level
  bindings, breaking out usual rule.  See GHC.Core
  Note [Core top-level string literals]

* TODO: There is work on a special code-gen path for top-level boxed strings
     str :: [Char]
     str = unpackCString# "foo"#
  so that they can all share a common code pointer

  There is a WIP MR on gitlab for this: !3012

-}

-----------------------------------------------------------------------------
-- Unpacking C strings
-----------------------------------------------------------------------------

-- This code is needed for virtually all programs, since it's used for
-- unpacking the strings of error messages.

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

 * unpacking rules; e.g. in GHC.Internal.Base,
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

{-
Note [Inlining of unpackFoldrCString]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Usually the unpack-list rule turns unpackFoldrCString# into unpackCString#
It also has a BuiltInRule in PrelRules.hs:
     unpackFoldrCString# "foo" c (unpackFoldrCString# "baz" c n)
       =  unpackFoldrCString# "foobaz" c n

We use NOINLINE [0] on the grounds that, unlike
unpackCString#, there *is* some point in inlining
unpackFoldrCString#, because we get better code for the
higher-order function call.

This can cause a code size increase but it was minimal
when looking at nofib.

This is especially important for elem which then results in an
allocation free loop.

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

This has the additional advantage the we can guarantee that only the
increment will happen in the loop.
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

-- Usually the unpack-list rule turns unpackFoldrCString# into unpackCString#.
-- See Note [String literals in GHC] for more details.
-- See [Inlining of unpackFoldrCString]
{-# NOINLINE[0] unpackFoldrCString# #-}
unpackFoldrCString# :: Addr# -> (Char -> a -> a) -> a -> a
unpackFoldrCString# str f z_init = go str z_init
  where
    go addr z
      | isTrue# (ch `eqChar#` '\0'#) = z
      | True                         = C# ch `f` go (addr `plusAddr#` 1#) z
      where
        -- See Note [unpackCString# iterating over addr]
        !ch = indexCharOffAddr# addr 0#

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

-- See Note [Inlining of unpackFoldrCString]
{-# NOINLINE[0] unpackFoldrCStringUtf8# #-}
unpackFoldrCStringUtf8# :: Addr# -> (Char -> a -> a) -> a -> a
unpackFoldrCStringUtf8# addr_init f z_init
  = go addr_init z_init
  where
    go addr z
      | isTrue# (ch `eqChar#` '\0'#) = z
      | True =
          let !byte_count = getByteCount ch
              !utf_ch = unpackUtf8Char# byte_count ch addr
              !addr' = (addr `plusBytes` byte_count)
          in C# utf_ch `f` go addr' z
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

-- The return type is not correct here. We really want CSize,
-- but that type is defined in base. However, CSize should always
-- match the size of a machine word (I hope), so this is probably
-- alright on all platforms that GHC supports.
foreign import ccall unsafe "strlen" c_strlen :: Addr# -> Int#

-- | Compute the length of a NUL-terminated string. This address
-- must refer to immutable memory. GHC includes a built-in rule for
-- constant folding when the argument is a statically-known literal.
-- That is, a core-to-core pass reduces the expression
-- @cstringLength# "hello"#@ to the constant @5#@.
cstringLength# :: Addr# -> Int#
{-# INLINE[0] cstringLength# #-}
cstringLength# = c_strlen


------------------------------
--- UTF-8 decoding utilities
------------------------------
--
-- This is one of several UTF-8 implementations provided by GHC; see Note
-- [GHC's many UTF-8 implementations] in "GHC.Encoding.UTF8" for an
-- overview.
--
-- These functions make explicit the logic that was originally
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
