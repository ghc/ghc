-----------------------------------------------------------------------
--
-- (c) 2010 The University of Glasgow
--
-- Primitive Operations and Types
--
-- For more information on PrimOps, see
--   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/prim-ops
--
-----------------------------------------------------------------------

-- This file is processed by the utility program genprimopcode to produce
-- a number of include files within the compiler and optionally to produce
-- human-readable documentation.
--
-- It should first be preprocessed.
--
-- Information on how PrimOps are implemented and the steps necessary to
-- add a new one can be found in the Commentary:
--
--  https://gitlab.haskell.org/ghc/ghc/wikis/commentary/prim-ops
--
-- Note in particular that Haskell block-style comments are not recognized
-- here, so stick to '--' (even for Notes spanning multiple lines).

-- This file is divided into named sections, each containing or more
-- primop entries. Section headers have the format:
--
--      section "section-name" {description}
--
-- This information is used solely when producing documentation; it is
-- otherwise ignored.  The description is optional.
--
-- The format of each primop entry is as follows:
--
--      primop internal-name "name-in-program-text" category type {description} attributes

-- The default attribute values which apply if you don't specify
-- other ones.  Attribute values can be True, False, or arbitrary
-- text between curly brackets.  This is a kludge to enable
-- processors of this file to easily get hold of simple info
-- (eg, out_of_line), whilst avoiding parsing complex expressions
-- needed for strictness info.
--
-- type refers to the general category of the primop. Valid settings include,
--
--  * Compare:   A comparison operation of the shape a -> a -> Int#
--  * Monadic:   A unary operation of shape a -> a
--  * Dyadic:    A binary operation of shape a -> a -> a
--  * GenPrimOp: Any other sort of primop
--

-- The vector attribute is rather special. It takes a list of 3-tuples, each of
-- which is of the form <ELEM_TYPE,SCALAR_TYPE,LENGTH>. ELEM_TYPE is the type of
-- the elements in the vector; LENGTH is the length of the vector; and
-- SCALAR_TYPE is the scalar type used to inject to/project from vector
-- element. Note that ELEM_TYPE and SCALAR_TYPE are not the same; for example,
-- to broadcast a scalar value to a vector whose elements are of type Int8, we
-- use an Int#.

-- When a primtype or primop has a vector attribute, it is instantiated at each
-- 3-tuple in the list of 3-tuples. That is, the vector attribute allows us to
-- define a family of types or primops. Vector support also adds three new
-- keywords: VECTOR, SCALAR, and VECTUPLE. These keywords are expanded to types
-- derived from the 3-tuple. For the 3-tuple <Int64,INT64,2>, VECTOR expands to
-- Int64X2#, SCALAR expands to INT64, and VECTUPLE expands to (# INT64, INT64
-- #).

defaults
   has_side_effects = False
   out_of_line      = False   -- See Note [When do out-of-line primops go in primops.txt.pp]
   can_fail         = False   -- See Note [PrimOp can_fail and has_side_effects] in PrimOp
   commutable       = False
   code_size        = { primOpCodeSizeDefault }
   strictness       = { \ arity -> mkClosedStrictSig (replicate arity topDmd) topDiv }
   fixity           = Nothing
   llvm_only        = False
   vector           = []
   deprecated_msg   = {}      -- A non-empty message indicates deprecation


-- Note [When do out-of-line primops go in primops.txt.pp]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Out of line primops are those with a C-- implementation. But that
-- doesn't mean they *just* have an C-- implementation. As mentioned in
-- Note [Inlining out-of-line primops and heap checks], some out-of-line
-- primops also have additional internal implementations under certain
-- conditions. Now that `foreign import prim` exists, only those primops
-- which have both internal and external implementations ought to be
-- this file. The rest aren't really primops, since they don't need
-- bespoke compiler support but just a general way to interface with
-- C--. They are just foreign calls.
--
-- Unfortunately, for the time being most of the primops which should be
-- moved according to the previous paragraph can't yet. There are some
-- superficial restrictions in `foreign import prim` which mus be fixed
-- first. Specifically, `foreign import prim` always requires:
--
--   - No polymorphism in type
--   - `strictness       = <default>`
--   - `can_fail         = False`
--   - `has_side_effects = True`
--
-- https://gitlab.haskell.org/ghc/ghc/issues/16929 tracks this issue,
-- and has a table of which external-only primops are blocked by which
-- of these. Hopefully those restrictions are relaxed so the rest of
-- those can be moved over.
--
-- 'module GHC.Prim.Ext is a temporarily "holding ground" for primops
-- that were formally in here, until they can be given a better home.
-- Likewise, their underlying C-- implementation need not live in the
-- RTS either. Best case (in my view), both the C-- and `foreign import
-- prim` can be moved to a small library tailured to the features being
-- implemented and dependencies of those features.

-- Currently, documentation is produced using latex, so contents of
-- description fields should be legal latex. Descriptions can contain
-- matched pairs of embedded curly brackets.

#include "MachDeps.h"

section "The word size story."
        {Haskell98 specifies that signed integers (type {\tt Int})
         must contain at least 30 bits. GHC always implements {\tt
         Int} using the primitive type {\tt Int\#}, whose size equals
         the {\tt MachDeps.h} constant {\tt WORD\_SIZE\_IN\_BITS}.
         This is normally set based on the {\tt config.h} parameter
         {\tt SIZEOF\_HSWORD}, i.e., 32 bits on 32-bit machines, 64
         bits on 64-bit machines.  However, it can also be explicitly
         set to a smaller number than 64, e.g., 62 bits, to allow the
         possibility of using tag bits. Currently GHC itself has only
         32-bit and 64-bit variants, but 61, 62, or 63-bit code can be
         exported as an external core file for use in other back ends.
         30 and 31-bit code is no longer supported.

         GHC also implements a primitive unsigned integer type {\tt
         Word\#} which always has the same number of bits as {\tt
         Int\#}.

         In addition, GHC supports families of explicit-sized integers
         and words at 8, 16, 32, and 64 bits, with the usual
         arithmetic operations, comparisons, and a range of
         conversions.  The 8-bit and 16-bit sizes are always
         represented as {\tt Int\#} and {\tt Word\#}, and the
         operations implemented in terms of the primops on these
         types, with suitable range restrictions on the results (using
         the {\tt narrow$n$Int\#} and {\tt narrow$n$Word\#} families
         of primops.  The 32-bit sizes are represented using {\tt
         Int\#} and {\tt Word\#} when {\tt WORD\_SIZE\_IN\_BITS}
         $\geq$ 32; otherwise, these are represented using distinct
         primitive types {\tt Int32\#} and {\tt Word32\#}. These (when
         needed) have a complete set of corresponding operations;
         however, nearly all of these are implemented as external C
         functions rather than as primops.  Exactly the same story
         applies to the 64-bit sizes.  All of these details are hidden
         under the {\tt PrelInt} and {\tt PrelWord} modules, which use
         {\tt \#if}-defs to invoke the appropriate types and
         operators.

         Word size also matters for the families of primops for
         indexing/reading/writing fixed-size quantities at offsets
         from an array base, address, or foreign pointer.  Here, a
         slightly different approach is taken.  The names of these
         primops are fixed, but their {\it types} vary according to
         the value of {\tt WORD\_SIZE\_IN\_BITS}. For example, if word
         size is at least 32 bits then an operator like
         \texttt{indexInt32Array\#} has type {\tt ByteArray\# -> Int\#
         -> Int\#}; otherwise it has type {\tt ByteArray\# -> Int\# ->
         Int32\#}.  This approach confines the necessary {\tt
         \#if}-defs to this file; no conditional compilation is needed
         in the files that expose these primops.

         Finally, there are strongly deprecated primops for coercing
         between {\tt Addr\#}, the primitive type of machine
         addresses, and {\tt Int\#}.  These are pretty bogus anyway,
         but will work on existing 32-bit and 64-bit GHC targets; they
         are completely bogus when tag bits are used in {\tt Int\#},
         so are not available in this case.  }

-- Define synonyms for indexing ops.

#define INT32 Int#
#define WORD32 Word#

#if WORD_SIZE_IN_BITS < 64
#define INT64 Int64#
#define WORD64 Word64#
#else
#define INT64 Int#
#define WORD64 Word#
#endif

-- This type won't be exported directly (since there is no concrete
-- syntax for this sort of export) so we'll have to manually patch
-- export lists in both GHC and Haddock.
primtype (->) a b
  {The builtin function type, written in infix form as {\tt a -> b} and
   in prefix form as {\tt (->) a b}. Values of this type are functions
   taking inputs of type {\tt a} and producing outputs of type {\tt b}.

   Note that {\tt a -> b} permits levity-polymorphism in both {\tt a} and
   {\tt b}, so that types like {\tt Int\# -> Int\#} can still be well-kinded.
  }
  with fixity = infixr -1
         -- This fixity is only the one picked up by Haddock. If you
         -- change this, do update 'ghcPrimIface' in 'GHC.Iface.Load'.

------------------------------------------------------------------------
section "Char#"
        {Operations on 31-bit characters.}
------------------------------------------------------------------------

primtype Char#

primop   CharGtOp  "gtChar#"   Compare   Char# -> Char# -> Int#
primop   CharGeOp  "geChar#"   Compare   Char# -> Char# -> Int#

primop   CharEqOp  "eqChar#"   Compare
   Char# -> Char# -> Int#
   with commutable = True

primop   CharNeOp  "neChar#"   Compare
   Char# -> Char# -> Int#
   with commutable = True

primop   CharLtOp  "ltChar#"   Compare   Char# -> Char# -> Int#
primop   CharLeOp  "leChar#"   Compare   Char# -> Char# -> Int#

primop   OrdOp   "ord#"  GenPrimOp   Char# -> Int#
   with code_size = 0

------------------------------------------------------------------------
section "Int8#"
        {Operations on 8-bit integers.}
------------------------------------------------------------------------

primtype Int8#

primop Int8Extend "extendInt8#" GenPrimOp Int8# -> Int#
primop Int8Narrow "narrowInt8#" GenPrimOp Int# -> Int8#

primop Int8NegOp "negateInt8#" Monadic Int8# -> Int8#

primop Int8AddOp "plusInt8#" Dyadic Int8# -> Int8# -> Int8#
  with
    commutable = True

primop Int8SubOp "subInt8#" Dyadic Int8# -> Int8# -> Int8#

primop Int8MulOp "timesInt8#" Dyadic Int8# -> Int8# -> Int8#
  with
    commutable = True

primop Int8QuotOp "quotInt8#" Dyadic Int8# -> Int8# -> Int8#
  with
    can_fail = True

primop Int8RemOp "remInt8#" Dyadic Int8# -> Int8# -> Int8#
  with
    can_fail = True

primop Int8QuotRemOp "quotRemInt8#" GenPrimOp Int8# -> Int8# -> (# Int8#, Int8# #)
  with
    can_fail = True

primop Int8EqOp "eqInt8#" Compare Int8# -> Int8# -> Int#
primop Int8GeOp "geInt8#" Compare Int8# -> Int8# -> Int#
primop Int8GtOp "gtInt8#" Compare Int8# -> Int8# -> Int#
primop Int8LeOp "leInt8#" Compare Int8# -> Int8# -> Int#
primop Int8LtOp "ltInt8#" Compare Int8# -> Int8# -> Int#
primop Int8NeOp "neInt8#" Compare Int8# -> Int8# -> Int#

------------------------------------------------------------------------
section "Word8#"
        {Operations on 8-bit unsigned integers.}
------------------------------------------------------------------------

primtype Word8#

primop Word8Extend "extendWord8#" GenPrimOp Word8# -> Word#
primop Word8Narrow "narrowWord8#" GenPrimOp Word# -> Word8#

primop Word8NotOp "notWord8#" Monadic Word8# -> Word8#

primop Word8AddOp "plusWord8#" Dyadic Word8# -> Word8# -> Word8#
  with
    commutable = True

primop Word8SubOp "subWord8#" Dyadic Word8# -> Word8# -> Word8#

primop Word8MulOp "timesWord8#" Dyadic Word8# -> Word8# -> Word8#
  with
    commutable = True

primop Word8QuotOp "quotWord8#" Dyadic Word8# -> Word8# -> Word8#
  with
    can_fail = True

primop Word8RemOp "remWord8#" Dyadic Word8# -> Word8# -> Word8#
  with
    can_fail = True

primop Word8QuotRemOp "quotRemWord8#" GenPrimOp Word8# -> Word8# -> (# Word8#, Word8# #)
  with
    can_fail = True

primop Word8EqOp "eqWord8#" Compare Word8# -> Word8# -> Int#
primop Word8GeOp "geWord8#" Compare Word8# -> Word8# -> Int#
primop Word8GtOp "gtWord8#" Compare Word8# -> Word8# -> Int#
primop Word8LeOp "leWord8#" Compare Word8# -> Word8# -> Int#
primop Word8LtOp "ltWord8#" Compare Word8# -> Word8# -> Int#
primop Word8NeOp "neWord8#" Compare Word8# -> Word8# -> Int#

------------------------------------------------------------------------
section "Int16#"
        {Operations on 16-bit integers.}
------------------------------------------------------------------------

primtype Int16#

primop Int16Extend "extendInt16#" GenPrimOp Int16# -> Int#
primop Int16Narrow "narrowInt16#" GenPrimOp Int# -> Int16#

primop Int16NegOp "negateInt16#" Monadic Int16# -> Int16#

primop Int16AddOp "plusInt16#" Dyadic Int16# -> Int16# -> Int16#
  with
    commutable = True

primop Int16SubOp "subInt16#" Dyadic Int16# -> Int16# -> Int16#

primop Int16MulOp "timesInt16#" Dyadic Int16# -> Int16# -> Int16#
  with
    commutable = True

primop Int16QuotOp "quotInt16#" Dyadic Int16# -> Int16# -> Int16#
  with
    can_fail = True

primop Int16RemOp "remInt16#" Dyadic Int16# -> Int16# -> Int16#
  with
    can_fail = True

primop Int16QuotRemOp "quotRemInt16#" GenPrimOp Int16# -> Int16# -> (# Int16#, Int16# #)
  with
    can_fail = True

primop Int16EqOp "eqInt16#" Compare Int16# -> Int16# -> Int#
primop Int16GeOp "geInt16#" Compare Int16# -> Int16# -> Int#
primop Int16GtOp "gtInt16#" Compare Int16# -> Int16# -> Int#
primop Int16LeOp "leInt16#" Compare Int16# -> Int16# -> Int#
primop Int16LtOp "ltInt16#" Compare Int16# -> Int16# -> Int#
primop Int16NeOp "neInt16#" Compare Int16# -> Int16# -> Int#

------------------------------------------------------------------------
section "Word16#"
        {Operations on 16-bit unsigned integers.}
------------------------------------------------------------------------

primtype Word16#

primop Word16Extend "extendWord16#" GenPrimOp Word16# -> Word#
primop Word16Narrow "narrowWord16#" GenPrimOp Word# -> Word16#

primop Word16NotOp "notWord16#" Monadic Word16# -> Word16#

primop Word16AddOp "plusWord16#" Dyadic Word16# -> Word16# -> Word16#
  with
    commutable = True

primop Word16SubOp "subWord16#" Dyadic Word16# -> Word16# -> Word16#

primop Word16MulOp "timesWord16#" Dyadic Word16# -> Word16# -> Word16#
  with
    commutable = True

primop Word16QuotOp "quotWord16#" Dyadic Word16# -> Word16# -> Word16#
  with
    can_fail = True

primop Word16RemOp "remWord16#" Dyadic Word16# -> Word16# -> Word16#
  with
    can_fail = True

primop Word16QuotRemOp "quotRemWord16#" GenPrimOp Word16# -> Word16# -> (# Word16#, Word16# #)
  with
    can_fail = True

primop Word16EqOp "eqWord16#" Compare Word16# -> Word16# -> Int#
primop Word16GeOp "geWord16#" Compare Word16# -> Word16# -> Int#
primop Word16GtOp "gtWord16#" Compare Word16# -> Word16# -> Int#
primop Word16LeOp "leWord16#" Compare Word16# -> Word16# -> Int#
primop Word16LtOp "ltWord16#" Compare Word16# -> Word16# -> Int#
primop Word16NeOp "neWord16#" Compare Word16# -> Word16# -> Int#

#if WORD_SIZE_IN_BITS < 64
------------------------------------------------------------------------
section "Int64#"
        {Operations on 64-bit unsigned words. This type is only used
         if plain {\tt Int\#} has less than 64 bits. In any case, the operations
         are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primtype Int64#

------------------------------------------------------------------------
section "Word64#"
        {Operations on 64-bit unsigned words. This type is only used
         if plain {\tt Word\#} has less than 64 bits. In any case, the operations
         are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primtype Word64#

#endif

------------------------------------------------------------------------
section "Int#"
        {Operations on native-size integers (32+ bits).}
------------------------------------------------------------------------

primtype Int#

primop   IntAddOp    "+#"    Dyadic
   Int# -> Int# -> Int#
   with commutable = True
        fixity = infixl 6

primop   IntSubOp    "-#"    Dyadic   Int# -> Int# -> Int#
   with fixity = infixl 6

primop   IntMulOp    "*#"
   Dyadic   Int# -> Int# -> Int#
   {Low word of signed integer multiply.}
   with commutable = True
        fixity = infixl 7

primop   IntMul2Op    "timesInt2#" GenPrimOp
   Int# -> Int# -> (# Int#, Int#, Int# #)
   {Return a triple (isHighNeeded,high,low) where high and low are respectively
   the high and low bits of the double-word result. isHighNeeded is a cheap way
   to test if the high word is a sign-extension of the low word (isHighNeeded =
   0#) or not (isHighNeeded = 1#).}

primop   IntMulMayOfloOp  "mulIntMayOflo#"
   Dyadic   Int# -> Int# -> Int#
   {Return non-zero if there is any possibility that the upper word of a
    signed integer multiply might contain useful information.  Return
    zero only if you are completely sure that no overflow can occur.
    On a 32-bit platform, the recommended implementation is to do a
    32 x 32 -> 64 signed multiply, and subtract result[63:32] from
    (result[31] >>signed 31).  If this is zero, meaning that the
    upper word is merely a sign extension of the lower one, no
    overflow can occur.

    On a 64-bit platform it is not always possible to
    acquire the top 64 bits of the result.  Therefore, a recommended
    implementation is to take the absolute value of both operands, and
    return 0 iff bits[63:31] of them are zero, since that means that their
    magnitudes fit within 31 bits, so the magnitude of the product must fit
    into 62 bits.

    If in doubt, return non-zero, but do make an effort to create the
    correct answer for small args, since otherwise the performance of
    \texttt{(*) :: Integer -> Integer -> Integer} will be poor.
   }
   with commutable = True

primop   IntQuotOp    "quotInt#"    Dyadic
   Int# -> Int# -> Int#
   {Rounds towards zero. The behavior is undefined if the second argument is
    zero.
   }
   with can_fail = True

primop   IntRemOp    "remInt#"    Dyadic
   Int# -> Int# -> Int#
   {Satisfies \texttt{(quotInt\# x y) *\# y +\# (remInt\# x y) == x}. The
    behavior is undefined if the second argument is zero.
   }
   with can_fail = True

primop   IntQuotRemOp "quotRemInt#"    GenPrimOp
   Int# -> Int# -> (# Int#, Int# #)
   {Rounds towards zero.}
   with can_fail = True

primop   AndIOp   "andI#"   Dyadic    Int# -> Int# -> Int#
   {Bitwise "and".}
   with commutable = True

primop   OrIOp   "orI#"     Dyadic    Int# -> Int# -> Int#
   {Bitwise "or".}
   with commutable = True

primop   XorIOp   "xorI#"   Dyadic    Int# -> Int# -> Int#
   {Bitwise "xor".}
   with commutable = True

primop   NotIOp   "notI#"   Monadic   Int# -> Int#
   {Bitwise "not", also known as the binary complement.}

primop   IntNegOp    "negateInt#"    Monadic   Int# -> Int#
   {Unary negation.
    Since the negative {\tt Int#} range extends one further than the
    positive range, {\tt negateInt#} of the most negative number is an
    identity operation. This way, {\tt negateInt#} is always its own inverse.}

primop   IntAddCOp   "addIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
         {Add signed integers reporting overflow.
          First member of result is the sum truncated to an {\tt Int#};
          second member is zero if the true sum fits in an {\tt Int#},
          nonzero if overflow occurred (the sum is either too large
          or too small to fit in an {\tt Int#}).}
   with code_size = 2
        commutable = True

primop   IntSubCOp   "subIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
         {Subtract signed integers reporting overflow.
          First member of result is the difference truncated to an {\tt Int#};
          second member is zero if the true difference fits in an {\tt Int#},
          nonzero if overflow occurred (the difference is either too large
          or too small to fit in an {\tt Int#}).}
   with code_size = 2

primop   IntGtOp  ">#"   Compare   Int# -> Int# -> Int#
   with fixity = infix 4

primop   IntGeOp  ">=#"   Compare   Int# -> Int# -> Int#
   with fixity = infix 4

primop   IntEqOp  "==#"   Compare
   Int# -> Int# -> Int#
   with commutable = True
        fixity = infix 4

primop   IntNeOp  "/=#"   Compare
   Int# -> Int# -> Int#
   with commutable = True
        fixity = infix 4

primop   IntLtOp  "<#"   Compare   Int# -> Int# -> Int#
   with fixity = infix 4

primop   IntLeOp  "<=#"   Compare   Int# -> Int# -> Int#
   with fixity = infix 4

primop   ChrOp   "chr#"   GenPrimOp   Int# -> Char#
   with code_size = 0

primop   Int2WordOp "int2Word#" GenPrimOp Int# -> Word#
   with code_size = 0

primop   Int2FloatOp   "int2Float#"      GenPrimOp  Int# -> Float#
primop   Int2DoubleOp   "int2Double#"          GenPrimOp  Int# -> Double#

primop   Word2FloatOp   "word2Float#"      GenPrimOp  Word# -> Float#
primop   Word2DoubleOp   "word2Double#"          GenPrimOp  Word# -> Double#

primop   ISllOp   "uncheckedIShiftL#" GenPrimOp  Int# -> Int# -> Int#
         {Shift left.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   ISraOp   "uncheckedIShiftRA#" GenPrimOp Int# -> Int# -> Int#
         {Shift right arithmetic.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   ISrlOp   "uncheckedIShiftRL#" GenPrimOp Int# -> Int# -> Int#
         {Shift right logical.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}

------------------------------------------------------------------------
section "Word#"
        {Operations on native-sized unsigned words (32+ bits).}
------------------------------------------------------------------------

primtype Word#

primop   WordAddOp   "plusWord#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   WordAddCOp   "addWordC#"   GenPrimOp   Word# -> Word# -> (# Word#, Int# #)
         {Add unsigned integers reporting overflow.
          The first element of the pair is the result.  The second element is
          the carry flag, which is nonzero on overflow. See also {\tt plusWord2#}.}
   with code_size = 2
        commutable = True

primop   WordSubCOp   "subWordC#"   GenPrimOp   Word# -> Word# -> (# Word#, Int# #)
         {Subtract unsigned integers reporting overflow.
          The first element of the pair is the result.  The second element is
          the carry flag, which is nonzero on overflow.}
   with code_size = 2

primop   WordAdd2Op   "plusWord2#"   GenPrimOp   Word# -> Word# -> (# Word#, Word# #)
         {Add unsigned integers, with the high part (carry) in the first
          component of the returned pair and the low part in the second
          component of the pair. See also {\tt addWordC#}.}
   with code_size = 2
        commutable = True

primop   WordSubOp   "minusWord#"   Dyadic   Word# -> Word# -> Word#

primop   WordMulOp   "timesWord#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

-- Returns (# high, low #)
primop   WordMul2Op  "timesWord2#"   GenPrimOp
   Word# -> Word# -> (# Word#, Word# #)
   with commutable = True

primop   WordQuotOp   "quotWord#"   Dyadic   Word# -> Word# -> Word#
   with can_fail = True

primop   WordRemOp   "remWord#"   Dyadic   Word# -> Word# -> Word#
   with can_fail = True

primop   WordQuotRemOp "quotRemWord#" GenPrimOp
   Word# -> Word# -> (# Word#, Word# #)
   with can_fail = True

primop   WordQuotRem2Op "quotRemWord2#" GenPrimOp
   Word# -> Word# -> Word# -> (# Word#, Word# #)
         { Takes high word of dividend, then low word of dividend, then divisor.
           Requires that high word < divisor.}
   with can_fail = True

primop   AndOp   "and#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   OrOp   "or#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   XorOp   "xor#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   NotOp   "not#"   Monadic   Word# -> Word#

primop   SllOp   "uncheckedShiftL#"   GenPrimOp   Word# -> Int# -> Word#
         {Shift left logical.   Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   SrlOp   "uncheckedShiftRL#"   GenPrimOp   Word# -> Int# -> Word#
         {Shift right logical.   Result undefined if shift  amount is not
          in the range 0 to word size - 1 inclusive.}

primop   Word2IntOp   "word2Int#"   GenPrimOp   Word# -> Int#
   with code_size = 0

primop   WordGtOp   "gtWord#"   Compare   Word# -> Word# -> Int#
primop   WordGeOp   "geWord#"   Compare   Word# -> Word# -> Int#
primop   WordEqOp   "eqWord#"   Compare   Word# -> Word# -> Int#
primop   WordNeOp   "neWord#"   Compare   Word# -> Word# -> Int#
primop   WordLtOp   "ltWord#"   Compare   Word# -> Word# -> Int#
primop   WordLeOp   "leWord#"   Compare   Word# -> Word# -> Int#

primop   PopCnt8Op   "popCnt8#"   Monadic   Word# -> Word#
    {Count the number of set bits in the lower 8 bits of a word.}
primop   PopCnt16Op   "popCnt16#"   Monadic   Word# -> Word#
    {Count the number of set bits in the lower 16 bits of a word.}
primop   PopCnt32Op   "popCnt32#"   Monadic   Word# -> Word#
    {Count the number of set bits in the lower 32 bits of a word.}
primop   PopCnt64Op   "popCnt64#"   GenPrimOp   WORD64 -> Word#
    {Count the number of set bits in a 64-bit word.}
primop   PopCntOp   "popCnt#"   Monadic   Word# -> Word#
    {Count the number of set bits in a word.}

primop   Pdep8Op   "pdep8#"   Dyadic   Word# -> Word# -> Word#
    {Deposit bits to lower 8 bits of a word at locations specified by a mask.}
primop   Pdep16Op   "pdep16#"   Dyadic   Word# -> Word# -> Word#
    {Deposit bits to lower 16 bits of a word at locations specified by a mask.}
primop   Pdep32Op   "pdep32#"   Dyadic   Word# -> Word# -> Word#
    {Deposit bits to lower 32 bits of a word at locations specified by a mask.}
primop   Pdep64Op   "pdep64#"   GenPrimOp   WORD64 -> WORD64 -> WORD64
    {Deposit bits to a word at locations specified by a mask.}
primop   PdepOp   "pdep#"   Dyadic   Word# -> Word# -> Word#
    {Deposit bits to a word at locations specified by a mask.}

primop   Pext8Op   "pext8#"   Dyadic   Word# -> Word# -> Word#
    {Extract bits from lower 8 bits of a word at locations specified by a mask.}
primop   Pext16Op   "pext16#"   Dyadic   Word# -> Word# -> Word#
    {Extract bits from lower 16 bits of a word at locations specified by a mask.}
primop   Pext32Op   "pext32#"   Dyadic   Word# -> Word# -> Word#
    {Extract bits from lower 32 bits of a word at locations specified by a mask.}
primop   Pext64Op   "pext64#"   GenPrimOp   WORD64 -> WORD64 -> WORD64
    {Extract bits from a word at locations specified by a mask.}
primop   PextOp   "pext#"   Dyadic   Word# -> Word# -> Word#
    {Extract bits from a word at locations specified by a mask.}

primop   Clz8Op   "clz8#" Monadic   Word# -> Word#
    {Count leading zeros in the lower 8 bits of a word.}
primop   Clz16Op   "clz16#" Monadic   Word# -> Word#
    {Count leading zeros in the lower 16 bits of a word.}
primop   Clz32Op   "clz32#" Monadic   Word# -> Word#
    {Count leading zeros in the lower 32 bits of a word.}
primop   Clz64Op   "clz64#" GenPrimOp WORD64 -> Word#
    {Count leading zeros in a 64-bit word.}
primop   ClzOp     "clz#"   Monadic   Word# -> Word#
    {Count leading zeros in a word.}

primop   Ctz8Op   "ctz8#"  Monadic   Word# -> Word#
    {Count trailing zeros in the lower 8 bits of a word.}
primop   Ctz16Op   "ctz16#" Monadic   Word# -> Word#
    {Count trailing zeros in the lower 16 bits of a word.}
primop   Ctz32Op   "ctz32#" Monadic   Word# -> Word#
    {Count trailing zeros in the lower 32 bits of a word.}
primop   Ctz64Op   "ctz64#" GenPrimOp WORD64 -> Word#
    {Count trailing zeros in a 64-bit word.}
primop   CtzOp     "ctz#"   Monadic   Word# -> Word#
    {Count trailing zeros in a word.}

primop   BSwap16Op   "byteSwap16#"   Monadic   Word# -> Word#
    {Swap bytes in the lower 16 bits of a word. The higher bytes are undefined. }
primop   BSwap32Op   "byteSwap32#"   Monadic   Word# -> Word#
    {Swap bytes in the lower 32 bits of a word. The higher bytes are undefined. }
primop   BSwap64Op   "byteSwap64#"   Monadic   WORD64 -> WORD64
    {Swap bytes in a 64 bits of a word.}
primop   BSwapOp     "byteSwap#"     Monadic   Word# -> Word#
    {Swap bytes in a word.}

primop   BRev8Op    "bitReverse8#"   Monadic   Word# -> Word#
    {Reverse the order of the bits in a 8-bit word.}
primop   BRev16Op   "bitReverse16#"   Monadic   Word# -> Word#
    {Reverse the order of the bits in a 16-bit word.}
primop   BRev32Op   "bitReverse32#"   Monadic   Word# -> Word#
    {Reverse the order of the bits in a 32-bit word.}
primop   BRev64Op   "bitReverse64#"   Monadic   WORD64 -> WORD64
    {Reverse the order of the bits in a 64-bit word.}
primop   BRevOp     "bitReverse#"     Monadic   Word# -> Word#
    {Reverse the order of the bits in a word.}

------------------------------------------------------------------------
section "Narrowings"
        {Explicit narrowing of native-sized ints or words.}
------------------------------------------------------------------------

primop   Narrow8IntOp      "narrow8Int#"      Monadic   Int# -> Int#
primop   Narrow16IntOp     "narrow16Int#"     Monadic   Int# -> Int#
primop   Narrow32IntOp     "narrow32Int#"     Monadic   Int# -> Int#
primop   Narrow8WordOp     "narrow8Word#"     Monadic   Word# -> Word#
primop   Narrow16WordOp    "narrow16Word#"    Monadic   Word# -> Word#
primop   Narrow32WordOp    "narrow32Word#"    Monadic   Word# -> Word#

------------------------------------------------------------------------
section "Double#"
        {Operations on double-precision (64 bit) floating-point numbers.}
------------------------------------------------------------------------

primtype Double#

primop   DoubleGtOp ">##"   Compare   Double# -> Double# -> Int#
   with fixity = infix 4

primop   DoubleGeOp ">=##"   Compare   Double# -> Double# -> Int#
   with fixity = infix 4

primop DoubleEqOp "==##"   Compare
   Double# -> Double# -> Int#
   with commutable = True
        fixity = infix 4

primop DoubleNeOp "/=##"   Compare
   Double# -> Double# -> Int#
   with commutable = True
        fixity = infix 4

primop   DoubleLtOp "<##"   Compare   Double# -> Double# -> Int#
   with fixity = infix 4

primop   DoubleLeOp "<=##"   Compare   Double# -> Double# -> Int#
   with fixity = infix 4

primop   DoubleAddOp   "+##"   Dyadic
   Double# -> Double# -> Double#
   with commutable = True
        fixity = infixl 6

primop   DoubleSubOp   "-##"   Dyadic   Double# -> Double# -> Double#
   with fixity = infixl 6

primop   DoubleMulOp   "*##"   Dyadic
   Double# -> Double# -> Double#
   with commutable = True
        fixity = infixl 7

primop   DoubleDivOp   "/##"   Dyadic
   Double# -> Double# -> Double#
   with can_fail = True
        fixity = infixl 7

primop   DoubleNegOp   "negateDouble#"  Monadic   Double# -> Double#

primop   DoubleFabsOp  "fabsDouble#"    Monadic   Double# -> Double#

primop   Double2IntOp   "double2Int#"          GenPrimOp  Double# -> Int#
   {Truncates a {\tt Double#} value to the nearest {\tt Int#}.
    Results are undefined if the truncation if truncation yields
    a value outside the range of {\tt Int#}.}

primop   Double2FloatOp   "double2Float#" GenPrimOp Double# -> Float#

primop   DoubleExpOp   "expDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleExpM1Op "expm1Double#"    Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleLogOp   "logDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleLog1POp   "log1pDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleSqrtOp   "sqrtDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleSinOp   "sinDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleCosOp   "cosDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleTanOp   "tanDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAsinOp   "asinDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleAcosOp   "acosDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleAtanOp   "atanDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleSinhOp   "sinhDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleCoshOp   "coshDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleTanhOp   "tanhDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAsinhOp   "asinhDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAcoshOp   "acoshDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAtanhOp   "atanhDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoublePowerOp   "**##" Dyadic
   Double# -> Double# -> Double#
   {Exponentiation.}
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleDecode_2IntOp   "decodeDouble_2Int#" GenPrimOp
   Double# -> (# Int#, Word#, Word#, Int# #)
   {Convert to integer.
    First component of the result is -1 or 1, indicating the sign of the
    mantissa. The next two are the high and low 32 bits of the mantissa
    respectively, and the last is the exponent.}
   with out_of_line = True

primop   DoubleDecode_Int64Op   "decodeDouble_Int64#" GenPrimOp
   Double# -> (# INT64, Int# #)
   {Decode {\tt Double\#} into mantissa and base-2 exponent.}
   with out_of_line = True

------------------------------------------------------------------------
section "Float#"
        {Operations on single-precision (32-bit) floating-point numbers.}
------------------------------------------------------------------------

primtype Float#

primop   FloatGtOp  "gtFloat#"   Compare   Float# -> Float# -> Int#
primop   FloatGeOp  "geFloat#"   Compare   Float# -> Float# -> Int#

primop   FloatEqOp  "eqFloat#"   Compare
   Float# -> Float# -> Int#
   with commutable = True

primop   FloatNeOp  "neFloat#"   Compare
   Float# -> Float# -> Int#
   with commutable = True

primop   FloatLtOp  "ltFloat#"   Compare   Float# -> Float# -> Int#
primop   FloatLeOp  "leFloat#"   Compare   Float# -> Float# -> Int#

primop   FloatAddOp   "plusFloat#"      Dyadic
   Float# -> Float# -> Float#
   with commutable = True

primop   FloatSubOp   "minusFloat#"      Dyadic      Float# -> Float# -> Float#

primop   FloatMulOp   "timesFloat#"      Dyadic
   Float# -> Float# -> Float#
   with commutable = True

primop   FloatDivOp   "divideFloat#"      Dyadic
   Float# -> Float# -> Float#
   with can_fail = True

primop   FloatNegOp   "negateFloat#"      Monadic    Float# -> Float#

primop   FloatFabsOp  "fabsFloat#"        Monadic    Float# -> Float#

primop   Float2IntOp   "float2Int#"      GenPrimOp  Float# -> Int#
   {Truncates a {\tt Float#} value to the nearest {\tt Int#}.
    Results are undefined if the truncation if truncation yields
    a value outside the range of {\tt Int#}.}

primop   FloatExpOp   "expFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatExpM1Op   "expm1Float#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatLogOp   "logFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatLog1POp  "log1pFloat#"     Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatSqrtOp   "sqrtFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatSinOp   "sinFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatCosOp   "cosFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatTanOp   "tanFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAsinOp   "asinFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatAcosOp   "acosFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatAtanOp   "atanFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatSinhOp   "sinhFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatCoshOp   "coshFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatTanhOp   "tanhFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAsinhOp   "asinhFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAcoshOp   "acoshFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAtanhOp   "atanhFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatPowerOp   "powerFloat#"      Dyadic
   Float# -> Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   Float2DoubleOp   "float2Double#" GenPrimOp  Float# -> Double#

primop   FloatDecode_IntOp   "decodeFloat_Int#" GenPrimOp
   Float# -> (# Int#, Int# #)
   {Convert to integers.
    First {\tt Int\#} in result is the mantissa; second is the exponent.}
   with out_of_line = True

------------------------------------------------------------------------
section "Arrays"
        {Operations on {\tt Array\#}.}
------------------------------------------------------------------------

primtype Array# a

primtype MutableArray# s a

primop  NewArrayOp "newArray#" GenPrimOp
   Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
   {Create a new mutable array with the specified number of elements,
    in the specified state thread,
    with each element containing the specified initial value.}
   with
   out_of_line = True
   has_side_effects = True

primop  SameMutableArrayOp "sameMutableArray#" GenPrimOp
   MutableArray# s a -> MutableArray# s a -> Int#

primop  ReadArrayOp "readArray#" GenPrimOp
   MutableArray# s a -> Int# -> State# s -> (# State# s, a #)
   {Read from specified index of mutable array. Result is not yet evaluated.}
   with
   has_side_effects = True
   can_fail         = True

primop  WriteArrayOp "writeArray#" GenPrimOp
   MutableArray# s a -> Int# -> a -> State# s -> State# s
   {Write to specified index of mutable array.}
   with
   has_side_effects = True
   can_fail         = True
   code_size        = 2 -- card update too

primop  SizeofArrayOp "sizeofArray#" GenPrimOp
   Array# a -> Int#
   {Return the number of elements in the array.}

primop  SizeofMutableArrayOp "sizeofMutableArray#" GenPrimOp
   MutableArray# s a -> Int#
   {Return the number of elements in the array.}

primop  IndexArrayOp "indexArray#" GenPrimOp
   Array# a -> Int# -> (# a #)
   {Read from the specified index of an immutable array. The result is packaged
    into an unboxed unary tuple; the result itself is not yet
    evaluated. Pattern matching on the tuple forces the indexing of the
    array to happen but does not evaluate the element itself. Evaluating
    the thunk prevents additional thunks from building up on the
    heap. Avoiding these thunks, in turn, reduces references to the
    argument array, allowing it to be garbage collected more promptly.}
   with
   can_fail         = True

primop  UnsafeFreezeArrayOp "unsafeFreezeArray#" GenPrimOp
   MutableArray# s a -> State# s -> (# State# s, Array# a #)
   {Make a mutable array immutable, without copying.}
   with
   has_side_effects = True

primop  UnsafeThawArrayOp  "unsafeThawArray#" GenPrimOp
   Array# a -> State# s -> (# State# s, MutableArray# s a #)
   {Make an immutable array mutable, without copying.}
   with
   out_of_line = True
   has_side_effects = True

primop  CopyArrayOp "copyArray#" GenPrimOp
  Array# a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  {Given a source array, an offset into the source array, a
   destination array, an offset into the destination array, and a
   number of elements to copy, copy the elements from the source array
   to the destination array. Both arrays must fully contain the
   specified ranges, but this is not checked. The two arrays must not
   be the same array in different states, but this is not checked
   either.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CopyMutableArrayOp "copyMutableArray#" GenPrimOp
  MutableArray# s a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  {Given a source array, an offset into the source array, a
   destination array, an offset into the destination array, and a
   number of elements to copy, copy the elements from the source array
   to the destination array. Both arrays must fully contain the
   specified ranges, but this is not checked. In the case where
   the source and destination are the same array the source and
   destination regions may overlap.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneArrayOp "cloneArray#" GenPrimOp
  Array# a -> Int# -> Int# -> Array# a
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneMutableArrayOp "cloneMutableArray#" GenPrimOp
  MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  FreezeArrayOp "freezeArray#" GenPrimOp
  MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, Array# a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  ThawArrayOp "thawArray#" GenPrimOp
  Array# a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop CasArrayOp  "casArray#" GenPrimOp
   MutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
   {Given an array, an offset, the expected old value, and
    the new value, perform an atomic compare and swap (i.e. write the new
    value if the current value and the old value are the same pointer).
    Returns 0 if the swap succeeds and 1 if it fails. Additionally, returns
    the element at the offset after the operation completes. This means that
    on a success the new value is returned, and on a failure the actual old
    value (not the expected one) is returned. Implies a full memory barrier.
    The use of a pointer equality on a lifted value makes this function harder
    to use correctly than {\tt casIntArray\#}. All of the difficulties
    of using {\tt reallyUnsafePtrEquality\#} correctly apply to
    {\tt casArray\#} as well.
   }
   with
   out_of_line = True
   has_side_effects = True


------------------------------------------------------------------------
section "Small Arrays"

        {Operations on {\tt SmallArray\#}. A {\tt SmallArray\#} works
         just like an {\tt Array\#}, but with different space use and
         performance characteristics (that are often useful with small
         arrays). The {\tt SmallArray\#} and {\tt SmallMutableArray#}
         lack a `card table'. The purpose of a card table is to avoid
         having to scan every element of the array on each GC by
         keeping track of which elements have changed since the last GC
         and only scanning those that have changed. So the consequence
         of there being no card table is that the representation is
         somewhat smaller and the writes are somewhat faster (because
         the card table does not need to be updated). The disadvantage
         of course is that for a {\tt SmallMutableArray#} the whole
         array has to be scanned on each GC. Thus it is best suited for
         use cases where the mutable array is not long lived, e.g.
         where a mutable array is initialised quickly and then frozen
         to become an immutable {\tt SmallArray\#}.
        }

------------------------------------------------------------------------

primtype SmallArray# a

primtype SmallMutableArray# s a

primop  NewSmallArrayOp "newSmallArray#" GenPrimOp
   Int# -> a -> State# s -> (# State# s, SmallMutableArray# s a #)
   {Create a new mutable array with the specified number of elements,
    in the specified state thread,
    with each element containing the specified initial value.}
   with
   out_of_line = True
   has_side_effects = True

primop  SameSmallMutableArrayOp "sameSmallMutableArray#" GenPrimOp
   SmallMutableArray# s a -> SmallMutableArray# s a -> Int#

primop  ShrinkSmallMutableArrayOp_Char "shrinkSmallMutableArray#" GenPrimOp
   SmallMutableArray# s a -> Int# -> State# s -> State# s
   {Shrink mutable array to new specified size, in
    the specified state thread. The new size argument must be less than or
    equal to the current size as reported by {\tt sizeofSmallMutableArray\#}.}
   with out_of_line = True
        has_side_effects = True

primop  ReadSmallArrayOp "readSmallArray#" GenPrimOp
   SmallMutableArray# s a -> Int# -> State# s -> (# State# s, a #)
   {Read from specified index of mutable array. Result is not yet evaluated.}
   with
   has_side_effects = True
   can_fail         = True

primop  WriteSmallArrayOp "writeSmallArray#" GenPrimOp
   SmallMutableArray# s a -> Int# -> a -> State# s -> State# s
   {Write to specified index of mutable array.}
   with
   has_side_effects = True
   can_fail         = True

primop  SizeofSmallArrayOp "sizeofSmallArray#" GenPrimOp
   SmallArray# a -> Int#
   {Return the number of elements in the array.}

primop  SizeofSmallMutableArrayOp "sizeofSmallMutableArray#" GenPrimOp
   SmallMutableArray# s a -> Int#
   {Return the number of elements in the array. Note that this is deprecated
   as it is unsafe in the presence of resize operations on the
   same byte array.}
   with deprecated_msg = { Use 'getSizeofSmallMutableArray#' instead }

primop  GetSizeofSmallMutableArrayOp "getSizeofSmallMutableArray#" GenPrimOp
   SmallMutableArray# s a -> State# s -> (# State# s, Int# #)
   {Return the number of elements in the array.}

primop  IndexSmallArrayOp "indexSmallArray#" GenPrimOp
   SmallArray# a -> Int# -> (# a #)
   {Read from specified index of immutable array. Result is packaged into
    an unboxed singleton; the result itself is not yet evaluated.}
   with
   can_fail         = True

primop  UnsafeFreezeSmallArrayOp "unsafeFreezeSmallArray#" GenPrimOp
   SmallMutableArray# s a -> State# s -> (# State# s, SmallArray# a #)
   {Make a mutable array immutable, without copying.}
   with
   has_side_effects = True

primop  UnsafeThawSmallArrayOp  "unsafeThawSmallArray#" GenPrimOp
   SmallArray# a -> State# s -> (# State# s, SmallMutableArray# s a #)
   {Make an immutable array mutable, without copying.}
   with
   out_of_line = True
   has_side_effects = True

-- The code_size is only correct for the case when the copy family of
-- primops aren't inlined. It would be nice to keep track of both.

primop  CopySmallArrayOp "copySmallArray#" GenPrimOp
  SmallArray# a -> Int# -> SmallMutableArray# s a -> Int# -> Int# -> State# s -> State# s
  {Given a source array, an offset into the source array, a
   destination array, an offset into the destination array, and a
   number of elements to copy, copy the elements from the source array
   to the destination array. Both arrays must fully contain the
   specified ranges, but this is not checked. The two arrays must not
   be the same array in different states, but this is not checked
   either.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CopySmallMutableArrayOp "copySmallMutableArray#" GenPrimOp
  SmallMutableArray# s a -> Int# -> SmallMutableArray# s a -> Int# -> Int# -> State# s -> State# s
  {Given a source array, an offset into the source array, a
   destination array, an offset into the destination array, and a
   number of elements to copy, copy the elements from the source array
   to the destination array. The source and destination arrays can
   refer to the same array. Both arrays must fully contain the
   specified ranges, but this is not checked.
   The regions are allowed to overlap, although this is only possible when the same
   array is provided as both the source and the destination. }
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneSmallArrayOp "cloneSmallArray#" GenPrimOp
  SmallArray# a -> Int# -> Int# -> SmallArray# a
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneSmallMutableArrayOp "cloneSmallMutableArray#" GenPrimOp
  SmallMutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  FreezeSmallArrayOp "freezeSmallArray#" GenPrimOp
  SmallMutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, SmallArray# a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  ThawSmallArrayOp "thawSmallArray#" GenPrimOp
  SmallArray# a -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop CasSmallArrayOp  "casSmallArray#" GenPrimOp
   SmallMutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
   {Unsafe, machine-level atomic compare and swap on an element within an array.
    See the documentation of {\tt casArray\#}.}
   with
   out_of_line = True
   has_side_effects = True

------------------------------------------------------------------------
section "Byte Arrays"
        {Operations on {\tt ByteArray\#}. A {\tt ByteArray\#} is a just a region of
         raw memory in the garbage-collected heap, which is not
         scanned for pointers. It carries its own size (in bytes).
         There are
         three sets of operations for accessing byte array contents:
         index for reading from immutable byte arrays, and read/write
         for mutable byte arrays.  Each set contains operations for a
         range of useful primitive data types.  Each operation takes
         an offset measured in terms of the size of the primitive type
         being read or written.}

------------------------------------------------------------------------

primtype ByteArray#

primtype MutableByteArray# s

primop  NewByteArrayOp_Char "newByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutableByteArray# s #)
   {Create a new mutable byte array of specified size (in bytes), in
    the specified state thread.}
   with out_of_line = True
        has_side_effects = True

primop  NewPinnedByteArrayOp_Char "newPinnedByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutableByteArray# s #)
   {Create a mutable byte array that the GC guarantees not to move.}
   with out_of_line = True
        has_side_effects = True

primop  NewAlignedPinnedByteArrayOp_Char "newAlignedPinnedByteArray#" GenPrimOp
   Int# -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
   {Create a mutable byte array, aligned by the specified amount, that the GC guarantees not to move.}
   with out_of_line = True
        has_side_effects = True

primop  MutableByteArrayIsPinnedOp "isMutableByteArrayPinned#" GenPrimOp
   MutableByteArray# s -> Int#
   {Determine whether a {\tt MutableByteArray\#} is guaranteed not to move
   during GC.}
   with out_of_line = True

primop  ByteArrayIsPinnedOp "isByteArrayPinned#" GenPrimOp
   ByteArray# -> Int#
   {Determine whether a {\tt ByteArray\#} is guaranteed not to move during GC.}
   with out_of_line = True

primop  ByteArrayContents_Char "byteArrayContents#" GenPrimOp
   ByteArray# -> Addr#
   {Intended for use with pinned arrays; otherwise very unsafe!}

primop  SameMutableByteArrayOp "sameMutableByteArray#" GenPrimOp
   MutableByteArray# s -> MutableByteArray# s -> Int#

primop  ShrinkMutableByteArrayOp_Char "shrinkMutableByteArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> State# s
   {Shrink mutable byte array to new specified size (in bytes), in
    the specified state thread. The new size argument must be less than or
    equal to the current size as reported by {\tt sizeofMutableByteArray\#}.}
   with out_of_line = True
        has_side_effects = True

primop  ResizeMutableByteArrayOp_Char "resizeMutableByteArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s,MutableByteArray# s #)
   {Resize (unpinned) mutable byte array to new specified size (in bytes).
    The returned {\tt MutableByteArray\#} is either the original
    {\tt MutableByteArray\#} resized in-place or, if not possible, a newly
    allocated (unpinned) {\tt MutableByteArray\#} (with the original content
    copied over).

    To avoid undefined behaviour, the original {\tt MutableByteArray\#} shall
    not be accessed anymore after a {\tt resizeMutableByteArray\#} has been
    performed.  Moreover, no reference to the old one should be kept in order
    to allow garbage collection of the original {\tt MutableByteArray\#} in
    case a new {\tt MutableByteArray\#} had to be allocated.}
   with out_of_line = True
        has_side_effects = True

primop  UnsafeFreezeByteArrayOp "unsafeFreezeByteArray#" GenPrimOp
   MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
   {Make a mutable byte array immutable, without copying.}
   with
   has_side_effects = True

primop  SizeofByteArrayOp "sizeofByteArray#" GenPrimOp
   ByteArray# -> Int#
   {Return the size of the array in bytes.}

primop  SizeofMutableByteArrayOp "sizeofMutableByteArray#" GenPrimOp
   MutableByteArray# s -> Int#
   {Return the size of the array in bytes. Note that this is deprecated as it is
   unsafe in the presence of resize operations on the same byte
   array.}
   with deprecated_msg = { Use 'getSizeofMutableByteArray#' instead }

primop  GetSizeofMutableByteArrayOp "getSizeofMutableByteArray#" GenPrimOp
   MutableByteArray# s -> State# s -> (# State# s, Int# #)
   {Return the number of elements in the array.}

primop IndexByteArrayOp_Char "indexCharArray#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 8-bit character; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_WideChar "indexWideCharArray#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 31-bit character; offset in 4-byte words.}
   with can_fail = True

primop IndexByteArrayOp_Int "indexIntArray#" GenPrimOp
   ByteArray# -> Int# -> Int#
   with can_fail = True

primop IndexByteArrayOp_Word "indexWordArray#" GenPrimOp
   ByteArray# -> Int# -> Word#
   with can_fail = True

primop IndexByteArrayOp_Addr "indexAddrArray#" GenPrimOp
   ByteArray# -> Int# -> Addr#
   with can_fail = True

primop IndexByteArrayOp_Float "indexFloatArray#" GenPrimOp
   ByteArray# -> Int# -> Float#
   with can_fail = True

primop IndexByteArrayOp_Double "indexDoubleArray#" GenPrimOp
   ByteArray# -> Int# -> Double#
   with can_fail = True

primop IndexByteArrayOp_StablePtr "indexStablePtrArray#" GenPrimOp
   ByteArray# -> Int# -> StablePtr# a
   with can_fail = True

primop IndexByteArrayOp_Int8 "indexInt8Array#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read 8-bit integer; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Int16 "indexInt16Array#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read 16-bit integer; offset in 16-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Int32 "indexInt32Array#" GenPrimOp
   ByteArray# -> Int# -> INT32
   {Read 32-bit integer; offset in 32-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Int64 "indexInt64Array#" GenPrimOp
   ByteArray# -> Int# -> INT64
   {Read 64-bit integer; offset in 64-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word8 "indexWord8Array#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read 8-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word16 "indexWord16Array#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read 16-bit word; offset in 16-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word32 "indexWord32Array#" GenPrimOp
   ByteArray# -> Int# -> WORD32
   {Read 32-bit word; offset in 32-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word64 "indexWord64Array#" GenPrimOp
   ByteArray# -> Int# -> WORD64
   {Read 64-bit word; offset in 64-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsChar "indexWord8ArrayAsChar#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 8-bit character; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWideChar "indexWord8ArrayAsWideChar#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 31-bit character; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsAddr "indexWord8ArrayAsAddr#" GenPrimOp
   ByteArray# -> Int# -> Addr#
   {Read address; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsFloat "indexWord8ArrayAsFloat#" GenPrimOp
   ByteArray# -> Int# -> Float#
   {Read float; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsDouble "indexWord8ArrayAsDouble#" GenPrimOp
   ByteArray# -> Int# -> Double#
   {Read double; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsStablePtr "indexWord8ArrayAsStablePtr#" GenPrimOp
   ByteArray# -> Int# -> StablePtr# a
   {Read stable pointer; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt16 "indexWord8ArrayAsInt16#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read 16-bit int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt32 "indexWord8ArrayAsInt32#" GenPrimOp
   ByteArray# -> Int# -> INT32
   {Read 32-bit int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt64 "indexWord8ArrayAsInt64#" GenPrimOp
   ByteArray# -> Int# -> INT64
   {Read 64-bit int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt "indexWord8ArrayAsInt#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord16 "indexWord8ArrayAsWord16#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read 16-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord32 "indexWord8ArrayAsWord32#" GenPrimOp
   ByteArray# -> Int# -> WORD32
   {Read 32-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord64 "indexWord8ArrayAsWord64#" GenPrimOp
   ByteArray# -> Int# -> WORD64
   {Read 64-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord "indexWord8ArrayAsWord#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read word; offset in bytes.}
   with can_fail = True

primop  ReadByteArrayOp_Char "readCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 8-bit character; offset in bytes.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_WideChar "readWideCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 31-bit character; offset in 4-byte words.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int "readIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   {Read integer; offset in machine words.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word "readWordArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   {Read word; offset in machine words.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Addr "readAddrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Float "readFloatArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Double "readDoubleArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_StablePtr "readStablePtrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# a #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int8 "readInt8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int16 "readInt16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int32 "readInt32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int64 "readInt64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8 "readWord8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word16 "readWord16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word32 "readWord32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word64 "readWord64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsChar "readWord8ArrayAsChar#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWideChar "readWord8ArrayAsWideChar#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsAddr "readWord8ArrayAsAddr#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsFloat "readWord8ArrayAsFloat#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsDouble "readWord8ArrayAsDouble#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsStablePtr "readWord8ArrayAsStablePtr#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# a #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt16 "readWord8ArrayAsInt16#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt32 "readWord8ArrayAsInt32#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt64 "readWord8ArrayAsInt64#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt "readWord8ArrayAsInt#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord16 "readWord8ArrayAsWord16#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord32 "readWord8ArrayAsWord32#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord64 "readWord8ArrayAsWord64#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord "readWord8ArrayAsWord#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Char "writeCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   {Write 8-bit character; offset in bytes.}
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_WideChar "writeWideCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   {Write 31-bit character; offset in 4-byte words.}
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int "writeIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word "writeWordArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Addr "writeAddrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Float "writeFloatArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Double "writeDoubleArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_StablePtr "writeStablePtrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int8 "writeInt8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int16 "writeInt16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int32 "writeInt32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int64 "writeInt64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> INT64 -> State# s -> State# s
   with can_fail = True
        has_side_effects = True

primop  WriteByteArrayOp_Word8 "writeWord8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word16 "writeWord16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word32 "writeWord32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word64 "writeWord64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsChar "writeWord8ArrayAsChar#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWideChar "writeWord8ArrayAsWideChar#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsAddr "writeWord8ArrayAsAddr#" GenPrimOp
   MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsFloat "writeWord8ArrayAsFloat#" GenPrimOp
   MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsDouble "writeWord8ArrayAsDouble#" GenPrimOp
   MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsStablePtr "writeWord8ArrayAsStablePtr#" GenPrimOp
   MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt16 "writeWord8ArrayAsInt16#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt32 "writeWord8ArrayAsInt32#" GenPrimOp
   MutableByteArray# s -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt64 "writeWord8ArrayAsInt64#" GenPrimOp
   MutableByteArray# s -> Int# -> INT64 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt "writeWord8ArrayAsInt#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord16 "writeWord8ArrayAsWord16#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord32 "writeWord8ArrayAsWord32#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord64 "writeWord8ArrayAsWord64#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord "writeWord8ArrayAsWord#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  CompareByteArraysOp "compareByteArrays#" GenPrimOp
   ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
   {{\tt compareByteArrays# src1 src1_ofs src2 src2_ofs n} compares
    {\tt n} bytes starting at offset {\tt src1_ofs} in the first
    {\tt ByteArray#} {\tt src1} to the range of {\tt n} bytes
    (i.e. same length) starting at offset {\tt src2_ofs} of the second
    {\tt ByteArray#} {\tt src2}.  Both arrays must fully contain the
    specified ranges, but this is not checked.  Returns an {\tt Int#}
    less than, equal to, or greater than zero if the range is found,
    respectively, to be byte-wise lexicographically less than, to
    match, or be greater than the second range.}
   with
   can_fail = True

primop  CopyByteArrayOp "copyByteArray#" GenPrimOp
  ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {{\tt copyByteArray# src src_ofs dst dst_ofs n} copies the range
   starting at offset {\tt src_ofs} of length {\tt n} from the
   {\tt ByteArray#} {\tt src} to the {\tt MutableByteArray#} {\tt dst}
   starting at offset {\tt dst_ofs}.  Both arrays must fully contain
   the specified ranges, but this is not checked.  The two arrays must
   not be the same array in different states, but this is not checked
   either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyMutableByteArrayOp "copyMutableByteArray#" GenPrimOp
  MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the first MutableByteArray\# to the specified region in the second MutableByteArray\#.
   Both arrays must fully contain the specified ranges, but this is not checked. The regions are
   allowed to overlap, although this is only possible when the same array is provided
   as both the source and the destination.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }
  can_fail = True

primop  CopyByteArrayToAddrOp "copyByteArrayToAddr#" GenPrimOp
  ByteArray# -> Int# -> Addr# -> Int# -> State# s -> State# s
  {Copy a range of the ByteArray\# to the memory range starting at the Addr\#.
   The ByteArray\# and the memory region at Addr\# must fully contain the
   specified ranges, but this is not checked. The Addr\# must not point into the
   ByteArray\# (e.g. if the ByteArray\# were pinned), but this is not checked
   either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyMutableByteArrayToAddrOp "copyMutableByteArrayToAddr#" GenPrimOp
  MutableByteArray# s -> Int# -> Addr# -> Int# -> State# s -> State# s
  {Copy a range of the MutableByteArray\# to the memory range starting at the
   Addr\#. The MutableByteArray\# and the memory region at Addr\# must fully
   contain the specified ranges, but this is not checked. The Addr\# must not
   point into the MutableByteArray\# (e.g. if the MutableByteArray\# were
   pinned), but this is not checked either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyAddrToByteArrayOp "copyAddrToByteArray#" GenPrimOp
  Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a memory range starting at the Addr\# to the specified range in the
   MutableByteArray\#. The memory region at Addr\# and the ByteArray\# must fully
   contain the specified ranges, but this is not checked. The Addr\# must not
   point into the MutableByteArray\# (e.g. if the MutableByteArray\# were pinned),
   but this is not checked either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  SetByteArrayOp "setByteArray#" GenPrimOp
  MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
  {{\tt setByteArray# ba off len c} sets the byte range {\tt [off, off+len]} of
   the {\tt MutableByteArray#} to the byte {\tt c}.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }
  can_fail = True

-- Atomic operations

primop  AtomicReadByteArrayOp_Int "atomicReadIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array and an offset in machine words, read an element. The
    index is assumed to be in bounds. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop  AtomicWriteByteArrayOp_Int "atomicWriteIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   {Given an array and an offset in machine words, write an element. The
    index is assumed to be in bounds. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop CasByteArrayOp_Int "casIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, an offset in machine words, the expected old value, and
    the new value, perform an atomic compare and swap i.e. write the new
    value if the current value matches the provided old value. Returns
    the value of the element before the operation. Implies a full memory
    barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchAddByteArrayOp_Int "fetchAddIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to add,
    atomically add the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchSubByteArrayOp_Int "fetchSubIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to subtract,
    atomically subtract the value to the element. Returns the value of
    the element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchAndByteArrayOp_Int "fetchAndIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to AND,
    atomically AND the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchNandByteArrayOp_Int "fetchNandIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to NAND,
    atomically NAND the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchOrByteArrayOp_Int "fetchOrIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to OR,
    atomically OR the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchXorByteArrayOp_Int "fetchXorIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to XOR,
    atomically XOR the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True


------------------------------------------------------------------------
section "Arrays of arrays"
        {Operations on {\tt ArrayArray\#}. An {\tt ArrayArray\#} contains references to {\em unpointed}
         arrays, such as {\tt ByteArray\#s}. Hence, it is not parameterised by the element types,
         just like a {\tt ByteArray\#}, but it needs to be scanned during GC, just like an {\tt Array\#}.
         We represent an {\tt ArrayArray\#} exactly as a {\tt Array\#}, but provide element-type-specific
         indexing, reading, and writing.}
------------------------------------------------------------------------

primtype ArrayArray#

primtype MutableArrayArray# s

primop  NewArrayArrayOp "newArrayArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutableArrayArray# s #)
   {Create a new mutable array of arrays with the specified number of elements,
    in the specified state thread, with each element recursively referring to the
    newly created array.}
   with
   out_of_line = True
   has_side_effects = True

primop  SameMutableArrayArrayOp "sameMutableArrayArray#" GenPrimOp
   MutableArrayArray# s -> MutableArrayArray# s -> Int#

primop  UnsafeFreezeArrayArrayOp "unsafeFreezeArrayArray#" GenPrimOp
   MutableArrayArray# s -> State# s -> (# State# s, ArrayArray# #)
   {Make a mutable array of arrays immutable, without copying.}
   with
   has_side_effects = True

primop  SizeofArrayArrayOp "sizeofArrayArray#" GenPrimOp
   ArrayArray# -> Int#
   {Return the number of elements in the array.}

primop  SizeofMutableArrayArrayOp "sizeofMutableArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int#
   {Return the number of elements in the array.}

primop IndexArrayArrayOp_ByteArray "indexByteArrayArray#" GenPrimOp
   ArrayArray# -> Int# -> ByteArray#
   with can_fail = True

primop IndexArrayArrayOp_ArrayArray "indexArrayArrayArray#" GenPrimOp
   ArrayArray# -> Int# -> ArrayArray#
   with can_fail = True

primop  ReadArrayArrayOp_ByteArray "readByteArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> State# s -> (# State# s, ByteArray# #)
   with has_side_effects = True
        can_fail = True

primop  ReadArrayArrayOp_MutableByteArray "readMutableByteArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
   with has_side_effects = True
        can_fail = True

primop  ReadArrayArrayOp_ArrayArray "readArrayArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> State# s -> (# State# s, ArrayArray# #)
   with has_side_effects = True
        can_fail = True

primop  ReadArrayArrayOp_MutableArrayArray "readMutableArrayArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableArrayArray# s #)
   with has_side_effects = True
        can_fail = True

primop  WriteArrayArrayOp_ByteArray "writeByteArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> ByteArray# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteArrayArrayOp_MutableByteArray "writeMutableByteArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> MutableByteArray# s -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteArrayArrayOp_ArrayArray "writeArrayArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> ArrayArray# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteArrayArrayOp_MutableArrayArray "writeMutableArrayArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> MutableArrayArray# s -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  CopyArrayArrayOp "copyArrayArray#" GenPrimOp
  ArrayArray# -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the ArrayArray\# to the specified region in the MutableArrayArray\#.
   Both arrays must fully contain the specified ranges, but this is not checked.
   The two arrays must not be the same array in different states, but this is not checked either.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CopyMutableArrayArrayOp "copyMutableArrayArray#" GenPrimOp
  MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the first MutableArrayArray# to the specified region in the second
   MutableArrayArray#.
   Both arrays must fully contain the specified ranges, but this is not checked.
   The regions are allowed to overlap, although this is only possible when the same
   array is provided as both the source and the destination.
   }
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

------------------------------------------------------------------------
section "Addr#"
------------------------------------------------------------------------

primtype Addr#
        { An arbitrary machine address assumed to point outside
         the garbage-collected heap. }

pseudoop "nullAddr#" Addr#
        { The null address. }

primop   AddrAddOp "plusAddr#" GenPrimOp Addr# -> Int# -> Addr#
primop   AddrSubOp "minusAddr#" GenPrimOp Addr# -> Addr# -> Int#
         {Result is meaningless if two {\tt Addr\#}s are so far apart that their
         difference doesn't fit in an {\tt Int\#}.}
primop   AddrRemOp "remAddr#" GenPrimOp Addr# -> Int# -> Int#
         {Return the remainder when the {\tt Addr\#} arg, treated like an {\tt Int\#},
          is divided by the {\tt Int\#} arg.}
primop   Addr2IntOp  "addr2Int#"     GenPrimOp   Addr# -> Int#
        {Coerce directly from address to int.}
   with code_size = 0
        deprecated_msg = { This operation is strongly deprecated. }
primop   Int2AddrOp   "int2Addr#"    GenPrimOp  Int# -> Addr#
        {Coerce directly from int to address.}
   with code_size = 0
        deprecated_msg = { This operation is strongly deprecated. }

primop   AddrGtOp  "gtAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrGeOp  "geAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrEqOp  "eqAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrNeOp  "neAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrLtOp  "ltAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrLeOp  "leAddr#"   Compare   Addr# -> Addr# -> Int#

primop IndexOffAddrOp_Char "indexCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char#
   {Reads 8-bit character; offset in bytes.}
   with can_fail = True

primop IndexOffAddrOp_WideChar "indexWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char#
   {Reads 31-bit character; offset in 4-byte words.}
   with can_fail = True

primop IndexOffAddrOp_Int "indexIntOffAddr#" GenPrimOp
   Addr# -> Int# -> Int#
   with can_fail = True

primop IndexOffAddrOp_Word "indexWordOffAddr#" GenPrimOp
   Addr# -> Int# -> Word#
   with can_fail = True

primop IndexOffAddrOp_Addr "indexAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> Addr#
   with can_fail = True

primop IndexOffAddrOp_Float "indexFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> Float#
   with can_fail = True

primop IndexOffAddrOp_Double "indexDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> Double#
   with can_fail = True

primop IndexOffAddrOp_StablePtr "indexStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> StablePtr# a
   with can_fail = True

primop IndexOffAddrOp_Int8 "indexInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> Int#
   with can_fail = True

primop IndexOffAddrOp_Int16 "indexInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int#
   with can_fail = True

primop IndexOffAddrOp_Int32 "indexInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> INT32
   with can_fail = True

primop IndexOffAddrOp_Int64 "indexInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> INT64
   with can_fail = True

primop IndexOffAddrOp_Word8 "indexWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word#
   with can_fail = True

primop IndexOffAddrOp_Word16 "indexWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word#
   with can_fail = True

primop IndexOffAddrOp_Word32 "indexWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD32
   with can_fail = True

primop IndexOffAddrOp_Word64 "indexWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD64
   with can_fail = True

primop ReadOffAddrOp_Char "readCharOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Char# #)
   {Reads 8-bit character; offset in bytes.}
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_WideChar "readWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Char# #)
   {Reads 31-bit character; offset in 4-byte words.}
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int "readIntOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word "readWordOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Addr "readAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Addr# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Float "readFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Float# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Double "readDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Double# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_StablePtr "readStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, StablePtr# a #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int8 "readInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int16 "readInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int32 "readInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, INT32 #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int64 "readInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, INT64 #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word8 "readWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word16 "readWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word32 "readWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, WORD32 #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word64 "readWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, WORD64 #)
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Char "writeCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_WideChar "writeWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int "writeIntOffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word "writeWordOffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Addr "writeAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Float "writeFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Double "writeDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_StablePtr "writeStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int8 "writeInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int16 "writeInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int32 "writeInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int64 "writeInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> INT64 -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word8 "writeWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word16 "writeWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word32 "writeWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word64 "writeWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

------------------------------------------------------------------------
section "Mutable variables"
        {Operations on MutVar\#s.}
------------------------------------------------------------------------

primtype MutVar# s a
        {A {\tt MutVar\#} behaves like a single-element mutable array.}

primop  NewMutVarOp "newMutVar#" GenPrimOp
   a -> State# s -> (# State# s, MutVar# s a #)
   {Create {\tt MutVar\#} with specified initial value in specified state thread.}
   with
   out_of_line = True
   has_side_effects = True

-- Note [Why MutVar# ops can't fail]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We don't label readMutVar# or writeMutVar# as can_fail.
-- This may seem a bit peculiar, because they surely *could*
-- fail spectacularly if passed a pointer to unallocated memory.
-- But MutVar#s are always correct by construction; we never
-- test if a pointer is valid before using it with these operations.
-- So we never have to worry about floating the pointer reference
-- outside a validity test. At the moment, has_side_effects blocks
-- up the relevant optimizations anyway, but we hope to draw finer
-- distinctions soon, which should improve matters for readMutVar#
-- at least.

primop  ReadMutVarOp "readMutVar#" GenPrimOp
   MutVar# s a -> State# s -> (# State# s, a #)
   {Read contents of {\tt MutVar\#}. Result is not yet evaluated.}
   with
   -- See Note [Why MutVar# ops can't fail]
   has_side_effects = True

primop  WriteMutVarOp "writeMutVar#"  GenPrimOp
   MutVar# s a -> a -> State# s -> State# s
   {Write contents of {\tt MutVar\#}.}
   with
   -- See Note [Why MutVar# ops can't fail]
   has_side_effects = True
   code_size = { primOpCodeSizeForeignCall } -- for the write barrier

primop  SameMutVarOp "sameMutVar#" GenPrimOp
   MutVar# s a -> MutVar# s a -> Int#

-- Note [Why not an unboxed tuple in atomicModifyMutVar2#?]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Looking at the type of atomicModifyMutVar2#, one might wonder why
-- it doesn't return an unboxed tuple. e.g.,
--
--   MutVar# s a -> (a -> (# a, b #)) -> State# s -> (# State# s, a, (# a, b #) #)
--
-- The reason is that atomicModifyMutVar2# relies on laziness for its atomicity.
-- Given a MutVar# containing x, atomicModifyMutVar2# merely replaces
-- its contents with a thunk of the form (fst (f x)). This can be done using an
-- atomic compare-and-swap as it is merely replacing a pointer.

primop  AtomicModifyMutVar2Op "atomicModifyMutVar2#" GenPrimOp
   MutVar# s a -> (a -> c) -> State# s -> (# State# s, a, c #)
   { Modify the contents of a {\tt MutVar\#}, returning the previous
     contents and the result of applying the given function to the
     previous contents. Note that this isn't strictly
     speaking the correct type for this function; it should really be
     {\tt MutVar\# s a -> (a -> (a,b)) -> State\# s -> (\# State\# s, a, (a, b) \#)},
     but we don't know about pairs here. }
   with
   out_of_line = True
   has_side_effects = True
   can_fail         = True

primop  AtomicModifyMutVar_Op "atomicModifyMutVar_#" GenPrimOp
   MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)
   { Modify the contents of a {\tt MutVar\#}, returning the previous
     contents and the result of applying the given function to the
     previous contents. }
   with
   out_of_line = True
   has_side_effects = True
   can_fail         = True

primop  CasMutVarOp "casMutVar#" GenPrimOp
  MutVar# s a -> a -> a -> State# s -> (# State# s, Int#, a #)
   with
   out_of_line = True
   has_side_effects = True

------------------------------------------------------------------------
section "Exceptions"
------------------------------------------------------------------------

-- Note [Strictness for mask/unmask/catch]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Consider this example, which comes from GHC.IO.Handle.Internals:
--    wantReadableHandle3 f ma b st
--      = case ... of
--          DEFAULT -> case ma of MVar a -> ...
--          0#      -> maskAsynchExceptions# (\st -> case ma of MVar a -> ...)
-- The outer case just decides whether to mask exceptions, but we don't want
-- thereby to hide the strictness in 'ma'!  Hence the use of strictApply1Dmd.

primop  CatchOp "catch#" GenPrimOp
          (State# RealWorld -> (# State# RealWorld, a #) )
       -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
       -> State# RealWorld
       -> (# State# RealWorld, a #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [ lazyApply1Dmd
                                                 , lazyApply2Dmd
                                                 , topDmd] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  RaiseOp "raise#" GenPrimOp
   b -> o
      -- NB: the type variable "o" is "a", but with OpenKind
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True
     -- raise# certainly throws a Haskell exception and hence has_side_effects
     -- It doesn't actually make much difference because the fact that it
     -- returns bottom independently ensures that we are careful not to discard
     -- it.  But still, it's better to say the Right Thing.

-- Note [Arithmetic exception primops]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The RTS provides several primops to raise specific exceptions (raiseDivZero#,
-- raiseUnderflow#, raiseOverflow#). These primops are meant to be used by the
-- package implementing arbitrary precision numbers (Natural,Integer). It can't
-- depend on `base` package to raise exceptions in a normal way because it would
-- create a package dependency circle (base <-> bignum package).
--
-- See #14664

primtype Void#

primop  RaiseDivZeroOp "raiseDivZero#" GenPrimOp
   Void# -> o
   {Raise a 'DivideByZero' arithmetic exception.}
      -- NB: the type variable "o" is "a", but with OpenKind
      -- See Note [Arithmetic exception primops]
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

primop  RaiseUnderflowOp "raiseUnderflow#" GenPrimOp
   Void# -> o
   {Raise an 'Underflow' arithmetic exception.}
      -- NB: the type variable "o" is "a", but with OpenKind
      -- See Note [Arithmetic exception primops]
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

primop  RaiseOverflowOp "raiseOverflow#" GenPrimOp
   Void# -> o
   {Raise an 'Overflow' arithmetic exception.}
      -- NB: the type variable "o" is "a", but with OpenKind
      -- See Note [Arithmetic exception primops]
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

-- raiseIO# needs to be a primop, because exceptions in the IO monad
-- must be *precise* - we don't want the strictness analyser turning
-- one kind of bottom into another, as it is allowed to do in pure code.
--
-- But we *do* want to know that it returns bottom after
-- being applied to two arguments, so that this function is strict in y
--     f x y | x>0       = raiseIO blah
--           | y>0       = return 1
--           | otherwise = return 2
--
-- TODO Check that the above notes on @f@ are valid. The function successfully
-- produces an IO exception when compiled without optimization. If we analyze
-- it as strict in @y@, won't we change that behavior under optimization?
-- I thought the rule was that it was okay to replace one valid imprecise
-- exception with another, but not to replace a precise exception with
-- an imprecise one (dfeuer, 2017-03-05).

primop  RaiseIOOp "raiseIO#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, b #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd, topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

primop  MaskAsyncExceptionsOp "maskAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  MaskUninterruptibleOp "maskUninterruptible#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topDiv }
   out_of_line = True
   has_side_effects = True

primop  UnmaskAsyncExceptionsOp "unmaskAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  MaskStatus "getMaskingState#" GenPrimOp
        State# RealWorld -> (# State# RealWorld, Int# #)
   with
   out_of_line = True
   has_side_effects = True

------------------------------------------------------------------------
section "STM-accessible Mutable Variables"
------------------------------------------------------------------------

primtype TVar# s a

primop  AtomicallyOp "atomically#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> State# RealWorld -> (# State# RealWorld, a #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

-- NB: retry#'s strictness information specifies it to diverge.
-- This lets the compiler perform some extra simplifications, since retry#
-- will technically never return.
--
-- This allows the simplifier to replace things like:
--   case retry# s1
--     (# s2, a #) -> e
-- with:
--   retry# s1
-- where 'e' would be unreachable anyway.  See #8091.
primop  RetryOp "retry#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, a #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

primop  CatchRetryOp "catchRetry#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   with
   strictness  = { \ _arity -> mkClosedStrictSig [ lazyApply1Dmd
                                                 , lazyApply1Dmd
                                                 , topDmd ] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  CatchSTMOp "catchSTM#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   with
   strictness  = { \ _arity -> mkClosedStrictSig [ lazyApply1Dmd
                                                 , lazyApply2Dmd
                                                 , topDmd ] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  NewTVarOp "newTVar#" GenPrimOp
       a
    -> State# s -> (# State# s, TVar# s a #)
   {Create a new {\tt TVar\#} holding a specified initial value.}
   with
   out_of_line  = True
   has_side_effects = True

primop  ReadTVarOp "readTVar#" GenPrimOp
       TVar# s a
    -> State# s -> (# State# s, a #)
   {Read contents of {\tt TVar\#}.  Result is not yet evaluated.}
   with
   out_of_line  = True
   has_side_effects = True

primop ReadTVarIOOp "readTVarIO#" GenPrimOp
       TVar# s a
    -> State# s -> (# State# s, a #)
   {Read contents of {\tt TVar\#} outside an STM transaction}
   with
   out_of_line      = True
   has_side_effects = True

primop  WriteTVarOp "writeTVar#" GenPrimOp
       TVar# s a
    -> a
    -> State# s -> State# s
   {Write contents of {\tt TVar\#}.}
   with
   out_of_line      = True
   has_side_effects = True

primop  SameTVarOp "sameTVar#" GenPrimOp
   TVar# s a -> TVar# s a -> Int#


------------------------------------------------------------------------
section "Synchronized Mutable Variables"
        {Operations on {\tt MVar\#}s. }
------------------------------------------------------------------------

primtype MVar# s a
        { A shared mutable variable ({\it not} the same as a {\tt MutVar\#}!).
        (Note: in a non-concurrent implementation, {\tt (MVar\# a)} can be
        represented by {\tt (MutVar\# (Maybe a))}.) }

primop  NewMVarOp "newMVar#"  GenPrimOp
   State# s -> (# State# s, MVar# s a #)
   {Create new {\tt MVar\#}; initially empty.}
   with
   out_of_line = True
   has_side_effects = True

primop  TakeMVarOp "takeMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, a #)
   {If {\tt MVar\#} is empty, block until it becomes full.
   Then remove and return its contents, and set it empty.}
   with
   out_of_line      = True
   has_side_effects = True

primop  TryTakeMVarOp "tryTakeMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, Int#, a #)
   {If {\tt MVar\#} is empty, immediately return with integer 0 and value undefined.
   Otherwise, return with integer 1 and contents of {\tt MVar\#}, and set {\tt MVar\#} empty.}
   with
   out_of_line      = True
   has_side_effects = True

primop  PutMVarOp "putMVar#" GenPrimOp
   MVar# s a -> a -> State# s -> State# s
   {If {\tt MVar\#} is full, block until it becomes empty.
   Then store value arg as its new contents.}
   with
   out_of_line      = True
   has_side_effects = True

primop  TryPutMVarOp "tryPutMVar#" GenPrimOp
   MVar# s a -> a -> State# s -> (# State# s, Int# #)
   {If {\tt MVar\#} is full, immediately return with integer 0.
    Otherwise, store value arg as {\tt MVar\#}'s new contents, and return with integer 1.}
   with
   out_of_line      = True
   has_side_effects = True

primop  ReadMVarOp "readMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, a #)
   {If {\tt MVar\#} is empty, block until it becomes full.
   Then read its contents without modifying the MVar, without possibility
   of intervention from other threads.}
   with
   out_of_line      = True
   has_side_effects = True

primop  TryReadMVarOp "tryReadMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, Int#, a #)
   {If {\tt MVar\#} is empty, immediately return with integer 0 and value undefined.
   Otherwise, return with integer 1 and contents of {\tt MVar\#}.}
   with
   out_of_line      = True
   has_side_effects = True

primop  SameMVarOp "sameMVar#" GenPrimOp
   MVar# s a -> MVar# s a -> Int#

primop  IsEmptyMVarOp "isEmptyMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, Int# #)
   {Return 1 if {\tt MVar\#} is empty; 0 otherwise.}
   with
   out_of_line = True
   has_side_effects = True

------------------------------------------------------------------------
section "Delay/wait operations"
------------------------------------------------------------------------

primop  DelayOp "delay#" GenPrimOp
   Int# -> State# s -> State# s
   {Sleep specified number of microseconds.}
   with
   has_side_effects = True
   out_of_line      = True

primop  WaitReadOp "waitRead#" GenPrimOp
   Int# -> State# s -> State# s
   {Block until input is available on specified file descriptor.}
   with
   has_side_effects = True
   out_of_line      = True

primop  WaitWriteOp "waitWrite#" GenPrimOp
   Int# -> State# s -> State# s
   {Block until output is possible on specified file descriptor.}
   with
   has_side_effects = True
   out_of_line      = True

------------------------------------------------------------------------
section "Concurrency primitives"
------------------------------------------------------------------------

primtype State# s
        { {\tt State\#} is the primitive, unlifted type of states.  It has
        one type parameter, thus {\tt State\# RealWorld}, or {\tt State\# s},
        where s is a type variable. The only purpose of the type parameter
        is to keep different state threads separate.  It is represented by
        nothing at all. }

primtype RealWorld
        { {\tt RealWorld} is deeply magical.  It is {\it primitive}, but it is not
        {\it unlifted} (hence {\tt ptrArg}).  We never manipulate values of type
        {\tt RealWorld}; it's only used in the type system, to parameterise {\tt State\#}. }

primtype ThreadId#
        {(In a non-concurrent implementation, this can be a singleton
        type, whose (unique) value is returned by {\tt myThreadId\#}.  The
        other operations can be omitted.)}

primop  ForkOp "fork#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
   with
   has_side_effects = True
   out_of_line      = True

primop  ForkOnOp "forkOn#" GenPrimOp
   Int# -> a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
   with
   has_side_effects = True
   out_of_line      = True

primop  KillThreadOp "killThread#"  GenPrimOp
   ThreadId# -> a -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line      = True

primop  YieldOp "yield#" GenPrimOp
   State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line      = True

primop  MyThreadIdOp "myThreadId#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, ThreadId# #)
   with
   has_side_effects = True

primop LabelThreadOp "labelThread#" GenPrimOp
   ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line      = True

primop  IsCurrentThreadBoundOp "isCurrentThreadBound#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, Int# #)
   with
   out_of_line = True
   has_side_effects = True

primop  NoDuplicateOp "noDuplicate#" GenPrimOp
   State# s -> State# s
   with
   out_of_line = True
   has_side_effects = True

primop  ThreadStatusOp "threadStatus#" GenPrimOp
   ThreadId# -> State# RealWorld -> (# State# RealWorld, Int#, Int#, Int# #)
   with
   out_of_line = True
   has_side_effects = True

------------------------------------------------------------------------
section "Weak pointers"
------------------------------------------------------------------------

primtype Weak# b

-- note that tyvar "o" denotes openAlphaTyVar

primop  MkWeakOp "mkWeak#" GenPrimOp
   o -> b -> (State# RealWorld -> (# State# RealWorld, c #))
     -> State# RealWorld -> (# State# RealWorld, Weak# b #)
   { {\tt mkWeak# k v finalizer s} creates a weak reference to value {\tt k},
     with an associated reference to some value {\tt v}. If {\tt k} is still
     alive then {\tt v} can be retrieved using {\tt deRefWeak#}. Note that
     the type of {\tt k} must be represented by a pointer (i.e. of kind {\tt
     TYPE 'LiftedRep} or {\tt TYPE 'UnliftedRep}). }
   with
   has_side_effects = True
   out_of_line      = True

primop  MkWeakNoFinalizerOp "mkWeakNoFinalizer#" GenPrimOp
   o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
   with
   has_side_effects = True
   out_of_line      = True

primop  AddCFinalizerToWeakOp "addCFinalizerToWeak#" GenPrimOp
   Addr# -> Addr# -> Int# -> Addr# -> Weak# b
          -> State# RealWorld -> (# State# RealWorld, Int# #)
   { {\tt addCFinalizerToWeak# fptr ptr flag eptr w} attaches a C
     function pointer {\tt fptr} to a weak pointer {\tt w} as a finalizer. If
     {\tt flag} is zero, {\tt fptr} will be called with one argument,
     {\tt ptr}. Otherwise, it will be called with two arguments,
     {\tt eptr} and {\tt ptr}. {\tt addCFinalizerToWeak#} returns
     1 on success, or 0 if {\tt w} is already dead. }
   with
   has_side_effects = True
   out_of_line      = True

primop  DeRefWeakOp "deRefWeak#" GenPrimOp
   Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)
   with
   has_side_effects = True
   out_of_line      = True

primop  FinalizeWeakOp "finalizeWeak#" GenPrimOp
   Weak# a -> State# RealWorld -> (# State# RealWorld, Int#,
              (State# RealWorld -> (# State# RealWorld, b #) ) #)
   { Finalize a weak pointer. The return value is an unboxed tuple
     containing the new state of the world and an "unboxed Maybe",
     represented by an {\tt Int#} and a (possibly invalid) finalization
     action. An {\tt Int#} of {\tt 1} indicates that the finalizer is valid. The
     return value {\tt b} from the finalizer should be ignored. }
   with
   has_side_effects = True
   out_of_line      = True

primop TouchOp "touch#" GenPrimOp
   o -> State# RealWorld -> State# RealWorld
   with
   code_size = { 0 }
   has_side_effects = True

------------------------------------------------------------------------
section "Stable pointers and names"
------------------------------------------------------------------------

primtype StablePtr# a

primtype StableName# a

primop  MakeStablePtrOp "makeStablePtr#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
   with
   has_side_effects = True
   out_of_line      = True

primop  DeRefStablePtrOp "deRefStablePtr#" GenPrimOp
   StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
   with
   has_side_effects = True
   out_of_line      = True

primop  EqStablePtrOp "eqStablePtr#" GenPrimOp
   StablePtr# a -> StablePtr# a -> Int#
   with
   has_side_effects = True

primop  MakeStableNameOp "makeStableName#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, StableName# a #)
   with
   has_side_effects = True
   out_of_line      = True

primop  EqStableNameOp "eqStableName#" GenPrimOp
   StableName# a -> StableName# b -> Int#

primop  StableNameToIntOp "stableNameToInt#" GenPrimOp
   StableName# a -> Int#

------------------------------------------------------------------------
section "Compact normal form"

        {Primitives for working with compact regions. The {\tt ghc\-compact}
         library and the {\tt compact} library demonstrate how to use these
         primitives. The documentation below draws a distinction between
         a CNF and a compact block. A CNF contains one or more compact
         blocks. The source file {\tt rts\/sm\/CNF.c}
         diagrams this relationship. When discussing a compact
         block, an additional distinction is drawn between capacity and
         utilized bytes. The capacity is the maximum number of bytes that
         the compact block can hold. The utilized bytes is the number of
         bytes that are actually used by the compact block.
        }

------------------------------------------------------------------------

primtype Compact#

primop  CompactNewOp "compactNew#" GenPrimOp
   Word# -> State# RealWorld -> (# State# RealWorld, Compact# #)
   { Create a new CNF with a single compact block. The argument is
     the capacity of the compact block (in bytes, not words).
     The capacity is rounded up to a multiple of the allocator block size
     and is capped to one mega block. }
   with
   has_side_effects = True
   out_of_line      = True

primop  CompactResizeOp "compactResize#" GenPrimOp
   Compact# -> Word# -> State# RealWorld ->
   State# RealWorld
   { Set the new allocation size of the CNF. This value (in bytes)
     determines the capacity of each compact block in the CNF. It
     does not retroactively affect existing compact blocks in the CNF. }
   with
   has_side_effects = True
   out_of_line      = True

primop  CompactContainsOp "compactContains#" GenPrimOp
   Compact# -> a -> State# RealWorld -> (# State# RealWorld, Int# #)
   { Returns 1\# if the object is contained in the CNF, 0\# otherwise. }
   with
   out_of_line      = True

primop  CompactContainsAnyOp "compactContainsAny#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, Int# #)
   { Returns 1\# if the object is in any CNF at all, 0\# otherwise. }
   with
   out_of_line      = True

primop  CompactGetFirstBlockOp "compactGetFirstBlock#" GenPrimOp
   Compact# -> State# RealWorld -> (# State# RealWorld, Addr#, Word# #)
   { Returns the address and the utilized size (in bytes) of the
     first compact block of a CNF.}
   with
   out_of_line      = True

primop  CompactGetNextBlockOp "compactGetNextBlock#" GenPrimOp
   Compact# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, Word# #)
   { Given a CNF and the address of one its compact blocks, returns the
     next compact block and its utilized size, or {\tt nullAddr\#} if the
     argument was the last compact block in the CNF. }
   with
   out_of_line      = True

primop  CompactAllocateBlockOp "compactAllocateBlock#" GenPrimOp
   Word# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)
   { Attempt to allocate a compact block with the capacity (in
     bytes) given by the first argument. The {\texttt Addr\#} is a pointer
     to previous compact block of the CNF or {\texttt nullAddr\#} to create a
     new CNF with a single compact block.

     The resulting block is not known to the GC until
     {\texttt compactFixupPointers\#} is called on it, and care must be taken
     so that the address does not escape or memory will be leaked.
   }
   with
   has_side_effects = True
   out_of_line      = True

primop  CompactFixupPointersOp "compactFixupPointers#" GenPrimOp
   Addr# -> Addr# -> State# RealWorld -> (# State# RealWorld, Compact#, Addr# #)
   { Given the pointer to the first block of a CNF and the
     address of the root object in the old address space, fix up
     the internal pointers inside the CNF to account for
     a different position in memory than when it was serialized.
     This method must be called exactly once after importing
     a serialized CNF. It returns the new CNF and the new adjusted
     root address. }
   with
   has_side_effects = True
   out_of_line      = True

primop CompactAdd "compactAdd#" GenPrimOp
   Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)
   { Recursively add a closure and its transitive closure to a
     {\texttt Compact\#} (a CNF), evaluating any unevaluated components
     at the same time. Note: {\texttt compactAdd\#} is not thread-safe, so
     only one thread may call {\texttt compactAdd\#} with a particular
     {\texttt Compact\#} at any given time. The primop does not
     enforce any mutual exclusion; the caller is expected to
     arrange this. }
   with
   has_side_effects = True
   out_of_line      = True

primop CompactAddWithSharing "compactAddWithSharing#" GenPrimOp
   Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)
   { Like {\texttt compactAdd\#}, but retains sharing and cycles
   during compaction. }
   with
   has_side_effects = True
   out_of_line      = True

primop CompactSize "compactSize#" GenPrimOp
   Compact# -> State# RealWorld -> (# State# RealWorld, Word# #)
   { Return the total capacity (in bytes) of all the compact blocks
     in the CNF. }
   with
   has_side_effects = True
   out_of_line      = True

------------------------------------------------------------------------
section "Unsafe pointer equality"
--  (#1 Bad Guy: Alastair Reid :)
------------------------------------------------------------------------

primop  ReallyUnsafePtrEqualityOp "reallyUnsafePtrEquality#" GenPrimOp
   a -> a -> Int#
   { Returns {\texttt 1\#} if the given pointers are equal and {\texttt 0\#} otherwise. }
   with
   can_fail   = True -- See Note [reallyUnsafePtrEquality#]


-- Note [reallyUnsafePtrEquality#]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- reallyUnsafePtrEquality# can't actually fail, per se, but we mark it can_fail
-- anyway. Until 5a9a1738023a, GHC considered primops okay for speculation only
-- when their arguments were known to be forced. This was unnecessarily
-- conservative, but it prevented reallyUnsafePtrEquality# from floating out of
-- places where its arguments were known to be forced. Unfortunately, GHC could
-- sometimes lose track of whether those arguments were forced, leading to let/app
-- invariant failures (see #13027 and the discussion in #11444). Now that
-- ok_for_speculation skips over lifted arguments, we need to explicitly prevent
-- reallyUnsafePtrEquality# from floating out. Imagine if we had
--
--     \x y . case x of x'
--              DEFAULT ->
--            case y of y'
--              DEFAULT ->
--               let eq = reallyUnsafePtrEquality# x' y'
--               in ...
--
-- If the let floats out, we'll get
--
--     \x y . let eq = reallyUnsafePtrEquality# x y
--            in case x of ...
--
-- The trouble is that pointer equality between thunks is very different
-- from pointer equality between the values those thunks reduce to, and the latter
-- is typically much more precise.

------------------------------------------------------------------------
section "Parallelism"
------------------------------------------------------------------------

primop  ParOp "par#" GenPrimOp
   a -> Int#
   with
      -- Note that Par is lazy to avoid that the sparked thing
      -- gets evaluated strictly, which it should *not* be
   has_side_effects = True
   code_size = { primOpCodeSizeForeignCall }
   deprecated_msg = { Use 'spark#' instead }

primop SparkOp "spark#" GenPrimOp
   a -> State# s -> (# State# s, a #)
   with has_side_effects = True
   code_size = { primOpCodeSizeForeignCall }

primop SeqOp "seq#" GenPrimOp
   a -> State# s -> (# State# s, a #)
   -- See Note [seq# magic] in GHC.Core.Op.ConstantFold

primop GetSparkOp "getSpark#" GenPrimOp
   State# s -> (# State# s, Int#, a #)
   with
   has_side_effects = True
   out_of_line = True

primop NumSparks "numSparks#" GenPrimOp
   State# s -> (# State# s, Int# #)
   { Returns the number of sparks in the local spark pool. }
   with
   has_side_effects = True
   out_of_line = True

------------------------------------------------------------------------
section "Tag to enum stuff"
        {Convert back and forth between values of enumerated types
        and small integers.}
------------------------------------------------------------------------

primop  DataToTagOp "dataToTag#" GenPrimOp
   a -> Int#  -- Zero-indexed; the first constructor has tag zero
   with
   strictness = { \ _arity -> mkClosedStrictSig [evalDmd] topDiv }
   -- See Note [dataToTag# magic] in GHC.Core.Op.ConstantFold

primop  TagToEnumOp "tagToEnum#" GenPrimOp
   Int# -> a

------------------------------------------------------------------------
section "Bytecode operations"
        {Support for manipulating bytecode objects used by the interpreter and
        linker.

        Bytecode objects are heap objects which represent top-level bindings and
        contain a list of instructions and data needed by these instructions.}
------------------------------------------------------------------------

primtype BCO
   { Primitive bytecode type. }

primop   AddrToAnyOp "addrToAny#" GenPrimOp
   Addr# -> (# a #)
   { Convert an {\tt Addr\#} to a followable Any type. }
   with
   code_size = 0

primop   AnyToAddrOp "anyToAddr#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, Addr# #)
   { Retrieve the address of any Haskell value. This is
     essentially an {\texttt unsafeCoerce\#}, but if implemented as such
     the core lint pass complains and fails to compile.
     As a primop, it is opaque to core/stg, and only appears
     in cmm (where the copy propagation pass will get rid of it).
     Note that "a" must be a value, not a thunk! It's too late
     for strictness analysis to enforce this, so you're on your
     own to guarantee this. Also note that {\texttt Addr\#} is not a GC
     pointer - up to you to guarantee that it does not become
     a dangling pointer immediately after you get it.}
   with
   code_size = 0

primop   MkApUpd0_Op "mkApUpd0#" GenPrimOp
   BCO -> (# a #)
   { Wrap a BCO in a {\tt AP_UPD} thunk which will be updated with the value of
     the BCO when evaluated. }
   with
   out_of_line = True

primop  NewBCOOp "newBCO#" GenPrimOp
   ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> State# s -> (# State# s, BCO #)
   { {\tt newBCO\# instrs lits ptrs arity bitmap} creates a new bytecode object. The
     resulting object encodes a function of the given arity with the instructions
     encoded in {\tt instrs}, and a static reference table usage bitmap given by
     {\tt bitmap}. }
   with
   has_side_effects = True
   out_of_line      = True

primop  UnpackClosureOp "unpackClosure#" GenPrimOp
   a -> (# Addr#, ByteArray#, Array# b #)
   { {\tt unpackClosure\# closure} copies the closure and pointers in the
     payload of the given closure into two new arrays, and returns a pointer to
     the first word of the closure's info table, a non-pointer array for the raw
     bytes of the closure, and a pointer array for the pointers in the payload. }
   with
   out_of_line = True

primop  ClosureSizeOp "closureSize#" GenPrimOp
   a -> Int#
   { {\tt closureSize\# closure} returns the size of the given closure in
     machine words. }
   with
   out_of_line = True

primop  GetApStackValOp "getApStackVal#" GenPrimOp
   a -> Int# -> (# Int#, b #)
   with
   out_of_line = True

------------------------------------------------------------------------
section "Misc"
        {These aren't nearly as wired in as Etc...}
------------------------------------------------------------------------

primop  GetCCSOfOp "getCCSOf#" GenPrimOp
   a -> State# s -> (# State# s, Addr# #)

primop  GetCurrentCCSOp "getCurrentCCS#" GenPrimOp
   a -> State# s -> (# State# s, Addr# #)
   { Returns the current {\tt CostCentreStack} (value is {\tt NULL} if
     not profiling).  Takes a dummy argument which can be used to
     avoid the call to {\tt getCurrentCCS\#} being floated out by the
     simplifier, which would result in an uninformative stack
     ("CAF"). }

primop  ClearCCSOp "clearCCS#" GenPrimOp
   (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
   { Run the supplied IO action with an empty CCS.  For example, this
     is used by the interpreter to run an interpreted computation
     without the call stack showing that it was invoked from GHC. }
   with
   out_of_line = True

------------------------------------------------------------------------
section "Etc"
        {Miscellaneous built-ins}
------------------------------------------------------------------------

primtype Proxy# a
   { The type constructor {\tt Proxy#} is used to bear witness to some
   type variable. It's used when you want to pass around proxy values
   for doing things like modelling type applications. A {\tt Proxy#}
   is not only unboxed, it also has a polymorphic kind, and has no
   runtime representation, being totally free. }

pseudoop "proxy#"
   Proxy# a
   { Witness for an unboxed {\tt Proxy#} value, which has no runtime
   representation. }

pseudoop "with#"
   o -> (State# RealWorld -> (# State# RealWorld, p #)) -> State# RealWorld -> (# State# RealWorld, p #)
   { TODO. }

pseudoop   "seq"
   a -> b -> b
   { The value of {\tt seq a b} is bottom if {\tt a} is bottom, and
     otherwise equal to {\tt b}. In other words, it evaluates the first
     argument {\tt a} to weak head normal form (WHNF). {\tt seq} is usually
     introduced to improve performance by avoiding unneeded laziness.

     A note on evaluation order: the expression {\tt seq a b} does
     {\it not} guarantee that {\tt a} will be evaluated before {\tt b}.
     The only guarantee given by {\tt seq} is that the both {\tt a}
     and {\tt b} will be evaluated before {\tt seq} returns a value.
     In particular, this means that {\tt b} may be evaluated before
     {\tt a}. If you need to guarantee a specific order of evaluation,
     you must use the function {\tt pseq} from the "parallel" package. }
   with fixity = infixr 0
         -- This fixity is only the one picked up by Haddock. If you
         -- change this, do update 'ghcPrimIface' in 'GHC.Iface.Load'.

pseudoop   "unsafeCoerce#"
   a -> b
   { The function {\tt unsafeCoerce\#} allows you to side-step the typechecker entirely. That
        is, it allows you to coerce any type into any other type. If you use this function,
        you had better get it right, otherwise segmentation faults await. It is generally
        used when you want to write a program that you know is well-typed, but where Haskell's
        type system is not expressive enough to prove that it is well typed.

        The following uses of {\tt unsafeCoerce\#} are supposed to work (i.e. not lead to
        spurious compile-time or run-time crashes):

         * Casting any lifted type to {\tt Any}

         * Casting {\tt Any} back to the real type

         * Casting an unboxed type to another unboxed type of the same size.
           (Casting between floating-point and integral types does not work.
           See the {\tt GHC.Float} module for functions to do work.)

         * Casting between two types that have the same runtime representation.  One case is when
           the two types differ only in "phantom" type parameters, for example
           {\tt Ptr Int} to {\tt Ptr Float}, or {\tt [Int]} to {\tt [Float]} when the list is
           known to be empty.  Also, a {\tt newtype} of a type {\tt T} has the same representation
           at runtime as {\tt T}.

        Other uses of {\tt unsafeCoerce\#} are undefined.  In particular, you should not use
        {\tt unsafeCoerce\#} to cast a T to an algebraic data type D, unless T is also
        an algebraic data type.  For example, do not cast {\tt Int->Int} to {\tt Bool}, even if
        you later cast that {\tt Bool} back to {\tt Int->Int} before applying it.  The reasons
        have to do with GHC's internal representation details (for the cognoscenti, data values
        can be entered but function closures cannot).  If you want a safe type to cast things
        to, use {\tt Any}, which is not an algebraic data type.

        }
   with can_fail = True

-- NB. It is tempting to think that casting a value to a type that it doesn't have is safe
-- as long as you don't "do anything" with the value in its cast form, such as seq on it.  This
-- isn't the case: the compiler can insert seqs itself, and if these happen at the wrong type,
-- Bad Things Might Happen.  See bug #1616: in this case we cast a function of type (a,b) -> (a,b)
-- to () -> () and back again.  The strictness analyser saw that the function was strict, but
-- the wrapper had type () -> (), and hence the wrapper de-constructed the (), the worker re-constructed
-- a new (), with the result that the code ended up with "case () of (a,b) -> ...".

primop  TraceEventOp "traceEvent#" GenPrimOp
   Addr# -> State# s -> State# s
   { Emits an event via the RTS tracing framework.  The contents
     of the event is the zero-terminated byte string passed as the first
     argument.  The event will be emitted either to the {\tt .eventlog} file,
     or to stderr, depending on the runtime RTS flags. }
   with
   has_side_effects = True
   out_of_line      = True

primop  TraceEventBinaryOp "traceBinaryEvent#" GenPrimOp
   Addr# -> Int# -> State# s -> State# s
   { Emits an event via the RTS tracing framework.  The contents
     of the event is the binary object passed as the first argument with
     the the given length passed as the second argument. The event will be
     emitted to the {\tt .eventlog} file. }
   with
   has_side_effects = True
   out_of_line      = True

primop  TraceMarkerOp "traceMarker#" GenPrimOp
   Addr# -> State# s -> State# s
   { Emits a marker event via the RTS tracing framework.  The contents
     of the event is the zero-terminated byte string passed as the first
     argument.  The event will be emitted either to the {\tt .eventlog} file,
     or to stderr, depending on the runtime RTS flags. }
   with
   has_side_effects = True
   out_of_line      = True

primop  SetThreadAllocationCounter "setThreadAllocationCounter#" GenPrimOp
   INT64 -> State# RealWorld -> State# RealWorld
   { Sets the allocation counter for the current thread to the given value. }
   with
   has_side_effects = True
   out_of_line      = True

------------------------------------------------------------------------
section "Safe coercions"
------------------------------------------------------------------------

pseudoop   "coerce"
   Coercible a b => a -> b
   { The function {\tt coerce} allows you to safely convert between values of
     types that have the same representation with no run-time overhead. In the
     simplest case you can use it instead of a newtype constructor, to go from
     the newtype's concrete type to the abstract type. But it also works in
     more complicated settings, e.g. converting a list of newtypes to a list of
     concrete types.

     This function is runtime-representation polymorphic, but the
     {\tt RuntimeRep} type argument is marked as {\tt Inferred}, meaning
     that it is not available for visible type application. This means
     the typechecker will accept {\tt coerce @Int @Age 42}.
   }

------------------------------------------------------------------------
section "SIMD Vectors"
        {Operations on SIMD vectors.}
------------------------------------------------------------------------

#define ALL_VECTOR_TYPES \
  [<Int8,Int#,16>,<Int16,Int#,8>,<Int32,INT32,4>,<Int64,INT64,2> \
  ,<Int8,Int#,32>,<Int16,Int#,16>,<Int32,INT32,8>,<Int64,INT64,4> \
  ,<Int8,Int#,64>,<Int16,Int#,32>,<Int32,INT32,16>,<Int64,INT64,8> \
  ,<Word8,Word#,16>,<Word16,Word#,8>,<Word32,WORD32,4>,<Word64,WORD64,2> \
  ,<Word8,Word#,32>,<Word16,Word#,16>,<Word32,WORD32,8>,<Word64,WORD64,4> \
  ,<Word8,Word#,64>,<Word16,Word#,32>,<Word32,WORD32,16>,<Word64,WORD64,8> \
  ,<Float,Float#,4>,<Double,Double#,2> \
  ,<Float,Float#,8>,<Double,Double#,4> \
  ,<Float,Float#,16>,<Double,Double#,8>]

#define SIGNED_VECTOR_TYPES \
  [<Int8,Int#,16>,<Int16,Int#,8>,<Int32,INT32,4>,<Int64,INT64,2> \
  ,<Int8,Int#,32>,<Int16,Int#,16>,<Int32,INT32,8>,<Int64,INT64,4> \
  ,<Int8,Int#,64>,<Int16,Int#,32>,<Int32,INT32,16>,<Int64,INT64,8> \
  ,<Float,Float#,4>,<Double,Double#,2> \
  ,<Float,Float#,8>,<Double,Double#,4> \
  ,<Float,Float#,16>,<Double,Double#,8>]

#define FLOAT_VECTOR_TYPES \
  [<Float,Float#,4>,<Double,Double#,2> \
  ,<Float,Float#,8>,<Double,Double#,4> \
  ,<Float,Float#,16>,<Double,Double#,8>]

#define INT_VECTOR_TYPES \
  [<Int8,Int#,16>,<Int16,Int#,8>,<Int32,INT32,4>,<Int64,INT64,2> \
  ,<Int8,Int#,32>,<Int16,Int#,16>,<Int32,INT32,8>,<Int64,INT64,4> \
  ,<Int8,Int#,64>,<Int16,Int#,32>,<Int32,INT32,16>,<Int64,INT64,8> \
  ,<Word8,Word#,16>,<Word16,Word#,8>,<Word32,WORD32,4>,<Word64,WORD64,2> \
  ,<Word8,Word#,32>,<Word16,Word#,16>,<Word32,WORD32,8>,<Word64,WORD64,4> \
  ,<Word8,Word#,64>,<Word16,Word#,32>,<Word32,WORD32,16>,<Word64,WORD64,8>]

primtype VECTOR
   with llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecBroadcastOp "broadcast#" GenPrimOp
   SCALAR -> VECTOR
   { Broadcast a scalar to all elements of a vector. }
   with llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecPackOp "pack#" GenPrimOp
   VECTUPLE -> VECTOR
   { Pack the elements of an unboxed tuple into a vector. }
   with llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecUnpackOp "unpack#" GenPrimOp
   VECTOR -> VECTUPLE
   { Unpack the elements of a vector into an unboxed tuple. #}
   with llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecInsertOp "insert#" GenPrimOp
   VECTOR -> SCALAR -> Int# -> VECTOR
   { Insert a scalar at the given position in a vector. }
   with can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecAddOp "plus#" Dyadic
   VECTOR -> VECTOR -> VECTOR
   { Add two vectors element-wise. }
   with commutable = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecSubOp "minus#" Dyadic
   VECTOR -> VECTOR -> VECTOR
   { Subtract two vectors element-wise. }
   with llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecMulOp "times#" Dyadic
   VECTOR -> VECTOR -> VECTOR
   { Multiply two vectors element-wise. }
   with commutable = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecDivOp "divide#" Dyadic
   VECTOR -> VECTOR -> VECTOR
   { Divide two vectors element-wise. }
   with can_fail = True
        llvm_only = True
        vector = FLOAT_VECTOR_TYPES

primop VecQuotOp "quot#" Dyadic
   VECTOR -> VECTOR -> VECTOR
   { Rounds towards zero element-wise. }
   with can_fail = True
        llvm_only = True
        vector = INT_VECTOR_TYPES

primop VecRemOp "rem#" Dyadic
   VECTOR -> VECTOR -> VECTOR
   { Satisfies \texttt{(quot\# x y) times\# y plus\# (rem\# x y) == x}. }
   with can_fail = True
        llvm_only = True
        vector = INT_VECTOR_TYPES

primop VecNegOp "negate#" Monadic
   VECTOR -> VECTOR
   { Negate element-wise. }
   with llvm_only = True
        vector = SIGNED_VECTOR_TYPES

primop VecIndexByteArrayOp "indexArray#" GenPrimOp
   ByteArray# -> Int# -> VECTOR
   { Read a vector from specified index of immutable array. }
   with can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecReadByteArrayOp "readArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, VECTOR #)
   { Read a vector from specified index of mutable array. }
   with has_side_effects = True
        can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecWriteByteArrayOp "writeArray#" GenPrimOp
   MutableByteArray# s -> Int# -> VECTOR -> State# s -> State# s
   { Write a vector to specified index of mutable array. }
   with has_side_effects = True
        can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecIndexOffAddrOp "indexOffAddr#" GenPrimOp
   Addr# -> Int# -> VECTOR
   { Reads vector; offset in bytes. }
   with can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecReadOffAddrOp "readOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, VECTOR #)
   { Reads vector; offset in bytes. }
   with has_side_effects = True
        can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecWriteOffAddrOp "writeOffAddr#" GenPrimOp
   Addr# -> Int# -> VECTOR -> State# s -> State# s
   { Write vector; offset in bytes. }
   with has_side_effects = True
        can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES


primop VecIndexScalarByteArrayOp "indexArrayAs#" GenPrimOp
   ByteArray# -> Int# -> VECTOR
   { Read a vector from specified index of immutable array of scalars; offset is in scalar elements. }
   with can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecReadScalarByteArrayOp "readArrayAs#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, VECTOR #)
   { Read a vector from specified index of mutable array of scalars; offset is in scalar elements. }
   with has_side_effects = True
        can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecWriteScalarByteArrayOp "writeArrayAs#" GenPrimOp
   MutableByteArray# s -> Int# -> VECTOR -> State# s -> State# s
   { Write a vector to specified index of mutable array of scalars; offset is in scalar elements. }
   with has_side_effects = True
        can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecIndexScalarOffAddrOp "indexOffAddrAs#" GenPrimOp
   Addr# -> Int# -> VECTOR
   { Reads vector; offset in scalar elements. }
   with can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecReadScalarOffAddrOp "readOffAddrAs#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, VECTOR #)
   { Reads vector; offset in scalar elements. }
   with has_side_effects = True
        can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecWriteScalarOffAddrOp "writeOffAddrAs#" GenPrimOp
   Addr# -> Int# -> VECTOR -> State# s -> State# s
   { Write vector; offset in scalar elements. }
   with has_side_effects = True
        can_fail = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

------------------------------------------------------------------------

section "Prefetch"
        {Prefetch operations: Note how every prefetch operation has a name
  with the pattern prefetch*N#, where N is either 0,1,2, or 3.

  This suffix number, N, is the "locality level" of the prefetch, following the
  convention in GCC and other compilers.
  Higher locality numbers correspond to the memory being loaded in more
  levels of the cpu cache, and being retained after initial use. The naming
  convention follows the naming convention of the prefetch intrinsic found
  in the GCC and Clang C compilers.

  On the LLVM backend, prefetch*N# uses the LLVM prefetch intrinsic
  with locality level N. The code generated by LLVM is target architecture
  dependent, but should agree with the GHC NCG on x86 systems.

  On the Sparc and PPC native backends, prefetch*N is a No-Op.

  On the x86 NCG, N=0 will generate prefetchNTA,
  N=1 generates prefetcht2, N=2 generates prefetcht1, and
  N=3 generates prefetcht0.

  For streaming workloads, the prefetch*0 operations are recommended.
  For workloads which do many reads or writes to a memory location in a short period of time,
  prefetch*3 operations are recommended.

  For further reading about prefetch and associated systems performance optimization,
  the instruction set and optimization manuals by Intel and other CPU vendors are
  excellent starting place.


  The "Intel 64 and IA-32 Architectures Optimization Reference Manual" is
  especially a helpful read, even if your software is meant for other CPU
  architectures or vendor hardware. The manual can be found at
  http://www.intel.com/content/www/us/en/architecture-and-technology/64-ia-32-architectures-optimization-manual.html .

  The {\tt prefetch*} family of operations has the order of operations
  determined by passing around the {\tt State#} token.

  To get a "pure" version of these operations, use {\tt inlinePerformIO} which is quite safe in this context.

  It is important to note that while the prefetch operations will never change the
  answer to a pure computation, They CAN change the memory locations resident
  in a CPU cache and that may change the performance and timing characteristics
  of an application. The prefetch operations are marked has_side_effects=True
  to reflect that these operations have side effects with respect to the runtime
  performance characteristics of the resulting code. Additionally, if the prefetchValue
  operations did not have this attribute, GHC does a float out transformation that
  results in a let/app violation, at least with the current design.
  }



------------------------------------------------------------------------


--- the Int# argument for prefetch is the byte offset on the byteArray or  Addr#

---
primop PrefetchByteArrayOp3 "prefetchByteArray3#" GenPrimOp
  ByteArray# -> Int# ->  State# s -> State# s
  with has_side_effects =  True

primop PrefetchMutableByteArrayOp3 "prefetchMutableByteArray3#" GenPrimOp
  MutableByteArray# s -> Int# -> State# s -> State# s
  with has_side_effects =  True

primop PrefetchAddrOp3 "prefetchAddr3#" GenPrimOp
  Addr# -> Int# -> State# s -> State# s
  with has_side_effects =  True

primop PrefetchValueOp3 "prefetchValue3#" GenPrimOp
   a -> State# s -> State# s
   with strictness  = { \ _arity -> mkClosedStrictSig [botDmd, topDmd] topDiv }
        has_side_effects =  True
----

primop PrefetchByteArrayOp2 "prefetchByteArray2#" GenPrimOp
  ByteArray# -> Int# ->  State# s -> State# s
  with has_side_effects =  True

primop PrefetchMutableByteArrayOp2 "prefetchMutableByteArray2#" GenPrimOp
  MutableByteArray# s -> Int# -> State# s -> State# s
  with has_side_effects =  True

primop PrefetchAddrOp2 "prefetchAddr2#" GenPrimOp
  Addr# -> Int# ->  State# s -> State# s
  with has_side_effects =  True

primop PrefetchValueOp2 "prefetchValue2#" GenPrimOp
   a ->  State# s -> State# s
   with strictness  = { \ _arity -> mkClosedStrictSig [botDmd, topDmd] topDiv }
        has_side_effects =  True
----

primop PrefetchByteArrayOp1 "prefetchByteArray1#" GenPrimOp
   ByteArray# -> Int# -> State# s -> State# s
   with has_side_effects =  True

primop PrefetchMutableByteArrayOp1 "prefetchMutableByteArray1#" GenPrimOp
  MutableByteArray# s -> Int# -> State# s -> State# s
  with has_side_effects =  True

primop PrefetchAddrOp1 "prefetchAddr1#" GenPrimOp
  Addr# -> Int# -> State# s -> State# s
  with has_side_effects =  True

primop PrefetchValueOp1 "prefetchValue1#" GenPrimOp
   a -> State# s -> State# s
   with strictness  = { \ _arity -> mkClosedStrictSig [botDmd, topDmd] topDiv }
        has_side_effects =  True
----

primop PrefetchByteArrayOp0 "prefetchByteArray0#" GenPrimOp
  ByteArray# -> Int# ->  State# s -> State# s
  with has_side_effects =  True

primop PrefetchMutableByteArrayOp0 "prefetchMutableByteArray0#" GenPrimOp
  MutableByteArray# s -> Int# -> State# s -> State# s
  with has_side_effects =  True

primop PrefetchAddrOp0 "prefetchAddr0#" GenPrimOp
  Addr# -> Int# -> State# s -> State# s
  with has_side_effects =  True

primop PrefetchValueOp0 "prefetchValue0#" GenPrimOp
   a -> State# s -> State# s
   with strictness  = { \ _arity -> mkClosedStrictSig [botDmd, topDmd] topDiv }
        has_side_effects =  True

------------------------------------------------------------------------
---                                                                  ---
------------------------------------------------------------------------

thats_all_folks
