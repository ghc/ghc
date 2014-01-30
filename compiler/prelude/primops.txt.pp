-----------------------------------------------------------------------
--
-- (c) 2010 The University of Glasgow
--
-- Primitive Operations and Types
--
-- For more information on PrimOps, see
--   http://ghc.haskell.org/trac/ghc/wiki/Commentary/PrimOps
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
--  http://ghc.haskell.org/trac/ghc/wiki/Commentary/PrimOps

-- This file is divided into named sections, each containing or more
-- primop entries. Section headers have the format:
--
--	section "section-name" {description}
--
-- This information is used solely when producing documentation; it is
-- otherwise ignored.  The description is optional.
--
-- The format of each primop entry is as follows:
--
--	primop internal-name "name-in-program-text" type category {description} attributes

-- The default attribute values which apply if you don't specify
-- other ones.  Attribute values can be True, False, or arbitrary
-- text between curly brackets.  This is a kludge to enable
-- processors of this file to easily get hold of simple info
-- (eg, out_of_line), whilst avoiding parsing complex expressions
-- needed for strictness info.

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
   out_of_line      = False   -- See Note Note [PrimOp can_fail and has_side_effects] in PrimOp
   can_fail         = False   -- See Note Note [PrimOp can_fail and has_side_effects] in PrimOp
   commutable       = False
   code_size        = { primOpCodeSizeDefault }
   strictness       = { \ arity -> mkClosedStrictSig (replicate arity topDmd) topRes }
   fixity           = Nothing
   llvm_only        = False
   vector           = []

-- Currently, documentation is produced using latex, so contents of
-- description fields should be legal latex. Descriptions can contain
-- matched pairs of embedded curly brackets.

#include "MachDeps.h"

-- We need platform defines (tests for mingw32 below).
#include "ghc_boot_platform.h"

section "The word size story."
	{Haskell98 specifies that signed integers (type {\tt Int})
	 must contain at least 30 bits. GHC always implements {\tt
	 Int} using the primitive type {\tt Int\#}, whose size equals
	 the {\tt MachDeps.h} constant {\tt WORD\_SIZE\_IN\_BITS}.
	 This is normally set based on the {\tt config.h} parameter
	 {\tt SIZEOF\_HSWORD}, i.e., 32 bits on 32-bit machines, 64
	 bits on 64-bit machines.  However, it can also be explicitly
	 set to a smaller number, e.g., 31 bits, to allow the
	 possibility of using tag bits. Currently GHC itself has only
	 32-bit and 64-bit variants, but 30 or 31-bit code can be
	 exported as an external core file for use in other back ends.

	 GHC also implements a primitive unsigned integer type {\tt
	 Word\#} which always has the same number of bits as {\tt
	 Int\#}.

	 In addition, GHC supports families of explicit-sized integers
	 and words at 8, 16, 32, and 64 bits, with the usual
	 arithmetic operations, comparisons, and a range of
	 conversions.  The 8-bit and 16-bit sizes are always
	 represented as {\tt Int\#} and {\tt Word\#}, and the
	 operations implemented in terms of the the primops on these
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

#if WORD_SIZE_IN_BITS < 32
#define INT32 Int32#
#define WORD32 Word32#
#else
#define INT32 Int#
#define WORD32 Word#
#endif

#if WORD_SIZE_IN_BITS < 64
#define INT64 Int64#
#define WORD64 Word64#
#else
#define INT64 Int#
#define WORD64 Word#
#endif

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
section "Int#"
	{Operations on native-size integers (30+ bits).}
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

primop   IntMulMayOfloOp  "mulIntMayOflo#"
   Dyadic   Int# -> Int# -> Int#
   {Return non-zero if there is any possibility that the upper word of a
    signed integer multiply might contain useful information.  Return
    zero only if you are completely sure that no overflow can occur.
    On a 32-bit platform, the recommmended implementation is to do a
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
   {Rounds towards zero.}
   with can_fail = True

primop   IntRemOp    "remInt#"    Dyadic
   Int# -> Int# -> Int#
   {Satisfies \texttt{(quotInt\# x y) *\# y +\# (remInt\# x y) == x}.}
   with can_fail = True

primop   IntQuotRemOp "quotRemInt#"    GenPrimOp
   Int# -> Int# -> (# Int#, Int# #)
   {Rounds towards zero.}
   with can_fail = True

primop   AndIOp   "andI#"   Dyadic    Int# -> Int# -> Int#
   with commutable = True

primop   OrIOp   "orI#"     Dyadic    Int# -> Int# -> Int#
   with commutable = True

primop   XorIOp   "xorI#"   Dyadic    Int# -> Int# -> Int#
   with commutable = True

primop   NotIOp   "notI#"   Monadic   Int# -> Int#

primop   IntNegOp    "negateInt#"    Monadic   Int# -> Int#
primop   IntAddCOp   "addIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
	 {Add with carry.  First member of result is (wrapped) sum;
          second member is 0 iff no overflow occured.}
   with code_size = 2

primop   IntSubCOp   "subIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
	 {Subtract with carry.  First member of result is (wrapped) difference;
          second member is 0 iff no overflow occured.}
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
	{Operations on native-sized unsigned words (30+ bits).}
------------------------------------------------------------------------

primtype Word#

primop   WordAddOp   "plusWord#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

-- Returns (# high, low #) (or equivalently, (# carry, low #))
primop   WordAdd2Op  "plusWord2#"  GenPrimOp
   Word# -> Word# -> (# Word#, Word# #)
   with commutable = True

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

-- Takes high word of dividend, then low word of dividend, then divisor.
-- Requires that high word is not divisible by divisor.
primop   WordQuotRem2Op "quotRemWord2#" GenPrimOp
   Word# -> Word# -> Word# -> (# Word#, Word# #)
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

primop   BSwap16Op   "byteSwap16#"   Monadic   Word# -> Word#
    {Swap bytes in the lower 16 bits of a word. The higher bytes are undefined. }
primop   BSwap32Op   "byteSwap32#"   Monadic   Word# -> Word#
    {Swap bytes in the lower 32 bits of a word. The higher bytes are undefined. }
primop   BSwap64Op   "byteSwap64#"   Monadic   WORD64 -> WORD64
    {Swap bytes in a 64 bits of a word.}
primop   BSwapOp     "byteSwap#"     Monadic   Word# -> Word#
    {Swap bytes in a word.}

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


#if WORD_SIZE_IN_BITS < 32
------------------------------------------------------------------------
section "Int32#"
	{Operations on 32-bit integers ({\tt Int32\#}).  This type is only used
         if plain {\tt Int\#} has less than 32 bits.  In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primtype Int32#

------------------------------------------------------------------------
section "Word32#"
	{Operations on 32-bit unsigned words. This type is only used
	 if plain {\tt Word\#} has less than 32 bits. In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primtype Word32#

#endif


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

primop   Double2IntOp   "double2Int#"          GenPrimOp  Double# -> Int#
   {Truncates a {\tt Double#} value to the nearest {\tt Int#}.
    Results are undefined if the truncation if truncation yields
    a value outside the range of {\tt Int#}.}

primop   Double2FloatOp   "double2Float#" GenPrimOp Double# -> Float#

primop   DoubleExpOp   "expDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleLogOp   "logDouble#"      Monadic
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

primop   Float2IntOp   "float2Int#"      GenPrimOp  Float# -> Int#
   {Truncates a {\tt Float#} value to the nearest {\tt Int#}.
    Results are undefined if the truncation if truncation yields
    a value outside the range of {\tt Int#}.}

primop   FloatExpOp   "expFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatLogOp   "logFloat#"      Monadic
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
   {Read from specified index of immutable array. Result is packaged into
    an unboxed singleton; the result itself is not yet evaluated.}
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
  {Copy a range of the Array# to the specified region in the MutableArray#.
   Both arrays must fully contain the specified ranges, but this is not checked.
   The two arrays must not be the same array in different states, but this is not checked either.}
  with
  has_side_effects = True
  can_fail         = True
  code_size = { primOpCodeSizeForeignCall + 4 }

primop  CopyMutableArrayOp "copyMutableArray#" GenPrimOp
  MutableArray# s a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the first MutableArray# to the specified region in the second MutableArray#.
   Both arrays must fully contain the specified ranges, but this is not checked.}
  with
  has_side_effects = True
  can_fail         = True
  code_size = { primOpCodeSizeForeignCall + 4 }

primop  CloneArrayOp "cloneArray#" GenPrimOp
  Array# a -> Int# -> Int# -> Array# a
  {Return a newly allocated Array# with the specified subrange of the provided Array#.
   The provided Array# should contain the full subrange specified by the two Int#s, but this is not checked.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }

primop  CloneMutableArrayOp "cloneMutableArray#" GenPrimOp
  MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  {Return a newly allocated Array# with the specified subrange of the provided Array#.
   The provided MutableArray# should contain the full subrange specified by the two Int#s, but this is not checked.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }

primop  FreezeArrayOp "freezeArray#" GenPrimOp
  MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, Array# a #)
  {Return a newly allocated Array# with the specified subrange of the provided MutableArray#.
   The provided MutableArray# should contain the full subrange specified by the two Int#s, but this is not checked.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }

primop  ThawArrayOp "thawArray#" GenPrimOp
  Array# a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  {Return a newly allocated Array# with the specified subrange of the provided MutableArray#.
   The provided Array# should contain the full subrange specified by the two Int#s, but this is not checked.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }

primop CasArrayOp  "casArray#" GenPrimOp
   MutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
   {Unsafe, machine-level atomic compare and swap on an element within an Array.}
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

primop  ByteArrayContents_Char "byteArrayContents#" GenPrimOp
   ByteArray# -> Addr#
   {Intended for use with pinned arrays; otherwise very unsafe!}

primop  SameMutableByteArrayOp "sameMutableByteArray#" GenPrimOp
   MutableByteArray# s -> MutableByteArray# s -> Int#

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
   {Return the size of the array in bytes.}

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
   with can_fail = True

primop IndexByteArrayOp_Int16 "indexInt16Array#" GenPrimOp
   ByteArray# -> Int# -> Int#
   with can_fail = True

primop IndexByteArrayOp_Int32 "indexInt32Array#" GenPrimOp
   ByteArray# -> Int# -> INT32
   with can_fail = True

primop IndexByteArrayOp_Int64 "indexInt64Array#" GenPrimOp
   ByteArray# -> Int# -> INT64
   with can_fail = True

primop IndexByteArrayOp_Word8 "indexWord8Array#" GenPrimOp
   ByteArray# -> Int# -> Word#
   with can_fail = True

primop IndexByteArrayOp_Word16 "indexWord16Array#" GenPrimOp
   ByteArray# -> Int# -> Word#
   with can_fail = True

primop IndexByteArrayOp_Word32 "indexWord32Array#" GenPrimOp
   ByteArray# -> Int# -> WORD32
   with can_fail = True

primop IndexByteArrayOp_Word64 "indexWord64Array#" GenPrimOp
   ByteArray# -> Int# -> WORD64
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
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word "readWordArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
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

primop  CopyByteArrayOp "copyByteArray#" GenPrimOp
  ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the ByteArray# to the specified region in the MutableByteArray#.
   Both arrays must fully contain the specified ranges, but this is not checked.
   The two arrays must not be the same array in different states, but this is not checked either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyMutableByteArrayOp "copyMutableByteArray#" GenPrimOp
  MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the first MutableByteArray# to the specified region in the second MutableByteArray#.
   Both arrays must fully contain the specified ranges, but this is not checked.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }
  can_fail = True

primop  CopyByteArrayToAddrOp "copyByteArrayToAddr#" GenPrimOp
  ByteArray# -> Int# -> Addr# -> Int# -> State# s -> State# s
  {Copy a range of the ByteArray# to the memory range starting at the Addr#.
   The ByteArray# and the memory region at Addr# must fully contain the
   specified ranges, but this is not checked. The Addr# must not point into the
   ByteArray# (e.g. if the ByteArray# were pinned), but this is not checked
   either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyMutableByteArrayToAddrOp "copyMutableByteArrayToAddr#" GenPrimOp
  MutableByteArray# s -> Int# -> Addr# -> Int# -> State# s -> State# s
  {Copy a range of the MutableByteArray# to the memory range starting at the
   Addr#. The MutableByteArray# and the memory region at Addr# must fully
   contain the specified ranges, but this is not checked. The Addr# must not
   point into the MutableByteArray# (e.g. if the MutableByteArray# were
   pinned), but this is not checked either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyAddrToByteArrayOp "copyAddrToByteArray#" GenPrimOp
  Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a memory range starting at the Addr# to the specified range in the
   MutableByteArray#. The memory region at Addr# and the ByteArray# must fully
   contain the specified ranges, but this is not checked. The Addr# must not
   point into the MutableByteArray# (e.g. if the MutableByteArray# were pinned),
   but this is not checked either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  SetByteArrayOp "setByteArray#" GenPrimOp
  MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
  {Set the range of the MutableByteArray# to the specified character.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }
  can_fail = True

primop CasByteArrayOp_Int "casIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Machine-level atomic compare and swap on a word within a ByteArray.}
   with
   out_of_line = True
   has_side_effects = True

primop FetchAddByteArrayOp_Int "fetchAddIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Machine-level word-sized fetch-and-add within a ByteArray.}
   with
   out_of_line = True
   has_side_effects = True


------------------------------------------------------------------------
section "Arrays of arrays"
	{Operations on {\tt ArrayArray\#}. An {\tt ArrayArray\#} contains references to {\em unpointed}
	 arrays, such as {\tt ByteArray\#s}. Hence, it is not parameterised by the element types,
	 just like a {\tt ByteArray\#}, but it needs to be scanned during GC, just like an {\tt Array#}.
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
  {Copy a range of the ArrayArray# to the specified region in the MutableArrayArray#.
   Both arrays must fully contain the specified ranges, but this is not checked.
   The two arrays must not be the same array in different states, but this is not checked either.}
  with
  has_side_effects = True
  can_fail = True
  code_size = { primOpCodeSizeForeignCall }

primop  CopyMutableArrayArrayOp "copyMutableArrayArray#" GenPrimOp
  MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the first MutableArrayArray# to the specified region in the second
   MutableArrayArray#.
   Both arrays must fully contain the specified ranges, but this is not checked.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall }
  can_fail = True

------------------------------------------------------------------------
section "Addr#"
------------------------------------------------------------------------

primtype Addr#
	{ An arbitrary machine address assumed to point outside
	 the garbage-collected heap. }

pseudoop "nullAddr#" Addr#
	{ The null address. }

primop	 AddrAddOp "plusAddr#" GenPrimOp Addr# -> Int# -> Addr#
primop	 AddrSubOp "minusAddr#" GenPrimOp Addr# -> Addr# -> Int#
	 {Result is meaningless if two {\tt Addr\#}s are so far apart that their
	 difference doesn't fit in an {\tt Int\#}.}
primop	 AddrRemOp "remAddr#" GenPrimOp Addr# -> Int# -> Int#
	 {Return the remainder when the {\tt Addr\#} arg, treated like an {\tt Int\#},
	  is divided by the {\tt Int\#} arg.}
#if (WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64)
primop   Addr2IntOp  "addr2Int#"     GenPrimOp   Addr# -> Int#
	{Coerce directly from address to int. Strongly deprecated.}
   with code_size = 0
primop   Int2AddrOp   "int2Addr#"    GenPrimOp  Int# -> Addr#
	{Coerce directly from int to address. Strongly deprecated.}
   with code_size = 0
#endif

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

primop  ReadMutVarOp "readMutVar#" GenPrimOp
   MutVar# s a -> State# s -> (# State# s, a #)
   {Read contents of {\tt MutVar\#}. Result is not yet evaluated.}
   with
   has_side_effects = True
   can_fail         = True

primop  WriteMutVarOp "writeMutVar#"  GenPrimOp
   MutVar# s a -> a -> State# s -> State# s
   {Write contents of {\tt MutVar\#}.}
   with
   has_side_effects = True
   code_size = { primOpCodeSizeForeignCall } -- for the write barrier
   can_fail         = True

primop  SameMutVarOp "sameMutVar#" GenPrimOp
   MutVar# s a -> MutVar# s a -> Int#

-- not really the right type, but we don't know about pairs here.  The
-- correct type is
--
--   MutVar# s a -> (a -> (a,b)) -> State# s -> (# State# s, b #)
--
primop  AtomicModifyMutVarOp "atomicModifyMutVar#" GenPrimOp
   MutVar# s a -> (a -> b) -> State# s -> (# State# s, c #)
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

primop  CatchOp "catch#" GenPrimOp
          (State# RealWorld -> (# State# RealWorld, a #) )
       -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
       -> State# RealWorld
       -> (# State# RealWorld, a #)
   with
	-- Catch is actually strict in its first argument
	-- but we don't want to tell the strictness
	-- analyser about that, so that exceptions stay inside it.
   strictness  = { \ _arity -> mkClosedStrictSig [apply1Dmd,apply2Dmd,topDmd] topRes }
   out_of_line = True
   has_side_effects = True

primop  RaiseOp "raise#" GenPrimOp
   a -> b
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botRes }
      -- NB: result is bottom
   out_of_line = True

-- raiseIO# needs to be a primop, because exceptions in the IO monad
-- must be *precise* - we don't want the strictness analyser turning
-- one kind of bottom into another, as it is allowed to do in pure code.
--
-- But we *do* want to know that it returns bottom after
-- being applied to two arguments, so that this function is strict in y
--     f x y | x>0  	 = raiseIO blah
--           | y>0  	 = return 1
--           | otherwise = return 2

primop  RaiseIOOp "raiseIO#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, b #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd, topDmd] botRes }
   out_of_line = True
   has_side_effects = True

primop  MaskAsyncExceptionsOp "maskAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [apply1Dmd,topDmd] topRes }
   out_of_line = True
   has_side_effects = True

primop  MaskUninterruptibleOp "maskUninterruptible#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [apply1Dmd,topDmd] topRes }
   out_of_line = True
   has_side_effects = True

primop  UnmaskAsyncExceptionsOp "unmaskAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [apply1Dmd,topDmd] topRes }
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

primop	AtomicallyOp "atomically#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> State# RealWorld -> (# State# RealWorld, a #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [apply1Dmd,topDmd] topRes }
   out_of_line = True
   has_side_effects = True

-- NB: retry#'s strictness information specifies it to return bottom.
-- This lets the compiler perform some extra simplifications, since retry#
-- will technically never return.
--
-- This allows the simplifier to replace things like:
--   case retry# s1
--     (# s2, a #) -> e
-- with:
--   retry# s1
-- where 'e' would be unreachable anyway.  See Trac #8091.
primop  RetryOp "retry#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, a #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botRes }
   out_of_line = True
   has_side_effects = True

primop  CatchRetryOp "catchRetry#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   with
   strictness  = { \ _arity -> mkClosedStrictSig [apply1Dmd,apply1Dmd,topDmd] topRes }
   out_of_line = True
   has_side_effects = True

primop  CatchSTMOp "catchSTM#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   with
   strictness  = { \ _arity -> mkClosedStrictSig [apply1Dmd,apply2Dmd,topDmd] topRes }
   out_of_line = True
   has_side_effects = True

primop  Check "check#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, () #) )
   with
   out_of_line = True
   has_side_effects = True

primop	NewTVarOp "newTVar#" GenPrimOp
       a
    -> State# s -> (# State# s, TVar# s a #)
   {Create a new {\tt TVar\#} holding a specified initial value.}
   with
   out_of_line  = True
   has_side_effects = True

primop	ReadTVarOp "readTVar#" GenPrimOp
       TVar# s a
    -> State# s -> (# State# s, a #)
   {Read contents of {\tt TVar\#}.  Result is not yet evaluated.}
   with
   out_of_line	= True
   has_side_effects = True

primop ReadTVarIOOp "readTVarIO#" GenPrimOp
       TVar# s a
    -> State# s -> (# State# s, a #)
   {Read contents of {\tt TVar\#} outside an STM transaction}
   with
   out_of_line	= True
   has_side_effects = True

primop	WriteTVarOp "writeTVar#" GenPrimOp
       TVar# s a
    -> a
    -> State# s -> State# s
   {Write contents of {\tt TVar\#}.}
   with
   out_of_line	    = True
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

#ifdef mingw32_TARGET_OS
primop  AsyncReadOp "asyncRead#" GenPrimOp
   Int# -> Int# -> Int# -> Addr# -> State# RealWorld-> (# State# RealWorld, Int#, Int# #)
   {Asynchronously read bytes from specified file descriptor.}
   with
   has_side_effects = True
   out_of_line      = True

primop  AsyncWriteOp "asyncWrite#" GenPrimOp
   Int# -> Int# -> Int# -> Addr# -> State# RealWorld-> (# State# RealWorld, Int#, Int# #)
   {Asynchronously write bytes from specified file descriptor.}
   with
   has_side_effects = True
   out_of_line      = True

primop  AsyncDoProcOp "asyncDoProc#" GenPrimOp
   Addr# -> Addr# -> State# RealWorld-> (# State# RealWorld, Int#, Int# #)
   {Asynchronously perform procedure (first arg), passing it 2nd arg.}
   with
   has_side_effects = True
   out_of_line      = True

#endif

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
   out_of_line = True
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
   State# RealWorld -> State# RealWorld
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
   o -> b -> c -> State# RealWorld -> (# State# RealWorld, Weak# b #)
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
              (State# RealWorld -> (# State# RealWorld, () #)) #)
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
section "Unsafe pointer equality"
--  (#1 Bad Guy: Alistair Reid :)
------------------------------------------------------------------------

primop  ReallyUnsafePtrEqualityOp "reallyUnsafePtrEquality#" GenPrimOp
   a -> a -> Int#

------------------------------------------------------------------------
section "Parallelism"
------------------------------------------------------------------------

primop  ParOp "par#" GenPrimOp
   a -> Int#
   with
      -- Note that Par is lazy to avoid that the sparked thing
      -- gets evaluted strictly, which it should *not* be
   has_side_effects = True
   code_size = { primOpCodeSizeForeignCall }

primop SparkOp "spark#" GenPrimOp
   a -> State# s -> (# State# s, a #)
   with has_side_effects = True
   code_size = { primOpCodeSizeForeignCall }

primop SeqOp "seq#" GenPrimOp
   a -> State# s -> (# State# s, a #)

   -- why return the value?  So that we can control sharing of seq'd
   -- values: in
   --    let x = e in x `seq` ... x ...
   -- we don't want to inline x, so better to represent it as
   --    let x = e in case seq# x RW of (# _, x' #) -> ... x' ...
   -- also it matches the type of rseq in the Eval monad.

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

-- HWL: The first 4 Int# in all par... annotations denote:
--   name, granularity info, size of result, degree of parallelism
--      Same  structure as _seq_ i.e. returns Int#
-- KSW: v, the second arg in parAt# and parAtForNow#, is used only to determine
--   `the processor containing the expression v'; it is not evaluated

primop  ParGlobalOp  "parGlobal#"  GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   has_side_effects = True

primop  ParLocalOp  "parLocal#"  GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   has_side_effects = True

primop  ParAtOp  "parAt#"  GenPrimOp
   b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
   with
   has_side_effects = True

primop  ParAtAbsOp  "parAtAbs#"  GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   has_side_effects = True

primop  ParAtRelOp  "parAtRel#" GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   has_side_effects = True

primop  ParAtForNowOp  "parAtForNow#" GenPrimOp
   b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
   with
   has_side_effects = True

-- copyable# and noFollow# are yet to be implemented (for GpH)
--
--primop  CopyableOp  "copyable#" GenPrimOp
--   a -> Int#
--   with
--   has_side_effects = True
--
--primop  NoFollowOp "noFollow#" GenPrimOp
--   a -> Int#
--   with
--   has_side_effects = True


------------------------------------------------------------------------
section "Tag to enum stuff"
	{Convert back and forth between values of enumerated types
	and small integers.}
------------------------------------------------------------------------

primop  DataToTagOp "dataToTag#" GenPrimOp
   a -> Int#
   with
   strictness  = { \ _arity -> mkClosedStrictSig [evalDmd] topRes }

	-- dataToTag# must have an evaluated argument

primop  TagToEnumOp "tagToEnum#" GenPrimOp
   Int# -> a

------------------------------------------------------------------------
section "Bytecode operations"
	{Support for the bytecode interpreter and linker.}
------------------------------------------------------------------------

primtype BCO#
   {Primitive bytecode type.}

primop   AddrToAnyOp "addrToAny#" GenPrimOp
   Addr# -> (# a #)
   {Convert an {\tt Addr\#} to a followable Any type.}
   with
   code_size = 0

primop   MkApUpd0_Op "mkApUpd0#" GenPrimOp
   BCO# -> (# a #)
   with
   out_of_line = True

primop  NewBCOOp "newBCO#" GenPrimOp
   ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> State# s -> (# State# s, BCO# #)
   with
   has_side_effects = True
   out_of_line      = True

primop  UnpackClosureOp "unpackClosure#" GenPrimOp
   a -> (# Addr#, Array# b, ByteArray# #)
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
     avoid the call to {\tt getCCCS\#} being floated out by the
     simplifier, which would result in an uninformative stack
     ("CAF"). }

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

pseudoop   "seq"
   a -> b -> b
   { Evaluates its first argument to head normal form, and then returns its second
	argument as the result. }

primtype Any k
	{ The type constructor {\tt Any} is type to which you can unsafely coerce any
	lifted type, and back.

	  * It is lifted, and hence represented by a pointer

	  * It does not claim to be a {\it data} type, and that's important for
	    the code generator, because the code gen may {\it enter} a data value
	    but never enters a function value.

	It's also used to instantiate un-constrained type variables after type
	checking.  For example, {\tt length} has type

	{\tt length :: forall a. [a] -> Int}

	and the list datacon for the empty list has type

	{\tt [] :: forall a. [a]}

	In order to compose these two terms as {\tt length []} a type
	application is required, but there is no constraint on the
	choice.  In this situation GHC uses {\tt Any}:

	{\tt length (Any *) ([] (Any *))}

        Note that {\tt Any} is kind polymorphic, and takes a kind {\tt k} as its
        first argument. The kind of {\tt Any} is thus {\tt forall k. k -> k}.}

primtype AnyK
        { The kind {\tt AnyK} is the kind level counterpart to {\tt Any}. In a
        kind polymorphic setting, a similar example to the length of the empty
        list can be given at the type level:

        {\tt type family Length (l :: [k]) :: Nat}
        {\tt type instance Length [] = Zero}

        When {\tt Length} is applied to the empty (promoted) list it will have
        the kind {\tt Length AnyK []}.

        {\tt AnyK} is currently not exported and cannot be used directly, but
        you might see it in debug output from the compiler.
        }

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

         * Casting an unboxed type to another unboxed type of the same size
           (but not coercions between floating-point and integral types)

         * Casting between two types that have the same runtime representation.  One case is when
           the two types differ only in "phantom" type parameters, for example
           {\tt Ptr Int} to {\tt Ptr Float}, or {\tt [Int]} to {\tt [Float]} when the list is
           known to be empty.  Also, a {\tt newtype} of a type {\tt T} has the same representation
           at runtime as {\tt T}.

        Other uses of {\tt unsafeCoerce\#} are undefined.  In particular, you should not use
	{\tt unsafeCoerce\#} to cast a T to an algebraic data type D, unless T is also
	an algebraic data type.  For example, do not cast {\tt Int->Int} to {\tt Bool}, even if
        you later cast that {\tt Bool} back to {\tt Int->Int} before applying it.  The reasons
        have to do with GHC's internal representation details (for the congnoscenti, data values
	can be entered but function closures cannot).  If you want a safe type to cast things
	to, use {\tt Any}, which is not an algebraic data type.

        }

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
     argument.  The event will be emitted either to the .eventlog file,
     or to stderr, depending on the runtime RTS flags. }
   with
   has_side_effects = True
   out_of_line      = True

primop  TraceMarkerOp "traceMarker#" GenPrimOp
   Addr# -> State# s -> State# s
   { Emits a marker event via the RTS tracing framework.  The contents
     of the event is the zero-terminated byte string passed as the first
     argument.  The event will be emitted either to the .eventlog file,
     or to stderr, depending on the runtime RTS flags. }
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
   }

primclass Coercible a b

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

  The {\tt prefetchMutableByteArray} family of operations has the order of operations
  determined by passing around the {\tt State#} token.

  For the {\tt prefetchByteArray}
  and {\tt prefetchAddr} families of operations, consider the following example:

  {\tt let a1 = prefetchByteArray2# a n in ...a1... }

  In the above fragement, {\tt a} is the input variable for the prefetch
  and {\tt a1 == a} will be true. To ensure that the prefetch is not treated as deadcode,
  the body of the let should only use {\tt a1} and NOT {\tt a}. The same principle
  applies for uses of prefetch in a loop.

  }


------------------------------------------------------------------------


--- the Int# argument for prefetch is the byte offset on the byteArray or  Addr#

---
primop PrefetchByteArrayOp3 "prefetchByteArray3#" GenPrimOp
   ByteArray# -> Int# -> ByteArray#

primop PrefetchMutableByteArrayOp3 "prefetchMutableByteArray3#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> State# s

primop PrefetchAddrOp3 "prefetchAddr3#" GenPrimOp
    Addr# -> Int# -> Addr#

----

primop PrefetchByteArrayOp2 "prefetchByteArray2#" GenPrimOp
   ByteArray# -> Int# -> ByteArray#

primop PrefetchMutableByteArrayOp2 "prefetchMutableByteArray2#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> State# s

primop PrefetchAddrOp2 "prefetchAddr2#" GenPrimOp
   Addr# -> Int# -> Addr#

----

primop PrefetchByteArrayOp1 "prefetchByteArray1#" GenPrimOp
   ByteArray# -> Int# -> ByteArray#

primop PrefetchMutableByteArrayOp1 "prefetchMutableByteArray1#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> State# s

primop PrefetchAddrOp1 "prefetchAddr1#" GenPrimOp
   Addr# -> Int# -> Addr#

----

primop PrefetchByteArrayOp0 "prefetchByteArray0#" GenPrimOp
   ByteArray# -> Int# -> ByteArray#

primop PrefetchMutableByteArrayOp0 "prefetchMutableByteArray0#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> State# s

primop PrefetchAddrOp0 "prefetchAddr0#" GenPrimOp
   Addr# -> Int# -> Addr#



------------------------------------------------------------------------
---                                                                  ---
------------------------------------------------------------------------

thats_all_folks
