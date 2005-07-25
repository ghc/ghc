-----------------------------------------------------------------------
-- $Id: primops.txt.pp,v 1.36 2005/07/25 14:12:48 simonmar Exp $
--
-- Primitive Operations
--
-----------------------------------------------------------------------

-- This file is processed by the utility program genprimopcode to produce
-- a number of include files within the compiler and optionally to produce
-- human-readable documentation.
--
-- It should first be preprocessed.
--
-- To add a new primop, you currently need to update the following files:
--
--	- this file (ghc/compiler/prelude/primops.txt.pp), which includes
--	  the type of the primop, and various other properties (its
--	  strictness attributes, whether it is defined as a macro
--	  or as out-of-line code, etc.)
--
--	- if the primop is inline (i.e. a macro), then:
--	  	ghc/compiler/AbsCUtils.lhs (dscCOpStmt)
--		  defines the translation of the primop into simpler
--		  abstract C operations.
--		
--	- or, for an out-of-line primop:
--	        ghc/includes/PrimOps.h (just add the declaration)
--		ghc/rts/PrimOps.hc     (define it here)
--		ghc/rts/Linker.c       (declare the symbol for GHCi)
--
--	- the User's Guide 
--

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
-- needed for strictness and usage info.

defaults
   has_side_effects = False
   out_of_line      = False
   commutable       = False
   needs_wrapper    = False
   can_fail         = False
   strictness       = { \ arity -> mkStrictSig (mkTopDmdType (replicate arity lazyDmd) TopRes) }
   usage            = { nomangle other }

-- Currently, documentation is produced using latex, so contents of
-- description fields should be legal latex. Descriptions can contain
-- matched pairs of embedded curly brackets.

#include "MachDeps.h"

-- We need platform defines (tests for mingw32 below).  However, we only
-- test the TARGET platform, which doesn't vary between stages, so the
-- stage1 platform defines are fine:
#include "../stage1/ghc_boot_platform.h"

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
	 \texttt{indexInt32Array\#} has type {\tt ByteArr\# -> Int\#
	 -> Int\#}; otherwise it has type {\tt ByteArr\# -> Int\# ->
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


primop   CharGtOp  "gtChar#"   Compare   Char# -> Char# -> Bool
primop   CharGeOp  "geChar#"   Compare   Char# -> Char# -> Bool

primop   CharEqOp  "eqChar#"   Compare
   Char# -> Char# -> Bool
   with commutable = True

primop   CharNeOp  "neChar#"   Compare
   Char# -> Char# -> Bool
   with commutable = True

primop   CharLtOp  "ltChar#"   Compare   Char# -> Char# -> Bool
primop   CharLeOp  "leChar#"   Compare   Char# -> Char# -> Bool

primop   OrdOp   "ord#"  GenPrimOp   Char# -> Int#

------------------------------------------------------------------------
section "Int#"
	{Operations on native-size integers (30+ bits).}
------------------------------------------------------------------------

primop   IntAddOp    "+#"    Dyadic
   Int# -> Int# -> Int#
   with commutable = True

primop   IntSubOp    "-#"    Dyadic   Int# -> Int# -> Int#

primop   IntMulOp    "*#" 
   Dyadic   Int# -> Int# -> Int#
   {Low word of signed integer multiply.}
   with commutable = True

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
    (*) :: Integer -> Integer -> Integer will be poor.
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

primop   IntGcdOp    "gcdInt#"    Dyadic   Int# -> Int# -> Int#
   with out_of_line = True

primop   IntNegOp    "negateInt#"    Monadic   Int# -> Int#
primop   IntAddCOp   "addIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
	 {Add with carry.  First member of result is (wrapped) sum; 
          second member is 0 iff no overflow occured.}
primop   IntSubCOp   "subIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
	 {Subtract with carry.  First member of result is (wrapped) difference; 
          second member is 0 iff no overflow occured.}

primop   IntGtOp  ">#"   Compare   Int# -> Int# -> Bool
primop   IntGeOp  ">=#"   Compare   Int# -> Int# -> Bool

primop   IntEqOp  "==#"   Compare
   Int# -> Int# -> Bool
   with commutable = True

primop   IntNeOp  "/=#"   Compare
   Int# -> Int# -> Bool
   with commutable = True

primop   IntLtOp  "<#"   Compare   Int# -> Int# -> Bool
primop   IntLeOp  "<=#"   Compare   Int# -> Int# -> Bool

primop   ChrOp   "chr#"   GenPrimOp   Int# -> Char#

primop   Int2WordOp "int2Word#" GenPrimOp Int# -> Word#
primop   Int2FloatOp   "int2Float#"      GenPrimOp  Int# -> Float#
primop   Int2DoubleOp   "int2Double#"          GenPrimOp  Int# -> Double#

primop   Int2IntegerOp    "int2Integer#"
   GenPrimOp Int# -> (# Int#, ByteArr# #)
   with out_of_line = True

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

primop   WordAddOp   "plusWord#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   WordSubOp   "minusWord#"   Dyadic   Word# -> Word# -> Word#

primop   WordMulOp   "timesWord#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   WordQuotOp   "quotWord#"   Dyadic   Word# -> Word# -> Word#
   with can_fail = True

primop   WordRemOp   "remWord#"   Dyadic   Word# -> Word# -> Word#
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

primop   Word2IntegerOp   "word2Integer#"   GenPrimOp 
   Word# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   WordGtOp   "gtWord#"   Compare   Word# -> Word# -> Bool
primop   WordGeOp   "geWord#"   Compare   Word# -> Word# -> Bool
primop   WordEqOp   "eqWord#"   Compare   Word# -> Word# -> Bool
primop   WordNeOp   "neWord#"   Compare   Word# -> Word# -> Bool
primop   WordLtOp   "ltWord#"   Compare   Word# -> Word# -> Bool
primop   WordLeOp   "leWord#"   Compare   Word# -> Word# -> Bool

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
	{Operations on 32-bit integers (Int32\#).  This type is only used
         if plain Int\# has less than 32 bits.  In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primop   Int32ToIntegerOp   "int32ToInteger#" GenPrimOp 
   Int32# -> (# Int#, ByteArr# #)
   with out_of_line = True


------------------------------------------------------------------------
section "Word32#"
	{Operations on 32-bit unsigned words. This type is only used 
	 if plain Word\# has less than 32 bits. In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primop   Word32ToIntegerOp   "word32ToInteger#" GenPrimOp
   Word32# -> (# Int#, ByteArr# #)
   with out_of_line = True


#endif 


#if WORD_SIZE_IN_BITS < 64
------------------------------------------------------------------------
section "Int64#"
	{Operations on 64-bit unsigned words. This type is only used 
	 if plain Int\# has less than 64 bits. In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primop   Int64ToIntegerOp   "int64ToInteger#" GenPrimOp 
   Int64# -> (# Int#, ByteArr# #)
   with out_of_line = True

------------------------------------------------------------------------
section "Word64#"
	{Operations on 64-bit unsigned words. This type is only used 
	 if plain Word\# has less than 64 bits. In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primop   Word64ToIntegerOp   "word64ToInteger#" GenPrimOp
   Word64# -> (# Int#, ByteArr# #)
   with out_of_line = True

#endif

------------------------------------------------------------------------
section "Integer#"
	{Operations on arbitrary-precision integers. These operations are 
implemented via the GMP package. An integer is represented as a pair
consisting of an Int\# representing the number of 'limbs' in use and
the sign, and a ByteArr\# containing the 'limbs' themselves.  Such pairs
are returned as unboxed pairs, but must be passed as separate
components.

For .NET these operations are implemented by foreign imports, so the
primops are omitted.}
------------------------------------------------------------------------

#ifndef ILX

primop   IntegerAddOp   "plusInteger#" GenPrimOp   
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with commutable = True
        out_of_line = True

primop   IntegerSubOp   "minusInteger#" GenPrimOp  
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   IntegerMulOp   "timesInteger#" GenPrimOp   
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with commutable = True
        out_of_line = True

primop   IntegerGcdOp   "gcdInteger#" GenPrimOp    
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   {Greatest common divisor.}
   with commutable = True
        out_of_line = True

primop   IntegerIntGcdOp   "gcdIntegerInt#" GenPrimOp
   Int# -> ByteArr# -> Int# -> Int#
   {Greatest common divisor, where second argument is an ordinary Int\#.}
   with out_of_line = True

primop   IntegerDivExactOp   "divExactInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   {Divisor is guaranteed to be a factor of dividend.}
   with out_of_line = True

primop   IntegerQuotOp   "quotInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   {Rounds towards zero.}
   with out_of_line = True

primop   IntegerRemOp   "remInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   {Satisfies \texttt{plusInteger\# (timesInteger\# (quotInteger\# x y) y) (remInteger\# x y) == x}.}
   with out_of_line = True

primop   IntegerCmpOp   "cmpInteger#"   GenPrimOp  
   Int# -> ByteArr# -> Int# -> ByteArr# -> Int#
   {Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument.}
   with needs_wrapper = True
        out_of_line = True

primop   IntegerCmpIntOp   "cmpIntegerInt#" GenPrimOp
   Int# -> ByteArr# -> Int# -> Int#
   {Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument, which
   is an ordinary Int\#.}
   with needs_wrapper = True
        out_of_line = True

primop   IntegerQuotRemOp   "quotRemInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr#, Int#, ByteArr# #)
   {Compute quot and rem simulaneously.}
   with can_fail = True
        out_of_line = True

primop   IntegerDivModOp    "divModInteger#"  GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr#, Int#, ByteArr# #)
   {Compute div and mod simultaneously, where div rounds towards negative infinity
    and\texttt{(q,r) = divModInteger\#(x,y)} implies \texttt{plusInteger\# (timesInteger\# q y) r = x}.}
   with can_fail = True
        out_of_line = True

primop   Integer2IntOp   "integer2Int#"    GenPrimOp
   Int# -> ByteArr# -> Int#
   with needs_wrapper = True
        out_of_line = True

primop   Integer2WordOp   "integer2Word#"   GenPrimOp
   Int# -> ByteArr# -> Word#
   with needs_wrapper = True
        out_of_line = True

#if WORD_SIZE_IN_BITS < 32
primop   IntegerToInt32Op   "integerToInt32#" GenPrimOp
   Int# -> ByteArr# -> Int32#

primop   IntegerToWord32Op   "integerToWord32#" GenPrimOp
   Int# -> ByteArr# -> Word32#
#endif

primop   IntegerAndOp  "andInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   IntegerOrOp  "orInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   IntegerXorOp  "xorInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   IntegerComplementOp  "complementInteger#" GenPrimOp
   Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

#endif /* ndef ILX */

------------------------------------------------------------------------
section "Double#"
	{Operations on double-precision (64 bit) floating-point numbers.}
------------------------------------------------------------------------

primop   DoubleGtOp ">##"   Compare   Double# -> Double# -> Bool
primop   DoubleGeOp ">=##"   Compare   Double# -> Double# -> Bool

primop DoubleEqOp "==##"   Compare
   Double# -> Double# -> Bool
   with commutable = True

primop DoubleNeOp "/=##"   Compare
   Double# -> Double# -> Bool
   with commutable = True

primop   DoubleLtOp "<##"   Compare   Double# -> Double# -> Bool
primop   DoubleLeOp "<=##"   Compare   Double# -> Double# -> Bool

primop   DoubleAddOp   "+##"   Dyadic
   Double# -> Double# -> Double#
   with commutable = True

primop   DoubleSubOp   "-##"   Dyadic   Double# -> Double# -> Double#

primop   DoubleMulOp   "*##"   Dyadic
   Double# -> Double# -> Double#
   with commutable = True

primop   DoubleDivOp   "/##"   Dyadic
   Double# -> Double# -> Double#
   with can_fail = True

primop   DoubleNegOp   "negateDouble#"  Monadic   Double# -> Double#

primop   Double2IntOp   "double2Int#"          GenPrimOp  Double# -> Int#
primop   Double2FloatOp   "double2Float#" GenPrimOp Double# -> Float#

primop   DoubleExpOp   "expDouble#"      Monadic
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleLogOp   "logDouble#"      Monadic         
   Double# -> Double#
   with
   needs_wrapper = True
   can_fail = True

primop   DoubleSqrtOp   "sqrtDouble#"      Monadic  
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleSinOp   "sinDouble#"      Monadic          
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleCosOp   "cosDouble#"      Monadic          
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleTanOp   "tanDouble#"      Monadic          
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleAsinOp   "asinDouble#"      Monadic 
   Double# -> Double#
   with
   needs_wrapper = True
   can_fail = True

primop   DoubleAcosOp   "acosDouble#"      Monadic  
   Double# -> Double#
   with
   needs_wrapper = True
   can_fail = True

primop   DoubleAtanOp   "atanDouble#"      Monadic  
   Double# -> Double#
   with
   needs_wrapper = True

primop   DoubleSinhOp   "sinhDouble#"      Monadic  
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleCoshOp   "coshDouble#"      Monadic  
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleTanhOp   "tanhDouble#"      Monadic  
   Double# -> Double#
   with needs_wrapper = True

primop   DoublePowerOp   "**##" Dyadic  
   Double# -> Double# -> Double#
   {Exponentiation.}
   with needs_wrapper = True

primop   DoubleDecodeOp   "decodeDouble#" GenPrimOp    
   Double# -> (# Int#, Int#, ByteArr# #)
   {Convert to arbitrary-precision integer.
    First Int\# in result is the exponent; second Int\# and ByteArr\# represent an Integer\# 
    holding the mantissa.}
   with out_of_line = True

------------------------------------------------------------------------
section "Float#" 
	{Operations on single-precision (32-bit) floating-point numbers.}
------------------------------------------------------------------------

primop   FloatGtOp  "gtFloat#"   Compare   Float# -> Float# -> Bool
primop   FloatGeOp  "geFloat#"   Compare   Float# -> Float# -> Bool

primop   FloatEqOp  "eqFloat#"   Compare
   Float# -> Float# -> Bool
   with commutable = True

primop   FloatNeOp  "neFloat#"   Compare
   Float# -> Float# -> Bool
   with commutable = True

primop   FloatLtOp  "ltFloat#"   Compare   Float# -> Float# -> Bool
primop   FloatLeOp  "leFloat#"   Compare   Float# -> Float# -> Bool

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

primop   FloatExpOp   "expFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatLogOp   "logFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True
        can_fail = True

primop   FloatSqrtOp   "sqrtFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatSinOp   "sinFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatCosOp   "cosFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatTanOp   "tanFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatAsinOp   "asinFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True
        can_fail = True

primop   FloatAcosOp   "acosFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True
        can_fail = True

primop   FloatAtanOp   "atanFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatSinhOp   "sinhFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatCoshOp   "coshFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatTanhOp   "tanhFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatPowerOp   "powerFloat#"      Dyadic   
   Float# -> Float# -> Float#
   with needs_wrapper = True

primop   Float2DoubleOp   "float2Double#" GenPrimOp  Float# -> Double#

primop   FloatDecodeOp   "decodeFloat#" GenPrimOp
   Float# -> (# Int#, Int#, ByteArr# #)
   {Convert to arbitrary-precision integer.
    First Int\# in result is the exponent; second Int\# and ByteArr\# represent an Integer\# 
    holding the mantissa.}
   with out_of_line = True

------------------------------------------------------------------------
section "Arrays"
	{Operations on Array\#.}
------------------------------------------------------------------------

primop  NewArrayOp "newArray#" GenPrimOp
   Int# -> a -> State# s -> (# State# s, MutArr# s a #)
   {Create a new mutable array of specified size (in bytes),
    in the specified state thread,
    with each element containing the specified initial value.}
   with
   usage       = { mangle NewArrayOp [mkP, mkM, mkP] mkM }
   out_of_line = True

primop  SameMutableArrayOp "sameMutableArray#" GenPrimOp
   MutArr# s a -> MutArr# s a -> Bool
   with
   usage = { mangle SameMutableArrayOp [mkP, mkP] mkM }

primop  ReadArrayOp "readArray#" GenPrimOp
   MutArr# s a -> Int# -> State# s -> (# State# s, a #)
   {Read from specified index of mutable array. Result is not yet evaluated.}
   with
   usage = { mangle ReadArrayOp [mkM, mkP, mkP] mkM }

primop  WriteArrayOp "writeArray#" GenPrimOp
   MutArr# s a -> Int# -> a -> State# s -> State# s
   {Write to specified index of mutable array.}
   with
   usage            = { mangle WriteArrayOp [mkM, mkP, mkM, mkP] mkR }
   has_side_effects = True

primop  IndexArrayOp "indexArray#" GenPrimOp
   Array# a -> Int# -> (# a #)
   {Read from specified index of immutable array. Result is packaged into
    an unboxed singleton; the result itself is not yet evaluated.}
   with
   usage = { mangle  IndexArrayOp [mkM, mkP] mkM }

primop  UnsafeFreezeArrayOp "unsafeFreezeArray#" GenPrimOp
   MutArr# s a -> State# s -> (# State# s, Array# a #)
   {Make a mutable array immutable, without copying.}
   with
   usage            = { mangle UnsafeFreezeArrayOp [mkM, mkP] mkM }
   has_side_effects = True

primop  UnsafeThawArrayOp  "unsafeThawArray#" GenPrimOp
   Array# a -> State# s -> (# State# s, MutArr# s a #)
   {Make an immutable array mutable, without copying.}
   with
   usage       = { mangle UnsafeThawArrayOp [mkM, mkP] mkM }
   out_of_line = True

------------------------------------------------------------------------
section "Byte Arrays"
	{Operations on ByteArray\#. A ByteArray\# is a just a region of
         raw memory in the garbage-collected heap, which is not scanned
         for pointers. It carries its own size (in bytes). There are
	 three sets of operations for accessing byte array contents:
	 index for reading from immutable byte arrays, and read/write
	 for mutable byte arrays.  Each set contains operations for 
	 a range of useful primitive data types.  Each operation takes	
	 an offset measured in terms of the size fo the primitive type
	 being read or written.}

------------------------------------------------------------------------

primop  NewByteArrayOp_Char "newByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutByteArr# s #)
   {Create a new mutable byte array of specified size (in bytes), in
    the specified state thread.}
   with out_of_line = True

primop  NewPinnedByteArrayOp_Char "newPinnedByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutByteArr# s #)
   {Create a mutable byte array that the GC guarantees not to move.}
   with out_of_line = True

primop  ByteArrayContents_Char "byteArrayContents#" GenPrimOp
   ByteArr# -> Addr#
   {Intended for use with pinned arrays; otherwise very unsafe!}

primop  SameMutableByteArrayOp "sameMutableByteArray#" GenPrimOp
   MutByteArr# s -> MutByteArr# s -> Bool

primop  UnsafeFreezeByteArrayOp "unsafeFreezeByteArray#" GenPrimOp
   MutByteArr# s -> State# s -> (# State# s, ByteArr# #)
   {Make a mutable byte array immutable, without copying.}
   with
   has_side_effects = True

primop  SizeofByteArrayOp "sizeofByteArray#" GenPrimOp  
   ByteArr# -> Int#

primop  SizeofMutableByteArrayOp "sizeofMutableByteArray#" GenPrimOp
   MutByteArr# s -> Int#


primop IndexByteArrayOp_Char "indexCharArray#" GenPrimOp
   ByteArr# -> Int# -> Char#
   {Read 8-bit character; offset in bytes.}

primop IndexByteArrayOp_WideChar "indexWideCharArray#" GenPrimOp
   ByteArr# -> Int# -> Char#
   {Read 31-bit character; offset in 4-byte words.}

primop IndexByteArrayOp_Int "indexIntArray#" GenPrimOp
   ByteArr# -> Int# -> Int#

primop IndexByteArrayOp_Word "indexWordArray#" GenPrimOp
   ByteArr# -> Int# -> Word#

primop IndexByteArrayOp_Addr "indexAddrArray#" GenPrimOp
   ByteArr# -> Int# -> Addr#

primop IndexByteArrayOp_Float "indexFloatArray#" GenPrimOp
   ByteArr# -> Int# -> Float#

primop IndexByteArrayOp_Double "indexDoubleArray#" GenPrimOp
   ByteArr# -> Int# -> Double#

primop IndexByteArrayOp_StablePtr "indexStablePtrArray#" GenPrimOp
   ByteArr# -> Int# -> StablePtr# a

primop IndexByteArrayOp_Int8 "indexInt8Array#" GenPrimOp
   ByteArr# -> Int# -> Int#

primop IndexByteArrayOp_Int16 "indexInt16Array#" GenPrimOp
   ByteArr# -> Int# -> Int#

primop IndexByteArrayOp_Int32 "indexInt32Array#" GenPrimOp
   ByteArr# -> Int# -> INT32

primop IndexByteArrayOp_Int64 "indexInt64Array#" GenPrimOp
   ByteArr# -> Int# -> INT64

primop IndexByteArrayOp_Word8 "indexWord8Array#" GenPrimOp
   ByteArr# -> Int# -> Word#

primop IndexByteArrayOp_Word16 "indexWord16Array#" GenPrimOp
   ByteArr# -> Int# -> Word#

primop IndexByteArrayOp_Word32 "indexWord32Array#" GenPrimOp
   ByteArr# -> Int# -> WORD32

primop IndexByteArrayOp_Word64 "indexWord64Array#" GenPrimOp
   ByteArr# -> Int# -> WORD64

primop  ReadByteArrayOp_Char "readCharArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 8-bit character; offset in bytes.}

primop  ReadByteArrayOp_WideChar "readWideCharArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 31-bit character; offset in 4-byte words.}

primop  ReadByteArrayOp_Int "readIntArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Int# #)

primop  ReadByteArrayOp_Word "readWordArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Word# #)

primop  ReadByteArrayOp_Addr "readAddrArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Addr# #)

primop  ReadByteArrayOp_Float "readFloatArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Float# #)

primop  ReadByteArrayOp_Double "readDoubleArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Double# #)

primop  ReadByteArrayOp_StablePtr "readStablePtrArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, StablePtr# a #)

primop  ReadByteArrayOp_Int8 "readInt8Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Int# #)

primop  ReadByteArrayOp_Int16 "readInt16Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Int# #)

primop  ReadByteArrayOp_Int32 "readInt32Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, INT32 #)

primop  ReadByteArrayOp_Int64 "readInt64Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, INT64 #)

primop  ReadByteArrayOp_Word8 "readWord8Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Word# #)

primop  ReadByteArrayOp_Word16 "readWord16Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Word# #)

primop  ReadByteArrayOp_Word32 "readWord32Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, WORD32 #)

primop  ReadByteArrayOp_Word64 "readWord64Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, WORD64 #)

primop  WriteByteArrayOp_Char "writeCharArray#" GenPrimOp
   MutByteArr# s -> Int# -> Char# -> State# s -> State# s
   {Write 8-bit character; offset in bytes.}
   with has_side_effects = True

primop  WriteByteArrayOp_WideChar "writeWideCharArray#" GenPrimOp
   MutByteArr# s -> Int# -> Char# -> State# s -> State# s
   {Write 31-bit character; offset in 4-byte words.}
   with has_side_effects = True

primop  WriteByteArrayOp_Int "writeIntArray#" GenPrimOp
   MutByteArr# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word "writeWordArray#" GenPrimOp
   MutByteArr# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Addr "writeAddrArray#" GenPrimOp
   MutByteArr# s -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Float "writeFloatArray#" GenPrimOp
   MutByteArr# s -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Double "writeDoubleArray#" GenPrimOp
   MutByteArr# s -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_StablePtr "writeStablePtrArray#" GenPrimOp
   MutByteArr# s -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Int8 "writeInt8Array#" GenPrimOp
   MutByteArr# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Int16 "writeInt16Array#" GenPrimOp
   MutByteArr# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Int32 "writeInt32Array#" GenPrimOp
   MutByteArr# s -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Int64 "writeInt64Array#" GenPrimOp
   MutByteArr# s -> Int# -> INT64 -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word8 "writeWord8Array#" GenPrimOp
   MutByteArr# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word16 "writeWord16Array#" GenPrimOp
   MutByteArr# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word32 "writeWord32Array#" GenPrimOp
   MutByteArr# s -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word64 "writeWord64Array#" GenPrimOp
   MutByteArr# s -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True

------------------------------------------------------------------------
section "Addr#"
	{Addr\# is an arbitrary machine address assumed to point outside
	 the garbage-collected heap.  

	 NB: {\tt nullAddr\#::Addr\#} is not a primop, but is defined in MkId.lhs.
	 It is the null address.}
------------------------------------------------------------------------

primop	 AddrAddOp "plusAddr#" GenPrimOp Addr# -> Int# -> Addr#
primop	 AddrSubOp "minusAddr#" GenPrimOp Addr# -> Addr# -> Int#
	 {Result is meaningless if two Addr\#s are so far apart that their
	 difference doesn't fit in an Int\#.}
primop	 AddrRemOp "remAddr#" GenPrimOp Addr# -> Int# -> Int#
	 {Return the remainder when the Addr\# arg, treated like an Int\#,
	  is divided by the Int\# arg.}
#if (WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64)
primop   Addr2IntOp  "addr2Int#"     GenPrimOp   Addr# -> Int#
	{Coerce directly from address to int. Strongly deprecated.}
primop   Int2AddrOp   "int2Addr#"    GenPrimOp  Int# -> Addr#
	{Coerce directly from int to address. Strongly deprecated.}
#endif

primop   AddrGtOp  "gtAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrGeOp  "geAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrEqOp  "eqAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrNeOp  "neAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrLtOp  "ltAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrLeOp  "leAddr#"   Compare   Addr# -> Addr# -> Bool

primop IndexOffAddrOp_Char "indexCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char#
   {Reads 8-bit character; offset in bytes.}

primop IndexOffAddrOp_WideChar "indexWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char#
   {Reads 31-bit character; offset in 4-byte words.}

primop IndexOffAddrOp_Int "indexIntOffAddr#" GenPrimOp
   Addr# -> Int# -> Int#

primop IndexOffAddrOp_Word "indexWordOffAddr#" GenPrimOp
   Addr# -> Int# -> Word#

primop IndexOffAddrOp_Addr "indexAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> Addr#

primop IndexOffAddrOp_Float "indexFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> Float#

primop IndexOffAddrOp_Double "indexDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> Double#

primop IndexOffAddrOp_StablePtr "indexStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> StablePtr# a

primop IndexOffAddrOp_Int8 "indexInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> Int#

primop IndexOffAddrOp_Int16 "indexInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int#

primop IndexOffAddrOp_Int32 "indexInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> INT32

primop IndexOffAddrOp_Int64 "indexInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> INT64

primop IndexOffAddrOp_Word8 "indexWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word#

primop IndexOffAddrOp_Word16 "indexWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word#

primop IndexOffAddrOp_Word32 "indexWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD32

primop IndexOffAddrOp_Word64 "indexWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD64

primop ReadOffAddrOp_Char "readCharOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Char# #)
   {Reads 8-bit character; offset in bytes.}

primop ReadOffAddrOp_WideChar "readWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Char# #)
   {Reads 31-bit character; offset in 4-byte words.}

primop ReadOffAddrOp_Int "readIntOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)

primop ReadOffAddrOp_Word "readWordOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)

primop ReadOffAddrOp_Addr "readAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Addr# #)

primop ReadOffAddrOp_Float "readFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Float# #)

primop ReadOffAddrOp_Double "readDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Double# #)

primop ReadOffAddrOp_StablePtr "readStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, StablePtr# a #)

primop ReadOffAddrOp_Int8 "readInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)

primop ReadOffAddrOp_Int16 "readInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)

primop ReadOffAddrOp_Int32 "readInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, INT32 #)

primop ReadOffAddrOp_Int64 "readInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, INT64 #)

primop ReadOffAddrOp_Word8 "readWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)

primop ReadOffAddrOp_Word16 "readWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)

primop ReadOffAddrOp_Word32 "readWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, WORD32 #)

primop ReadOffAddrOp_Word64 "readWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, WORD64 #)


primop  WriteOffAddrOp_Char "writeCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_WideChar "writeWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int "writeIntOffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word "writeWordOffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Addr "writeAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Float "writeFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Double "writeDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_StablePtr "writeStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int8 "writeInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int16 "writeInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int32 "writeInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int64 "writeInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> INT64 -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word8 "writeWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word16 "writeWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word32 "writeWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word64 "writeWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True

------------------------------------------------------------------------
section "Mutable variables"
	{Operations on MutVar\#s, which behave like single-element mutable arrays.}
------------------------------------------------------------------------

primop  NewMutVarOp "newMutVar#" GenPrimOp
   a -> State# s -> (# State# s, MutVar# s a #)
   {Create MutVar\# with specified initial value in specified state thread.}
   with
   usage       = { mangle NewMutVarOp [mkM, mkP] mkM }
   out_of_line = True

primop  ReadMutVarOp "readMutVar#" GenPrimOp
   MutVar# s a -> State# s -> (# State# s, a #)
   {Read contents of MutVar\#. Result is not yet evaluated.}
   with
   usage = { mangle ReadMutVarOp [mkM, mkP] mkM }

primop  WriteMutVarOp "writeMutVar#"  GenPrimOp
   MutVar# s a -> a -> State# s -> State# s
   {Write contents of MutVar\#.}
   with
   usage            = { mangle WriteMutVarOp [mkM, mkM, mkP] mkR }
   has_side_effects = True

primop  SameMutVarOp "sameMutVar#" GenPrimOp
   MutVar# s a -> MutVar# s a -> Bool
   with
   usage = { mangle SameMutVarOp [mkP, mkP] mkM }

-- not really the right type, but we don't know about pairs here.  The
-- correct type is
--
--   MutVar# s a -> (a -> (a,b)) -> State# s -> (# State# s, b #)
--
primop  AtomicModifyMutVarOp "atomicModifyMutVar#" GenPrimOp
   MutVar# s a -> (a -> b) -> State# s -> (# State# s, c #)
   with
   usage = { mangle AtomicModifyMutVarOp [mkP, mkM, mkP] mkM }
   has_side_effects = True
   out_of_line = True

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
	-- analyser about that!
   usage = { mangle CatchOp [mkM, mkM . (inFun CatchOp mkM mkM), mkP] mkM }
        --     [mkO, mkO . (inFun mkM mkO)] mkO
        -- might use caught action multiply
   out_of_line = True

primop  RaiseOp "raise#" GenPrimOp
   a -> b
   with
   strictness  = { \ arity -> mkStrictSig (mkTopDmdType [lazyDmd] BotRes) }
      -- NB: result is bottom
   usage       = { mangle RaiseOp [mkM] mkM }
   out_of_line = True

-- raiseIO# needs to be a primop, because exceptions in the IO monad
-- must be *precise* - we don't want the strictness analyser turning
-- one kind of bottom into another, as it is allowed to do in pure code.

primop  RaiseIOOp "raiseIO#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, b #)
   with
   out_of_line = True

primop  BlockAsyncExceptionsOp "blockAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   out_of_line = True

primop  UnblockAsyncExceptionsOp "unblockAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   out_of_line = True

------------------------------------------------------------------------
section "STM-accessible Mutable Variables"
------------------------------------------------------------------------

primop	AtomicallyOp "atomically#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> State# RealWorld -> (# State# RealWorld, a #)
   with
   out_of_line = True
   has_side_effects = True

primop  RetryOp "retry#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, a #)
   with 
   out_of_line = True
   has_side_effects = True

primop  CatchRetryOp "catchRetry#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   with 
   out_of_line = True
   has_side_effects = True

primop  CatchSTMOp "catchSTM#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   with 
   out_of_line = True
   has_side_effects = True

primop	NewTVarOp "newTVar#" GenPrimOp
       a
    -> State# s -> (# State# s, TVar# s a #)
   {Create a new Tar\# holding a specified initial value.}
   with
   out_of_line  = True

primop	ReadTVarOp "readTVar#" GenPrimOp
       TVar# s a
    -> State# s -> (# State# s, a #)
   {Read contents of TVar\#.  Result is not yet evaluated.}
   with
   out_of_line	= True

primop	WriteTVarOp "writeTVar#" GenPrimOp
       TVar# s a
    -> a
    -> State# s -> State# s
   {Write contents of TVar\#.}
   with
   out_of_line	    = True
   has_side_effects = True

primop  SameTVarOp "sameTVar#" GenPrimOp
   TVar# s a -> TVar# s a -> Bool


------------------------------------------------------------------------
section "Synchronized Mutable Variables"
	{Operations on MVar\#s, which are shared mutable variables
	({\it not} the same as MutVar\#s!). (Note: in a non-concurrent implementation,
	(MVar\# a) can be represented by (MutVar\# (Maybe a)).)}
------------------------------------------------------------------------


primop  NewMVarOp "newMVar#"  GenPrimOp
   State# s -> (# State# s, MVar# s a #)
   {Create new mvar; initially empty.}
   with
   usage       = { mangle NewMVarOp [mkP] mkR }
   out_of_line = True

primop  TakeMVarOp "takeMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, a #)
   {If mvar is empty, block until it becomes full.
   Then remove and return its contents, and set it empty.}
   with
   usage            = { mangle TakeMVarOp [mkM, mkP] mkM }
   has_side_effects = True
   out_of_line      = True

primop  TryTakeMVarOp "tryTakeMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, Int#, a #)
   {If mvar is empty, immediately return with integer 0 and value undefined.
   Otherwise, return with integer 1 and contents of mvar, and set mvar empty.}
   with
   usage            = { mangle TryTakeMVarOp [mkM, mkP] mkM }
   has_side_effects = True
   out_of_line      = True

primop  PutMVarOp "putMVar#" GenPrimOp
   MVar# s a -> a -> State# s -> State# s
   {If mvar is full, block until it becomes empty.
   Then store value arg as its new contents.}
   with
   usage            = { mangle PutMVarOp [mkM, mkM, mkP] mkR }
   has_side_effects = True
   out_of_line      = True

primop  TryPutMVarOp "tryPutMVar#" GenPrimOp
   MVar# s a -> a -> State# s -> (# State# s, Int# #)
   {If mvar is full, immediately return with integer 0.
    Otherwise, store value arg as mvar's new contents, and return with integer 1.}
   with
   usage            = { mangle TryPutMVarOp [mkM, mkM, mkP] mkR }
   has_side_effects = True
   out_of_line      = True

primop  SameMVarOp "sameMVar#" GenPrimOp
   MVar# s a -> MVar# s a -> Bool
   with
   usage = { mangle SameMVarOp [mkP, mkP] mkM }

primop  IsEmptyMVarOp "isEmptyMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, Int# #)
   {Return 1 if mvar is empty; 0 otherwise.}
   with
   usage = { mangle IsEmptyMVarOp [mkP, mkP] mkM }
   out_of_line = True

------------------------------------------------------------------------
section "Delay/wait operations"
------------------------------------------------------------------------

primop  DelayOp "delay#" GenPrimOp
   Int# -> State# s -> State# s
   {Sleep specified number of microseconds.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  WaitReadOp "waitRead#" GenPrimOp
   Int# -> State# s -> State# s
   {Block until input is available on specified file descriptor.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  WaitWriteOp "waitWrite#" GenPrimOp
   Int# -> State# s -> State# s
   {Block until output is possible on specified file descriptor.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

#ifdef mingw32_TARGET_OS
primop  AsyncReadOp "asyncRead#" GenPrimOp
   Int# -> Int# -> Int# -> Addr# -> State# RealWorld-> (# State# RealWorld, Int#, Int# #)
   {Asynchronously read bytes from specified file descriptor.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  AsyncWriteOp "asyncWrite#" GenPrimOp
   Int# -> Int# -> Int# -> Addr# -> State# RealWorld-> (# State# RealWorld, Int#, Int# #)
   {Asynchronously write bytes from specified file descriptor.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  AsyncDoProcOp "asyncDoProc#" GenPrimOp
   Addr# -> Addr# -> State# RealWorld-> (# State# RealWorld, Int#, Int# #)
   {Asynchronously perform procedure (first arg), passing it 2nd arg.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

#endif

------------------------------------------------------------------------
section "Concurrency primitives"
	{(In a non-concurrent implementation, ThreadId\# can be as singleton
	type, whose (unique) value is returned by myThreadId\#.  The 
	other operations can be omitted.)}
------------------------------------------------------------------------

primop  ForkOp "fork#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
   with
   usage            = { mangle ForkOp [mkO, mkP] mkR }
   has_side_effects = True
   out_of_line      = True

primop  KillThreadOp "killThread#"  GenPrimOp
   ThreadId# -> a -> State# RealWorld -> State# RealWorld
   with
   usage            = { mangle KillThreadOp [mkP, mkM, mkP] mkR }
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

primop LabelThreadOp "labelThread#" GenPrimOp
   ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line      = True
   
primop  IsCurrentThreadBoundOp "isCurrentThreadBound#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, Int# #)
   with
   out_of_line = True

------------------------------------------------------------------------
section "Weak pointers"
------------------------------------------------------------------------

-- note that tyvar "o" denotes openAlphaTyVar

primop  MkWeakOp "mkWeak#" GenPrimOp
   o -> b -> c -> State# RealWorld -> (# State# RealWorld, Weak# b #)
   with
   usage            = { mangle MkWeakOp [mkZ, mkM, mkM, mkP] mkM }
   has_side_effects = True
   out_of_line      = True

primop  DeRefWeakOp "deRefWeak#" GenPrimOp
   Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)
   with
   usage            = { mangle DeRefWeakOp [mkM, mkP] mkM }
   has_side_effects = True
   out_of_line      = True

primop  FinalizeWeakOp "finalizeWeak#" GenPrimOp
   Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, 
              (State# RealWorld -> (# State# RealWorld, () #)) #)
   with
   usage            = { mangle FinalizeWeakOp [mkM, mkP] 
                               (mkR . (inUB FinalizeWeakOp 
                                            [id,id,inFun FinalizeWeakOp mkR mkM])) }
   has_side_effects = True
   out_of_line      = True

primop TouchOp "touch#" GenPrimOp
   o -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True

------------------------------------------------------------------------
section "Stable pointers and names"
------------------------------------------------------------------------

primop  MakeStablePtrOp "makeStablePtr#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
   with
   usage            = { mangle MakeStablePtrOp [mkM, mkP] mkM }
   has_side_effects = True
   out_of_line      = True

primop  DeRefStablePtrOp "deRefStablePtr#" GenPrimOp
   StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
   with
   usage            = { mangle DeRefStablePtrOp [mkM, mkP] mkM }
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  EqStablePtrOp "eqStablePtr#" GenPrimOp
   StablePtr# a -> StablePtr# a -> Int#
   with
   usage            = { mangle EqStablePtrOp [mkP, mkP] mkR }
   has_side_effects = True

primop  MakeStableNameOp "makeStableName#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, StableName# a #)
   with
   usage            = { mangle MakeStableNameOp [mkZ, mkP] mkR }
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  EqStableNameOp "eqStableName#" GenPrimOp
   StableName# a -> StableName# a -> Int#
   with
   usage = { mangle EqStableNameOp [mkP, mkP] mkR }

primop  StableNameToIntOp "stableNameToInt#" GenPrimOp
   StableName# a -> Int#
   with
   usage = { mangle StableNameToIntOp [mkP] mkR }

------------------------------------------------------------------------
section "Unsafe pointer equality"
--  (#1 Bad Guy: Alistair Reid :)   
------------------------------------------------------------------------

primop  ReallyUnsafePtrEqualityOp "reallyUnsafePtrEquality#" GenPrimOp
   a -> a -> Int#
   with
   usage = { mangle ReallyUnsafePtrEqualityOp [mkZ, mkZ] mkR }

------------------------------------------------------------------------
section "Parallelism"
------------------------------------------------------------------------

primop  ParOp "par#" GenPrimOp
   a -> Int#
   with
   usage            = { mangle ParOp [mkO] mkR }
      -- Note that Par is lazy to avoid that the sparked thing
      -- gets evaluted strictly, which it should *not* be
   has_side_effects = True

-- HWL: The first 4 Int# in all par... annotations denote:
--   name, granularity info, size of result, degree of parallelism
--      Same  structure as _seq_ i.e. returns Int#
-- KSW: v, the second arg in parAt# and parAtForNow#, is used only to determine
--   `the processor containing the expression v'; it is not evaluated

primop  ParGlobalOp  "parGlobal#"  GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   usage            = { mangle ParGlobalOp [mkO, mkP, mkP, mkP, mkP, mkM] mkM }
   has_side_effects = True

primop  ParLocalOp  "parLocal#"  GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   usage            = { mangle ParLocalOp [mkO, mkP, mkP, mkP, mkP, mkM] mkM }
   has_side_effects = True

primop  ParAtOp  "parAt#"  GenPrimOp
   b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
   with
   usage            = { mangle ParAtOp [mkO, mkZ, mkP, mkP, mkP, mkP, mkM] mkM }
   has_side_effects = True

primop  ParAtAbsOp  "parAtAbs#"  GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   usage            = { mangle ParAtAbsOp [mkO, mkP, mkP, mkP, mkP, mkM] mkM }
   has_side_effects = True

primop  ParAtRelOp  "parAtRel#" GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   usage            = { mangle ParAtRelOp [mkO, mkP, mkP, mkP, mkP, mkM] mkM }
   has_side_effects = True

primop  ParAtForNowOp  "parAtForNow#" GenPrimOp
   b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
   with
   usage            = { mangle ParAtForNowOp [mkO, mkZ, mkP, mkP, mkP, mkP, mkM] mkM }
   has_side_effects = True

-- copyable# and noFollow# are yet to be implemented (for GpH)
--
--primop  CopyableOp  "copyable#" GenPrimOp
--   a -> Int#
--   with
--   usage            = { mangle CopyableOp [mkZ] mkR }
--   has_side_effects = True
--
--primop  NoFollowOp "noFollow#" GenPrimOp
--   a -> Int#
--   with
--   usage            = { mangle NoFollowOp [mkZ] mkR }
--   has_side_effects = True


------------------------------------------------------------------------
section "Tag to enum stuff"
	{Convert back and forth between values of enumerated types
	and small integers.}
------------------------------------------------------------------------

primop  DataToTagOp "dataToTag#" GenPrimOp
   a -> Int#
   with
   strictness  = { \ arity -> mkStrictSig (mkTopDmdType [seqDmd] TopRes) }
	-- dataToTag# must have an evaluated argument

primop  TagToEnumOp "tagToEnum#" GenPrimOp     
   Int# -> a

------------------------------------------------------------------------
section "Bytecode operations" 
	{Support for the bytecode interpreter and linker.}
------------------------------------------------------------------------


primop   AddrToHValueOp "addrToHValue#" GenPrimOp
   Addr# -> (# a #)
   {Convert an Addr\# to a followable type.}

primop   MkApUpd0_Op "mkApUpd0#" GenPrimOp
   BCO# -> (# a #)
   with
   out_of_line = True

primop  NewBCOOp "newBCO#" GenPrimOp
   ByteArr# -> ByteArr# -> Array# a -> ByteArr# -> Int# -> ByteArr# -> State# s -> (# State# s, BCO# #)
   with
   has_side_effects = True
   out_of_line      = True

------------------------------------------------------------------------
section "Coercion" 
	{{\tt unsafeCoerce\# :: a -> b} is not a primop, but is defined in MkId.lhs.}

------------------------------------------------------------------------


------------------------------------------------------------------------
---                                                                  ---
------------------------------------------------------------------------

thats_all_folks



