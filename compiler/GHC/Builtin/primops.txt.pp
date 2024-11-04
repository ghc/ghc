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
-- Note in particular that Haskell block-style comments are not recognized
-- here, so stick to '--' (even for Notes spanning multiple lines).

-- Note [GHC.Prim]
-- ~~~~~~~~~~~~~~~
-- GHC.Prim is a special module:
--
-- * It can be imported by any module (import GHC.Prim).
--   However, in the future we might change which functions are primitives
--   and which are defined in Haskell.
--   Users should import GHC.Exts, which reexports GHC.Prim and is more stable.
--   In particular, we might move some of the primops to 'foreign import prim'
--   (see ticket #16929 and Note [When do out-of-line primops go in primops.txt.pp])
--
-- * It provides primitives of three sorts:
--   - primitive types such as Int64#, MutableByteArray#
--   - primops such as (+#), newTVar#, touch#
--   - pseudoops such as realWorld#, nullAddr#
--
-- * The pseudoops are described in Note [ghcPrimIds (aka pseudoops)]
--   in GHC.Types.Id.Make.
--
-- * The primitives (primtypes, primops, pseudoops) cannot be defined in
--   source Haskell.
--   There is no GHC/Prim.hs file with definitions.
--   Instead, we support importing GHC.Prim by manually defining its
--   ModIface (see Iface.Load.ghcPrimIface).
--
-- * The primitives are listed in this file, primops.txt.pp.
--   It goes through CPP, which creates primops.txt.
--   It is then consumed by the utility program genprimopcode, which produces
--   the following three types of files.
--
--   1. The files with extension .hs-incl.
--      They can be found by grepping for hs-incl.
--      They are #included in compiler sources.
--
--      One of them, primop-data-decl.hs-incl, defines the PrimOp type:
--        data PrimOp
--         = IntAddOp
--         | IntSubOp
--         | CharGtOp
--         | CharGeOp
--         | ...
--
--      The remaining files define properties of the primops
--      by pattern matching, for example:
--        primOpFixity IntAddOp = Just (Fixity NoSourceText 6 InfixL)
--        primOpFixity IntSubOp = Just (Fixity NoSourceText 6 InfixL)
--        ...
--      This includes fixity, has-side-effects, commutability,
--      IDs used to generate Uniques etc.
--
--      Additionally, we pattern match on PrimOp when generating Cmm in
--      GHC/StgToCmm/Prim.hs.
--
--   2. The dummy Prim.hs file, which is used for Haddock and
--      contains descriptions taken from primops.txt.pp.
--      All definitions are replaced by placeholders.
--      See Note [GHC.Prim Docs] in genprimopcode.
--
--   3. The module PrimopWrappers.hs, which wraps every call for GHCi;
--      see Note [Primop wrappers] in GHC.Builtin.Primops for details.
--
-- * This file does not list internal-only equality types
--   (GHC.Builtin.Types.Prim.unexposedPrimTyCons and coercionToken#
--   in GHC.Types.Id.Make) which are defined but not exported from GHC.Prim.
--   Every export of GHC.Prim should be in listed in this file.
--
-- * The primitive types should be listed in primTyCons in Builtin.Types.Prim
--   in addition to primops.txt.pp.
--   (This task should be delegated to genprimopcode in the future.)
--
--
--
-- Information on how PrimOps are implemented and the steps necessary to
-- add a new one can be found in the Commentary:
--
--  https://gitlab.haskell.org/ghc/ghc/wikis/commentary/prim-ops
--
-- This file is divided into named sections, each containing or more
-- primop entries. Section headers have the format:
--
--      section "section-name" {haddock-description}
--
-- This information is used solely when producing documentation; it is
-- otherwise ignored.  The haddock-description is optional.
--
-- The format of each primop entry is as follows:
--
--      primop internal-name "name-in-program-text" category type {haddock-description} attributes

-- The default attribute values which apply if you don't specify
-- other ones.  Attribute values can be True, False, or arbitrary
-- text between curly brackets.  This is a kludge to enable
-- processors of this file to easily get hold of simple info
-- (eg, out_of_line), whilst avoiding parsing complex expressions
-- needed for strictness info.
--
-- type refers to the general category of the primop. There are only two:
--
--  * Compare:   A comparison operation of the shape a -> a -> Int#
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
-- derived from the 3-tuple. For the 3-tuple <Int64#,Int64#,2>, VECTOR expands to
-- Int64X2#, SCALAR expands to Int64#, and VECTUPLE expands to (# Int64#, Int64# #).

defaults
   has_side_effects = False
   out_of_line      = False   -- See Note [When do out-of-line primops go in primops.txt.pp]
   can_fail         = False   -- See Note [PrimOp can_fail and has_side_effects] in PrimOp
   commutable       = False
   code_size        = { primOpCodeSizeDefault }
   strictness       = { \ arity -> mkClosedDmdSig (replicate arity topDmd) topDiv }
   fixity           = Nothing
   llvm_only        = False
   vector           = []
   deprecated_msg   = {}      -- A non-empty message indicates deprecation

-- Note [When do out-of-line primops go in primops.txt.pp]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
-- superficial restrictions in `foreign import prim` which must be fixed
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

-- Note [Levity and representation polymorphic primops]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In the types of primops in this module,
--
-- * The names `a,b,c,s` stand for type variables of kind Type
--
-- * The names `v` and `w` stand for levity-polymorphic
--   type variables.
--   For example:
--      op :: v -> w -> Int
--   really means
--      op :: forall {l :: Levity} (a :: TYPE (BoxedRep l))
--                   {k :: Levity} (b :: TYPE (BoxedRep k)).
--            a -> b -> Int
--  Two important things to note:
--     - `v` and `w` have independent levities `l` and `k` (respectively), and
--       these are inferred (not specified), as seen from the curly brackets.
--     - `v` and `w` end up written as `a` and `b` (respectively) in types,
--       which means that one shouldn't write a primop type involving both
--       `a` and `v`, nor `b` and `w`.
--
-- * The names `o` and `p` stand for representation-polymorphic
--   type variables, similarly to `v` and `w` above. For example:
--      op :: o -> p -> Int
--   really means
--      op :: forall {q :: RuntimeRep} (a :: TYPE q)
--                   {r :: RuntimeRep} (b :: TYPE r)
--            a -> b -> Int
--   We note:
--    - `o` and `p` have independent `RuntimeRep`s `q` and `r`, which are
--       inferred type variables (like for `v` and `w` above).
--    - `o` and `p` share textual names with `a` and `b` (respectively).
--      This means one shouldn't write a type involving both `a` and `o`,
--      nor `b` and `p`, nor `o` and `v`, etc.

section "The word size story."
        {Haskell98 specifies that signed integers (type 'Int')
         must contain at least 30 bits. GHC always implements
         'Int' using the primitive type 'Int#', whose size equals
         the @MachDeps.h@ constant @WORD\_SIZE\_IN\_BITS@.
         This is normally set based on the @config.h@ parameter
         @SIZEOF\_HSWORD@, i.e., 32 bits on 32-bit machines, 64
         bits on 64-bit machines.

         GHC also implements a primitive unsigned integer type
         'Word#' which always has the same number of bits as 'Int#'.

         In addition, GHC supports families of explicit-sized integers
         and words at 8, 16, 32, and 64 bits, with the usual
         arithmetic operations, comparisons, and a range of
         conversions.

         Finally, there are strongly deprecated primops for coercing
         between 'Addr#', the primitive type of machine
         addresses, and 'Int#'.  These are pretty bogus anyway,
         but will work on existing 32-bit and 64-bit GHC targets; they
         are completely bogus when tag bits are used in 'Int#',
         so are not available in this case.}

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

primop Int8ToIntOp "int8ToInt#" GenPrimOp Int8# -> Int#
primop IntToInt8Op "intToInt8#" GenPrimOp Int# -> Int8#

primop Int8NegOp "negateInt8#" GenPrimOp Int8# -> Int8#

primop Int8AddOp "plusInt8#" GenPrimOp Int8# -> Int8# -> Int8#
  with
    commutable = True

primop Int8SubOp "subInt8#" GenPrimOp Int8# -> Int8# -> Int8#

primop Int8MulOp "timesInt8#" GenPrimOp Int8# -> Int8# -> Int8#
  with
    commutable = True

primop Int8QuotOp "quotInt8#" GenPrimOp Int8# -> Int8# -> Int8#
  with
    can_fail = True

primop Int8RemOp "remInt8#" GenPrimOp Int8# -> Int8# -> Int8#
  with
    can_fail = True

primop Int8QuotRemOp "quotRemInt8#" GenPrimOp Int8# -> Int8# -> (# Int8#, Int8# #)
  with
    can_fail = True

primop Int8SllOp "uncheckedShiftLInt8#"  GenPrimOp Int8# -> Int# -> Int8#
primop Int8SraOp "uncheckedShiftRAInt8#" GenPrimOp Int8# -> Int# -> Int8#
primop Int8SrlOp "uncheckedShiftRLInt8#" GenPrimOp Int8# -> Int# -> Int8#

primop Int8ToWord8Op "int8ToWord8#" GenPrimOp Int8# -> Word8#
   with code_size = 0

primop Int8EqOp "eqInt8#" Compare Int8# -> Int8# -> Int#
primop Int8GeOp "geInt8#" Compare Int8# -> Int8# -> Int#
primop Int8GtOp "gtInt8#" Compare Int8# -> Int8# -> Int#
primop Int8LeOp "leInt8#" Compare Int8# -> Int8# -> Int#
primop Int8LtOp "ltInt8#" Compare Int8# -> Int8# -> Int#
primop Int8NeOp "neInt8#" Compare Int8# -> Int8# -> Int#

------------------------------------------------------------------------
section "Word8#"
        {Operations on 8-bit unsigned words.}
------------------------------------------------------------------------

primtype Word8#

primop Word8ToWordOp "word8ToWord#" GenPrimOp Word8# -> Word#
primop WordToWord8Op "wordToWord8#" GenPrimOp Word# -> Word8#

primop Word8AddOp "plusWord8#" GenPrimOp Word8# -> Word8# -> Word8#
  with
    commutable = True

primop Word8SubOp "subWord8#" GenPrimOp Word8# -> Word8# -> Word8#

primop Word8MulOp "timesWord8#" GenPrimOp Word8# -> Word8# -> Word8#
  with
    commutable = True

primop Word8QuotOp "quotWord8#" GenPrimOp Word8# -> Word8# -> Word8#
  with
    can_fail = True

primop Word8RemOp "remWord8#" GenPrimOp Word8# -> Word8# -> Word8#
  with
    can_fail = True

primop Word8QuotRemOp "quotRemWord8#" GenPrimOp Word8# -> Word8# -> (# Word8#, Word8# #)
  with
    can_fail = True

primop Word8AndOp "andWord8#" GenPrimOp Word8# -> Word8# -> Word8#
   with commutable = True

primop Word8OrOp "orWord8#" GenPrimOp Word8# -> Word8# -> Word8#
   with commutable = True

primop Word8XorOp "xorWord8#" GenPrimOp Word8# -> Word8# -> Word8#
   with commutable = True

primop Word8NotOp "notWord8#" GenPrimOp Word8# -> Word8#

primop Word8SllOp "uncheckedShiftLWord8#"  GenPrimOp Word8# -> Int# -> Word8#
primop Word8SrlOp "uncheckedShiftRLWord8#" GenPrimOp Word8# -> Int# -> Word8#

primop Word8ToInt8Op "word8ToInt8#" GenPrimOp Word8# -> Int8#
   with code_size = 0

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

primop Int16ToIntOp "int16ToInt#" GenPrimOp Int16# -> Int#
primop IntToInt16Op "intToInt16#" GenPrimOp Int# -> Int16#

primop Int16NegOp "negateInt16#" GenPrimOp Int16# -> Int16#

primop Int16AddOp "plusInt16#" GenPrimOp Int16# -> Int16# -> Int16#
  with
    commutable = True

primop Int16SubOp "subInt16#" GenPrimOp Int16# -> Int16# -> Int16#

primop Int16MulOp "timesInt16#" GenPrimOp Int16# -> Int16# -> Int16#
  with
    commutable = True

primop Int16QuotOp "quotInt16#" GenPrimOp Int16# -> Int16# -> Int16#
  with
    can_fail = True

primop Int16RemOp "remInt16#" GenPrimOp Int16# -> Int16# -> Int16#
  with
    can_fail = True

primop Int16QuotRemOp "quotRemInt16#" GenPrimOp Int16# -> Int16# -> (# Int16#, Int16# #)
  with
    can_fail = True

primop Int16SllOp "uncheckedShiftLInt16#"  GenPrimOp Int16# -> Int# -> Int16#
primop Int16SraOp "uncheckedShiftRAInt16#" GenPrimOp Int16# -> Int# -> Int16#
primop Int16SrlOp "uncheckedShiftRLInt16#" GenPrimOp Int16# -> Int# -> Int16#

primop Int16ToWord16Op "int16ToWord16#" GenPrimOp Int16# -> Word16#
   with code_size = 0

primop Int16EqOp "eqInt16#" Compare Int16# -> Int16# -> Int#
primop Int16GeOp "geInt16#" Compare Int16# -> Int16# -> Int#
primop Int16GtOp "gtInt16#" Compare Int16# -> Int16# -> Int#
primop Int16LeOp "leInt16#" Compare Int16# -> Int16# -> Int#
primop Int16LtOp "ltInt16#" Compare Int16# -> Int16# -> Int#
primop Int16NeOp "neInt16#" Compare Int16# -> Int16# -> Int#

------------------------------------------------------------------------
section "Word16#"
        {Operations on 16-bit unsigned words.}
------------------------------------------------------------------------

primtype Word16#

primop Word16ToWordOp "word16ToWord#" GenPrimOp Word16# -> Word#
primop WordToWord16Op "wordToWord16#" GenPrimOp Word# -> Word16#

primop Word16AddOp "plusWord16#" GenPrimOp Word16# -> Word16# -> Word16#
  with
    commutable = True

primop Word16SubOp "subWord16#" GenPrimOp Word16# -> Word16# -> Word16#

primop Word16MulOp "timesWord16#" GenPrimOp Word16# -> Word16# -> Word16#
  with
    commutable = True

primop Word16QuotOp "quotWord16#" GenPrimOp Word16# -> Word16# -> Word16#
  with
    can_fail = True

primop Word16RemOp "remWord16#" GenPrimOp Word16# -> Word16# -> Word16#
  with
    can_fail = True

primop Word16QuotRemOp "quotRemWord16#" GenPrimOp Word16# -> Word16# -> (# Word16#, Word16# #)
  with
    can_fail = True

primop Word16AndOp "andWord16#" GenPrimOp Word16# -> Word16# -> Word16#
   with commutable = True

primop Word16OrOp "orWord16#" GenPrimOp Word16# -> Word16# -> Word16#
   with commutable = True

primop Word16XorOp "xorWord16#" GenPrimOp Word16# -> Word16# -> Word16#
   with commutable = True

primop Word16NotOp "notWord16#" GenPrimOp Word16# -> Word16#

primop Word16SllOp "uncheckedShiftLWord16#"  GenPrimOp Word16# -> Int# -> Word16#
primop Word16SrlOp "uncheckedShiftRLWord16#" GenPrimOp Word16# -> Int# -> Word16#

primop Word16ToInt16Op "word16ToInt16#" GenPrimOp Word16# -> Int16#
   with code_size = 0

primop Word16EqOp "eqWord16#" Compare Word16# -> Word16# -> Int#
primop Word16GeOp "geWord16#" Compare Word16# -> Word16# -> Int#
primop Word16GtOp "gtWord16#" Compare Word16# -> Word16# -> Int#
primop Word16LeOp "leWord16#" Compare Word16# -> Word16# -> Int#
primop Word16LtOp "ltWord16#" Compare Word16# -> Word16# -> Int#
primop Word16NeOp "neWord16#" Compare Word16# -> Word16# -> Int#

------------------------------------------------------------------------
section "Int32#"
        {Operations on 32-bit integers.}
------------------------------------------------------------------------

primtype Int32#

primop Int32ToIntOp "int32ToInt#" GenPrimOp Int32# -> Int#
primop IntToInt32Op "intToInt32#" GenPrimOp Int# -> Int32#

primop Int32NegOp "negateInt32#" GenPrimOp Int32# -> Int32#

primop Int32AddOp "plusInt32#" GenPrimOp Int32# -> Int32# -> Int32#
  with
    commutable = True

primop Int32SubOp "subInt32#" GenPrimOp Int32# -> Int32# -> Int32#

primop Int32MulOp "timesInt32#" GenPrimOp Int32# -> Int32# -> Int32#
  with
    commutable = True

primop Int32QuotOp "quotInt32#" GenPrimOp Int32# -> Int32# -> Int32#
  with
    can_fail = True

primop Int32RemOp "remInt32#" GenPrimOp Int32# -> Int32# -> Int32#
  with
    can_fail = True

primop Int32QuotRemOp "quotRemInt32#" GenPrimOp Int32# -> Int32# -> (# Int32#, Int32# #)
  with
    can_fail = True

primop Int32SllOp "uncheckedShiftLInt32#"  GenPrimOp Int32# -> Int# -> Int32#
primop Int32SraOp "uncheckedShiftRAInt32#" GenPrimOp Int32# -> Int# -> Int32#
primop Int32SrlOp "uncheckedShiftRLInt32#" GenPrimOp Int32# -> Int# -> Int32#

primop Int32ToWord32Op "int32ToWord32#" GenPrimOp Int32# -> Word32#
   with code_size = 0

primop Int32EqOp "eqInt32#" Compare Int32# -> Int32# -> Int#
primop Int32GeOp "geInt32#" Compare Int32# -> Int32# -> Int#
primop Int32GtOp "gtInt32#" Compare Int32# -> Int32# -> Int#
primop Int32LeOp "leInt32#" Compare Int32# -> Int32# -> Int#
primop Int32LtOp "ltInt32#" Compare Int32# -> Int32# -> Int#
primop Int32NeOp "neInt32#" Compare Int32# -> Int32# -> Int#

------------------------------------------------------------------------
section "Word32#"
        {Operations on 32-bit unsigned words.}
------------------------------------------------------------------------

primtype Word32#

primop Word32ToWordOp "word32ToWord#" GenPrimOp Word32# -> Word#
primop WordToWord32Op "wordToWord32#" GenPrimOp Word# -> Word32#

primop Word32AddOp "plusWord32#" GenPrimOp Word32# -> Word32# -> Word32#
  with
    commutable = True

primop Word32SubOp "subWord32#" GenPrimOp Word32# -> Word32# -> Word32#

primop Word32MulOp "timesWord32#" GenPrimOp Word32# -> Word32# -> Word32#
  with
    commutable = True

primop Word32QuotOp "quotWord32#" GenPrimOp Word32# -> Word32# -> Word32#
  with
    can_fail = True

primop Word32RemOp "remWord32#" GenPrimOp Word32# -> Word32# -> Word32#
  with
    can_fail = True

primop Word32QuotRemOp "quotRemWord32#" GenPrimOp Word32# -> Word32# -> (# Word32#, Word32# #)
  with
    can_fail = True

primop Word32AndOp "andWord32#" GenPrimOp Word32# -> Word32# -> Word32#
   with commutable = True

primop Word32OrOp "orWord32#" GenPrimOp Word32# -> Word32# -> Word32#
   with commutable = True

primop Word32XorOp "xorWord32#" GenPrimOp Word32# -> Word32# -> Word32#
   with commutable = True

primop Word32NotOp "notWord32#" GenPrimOp Word32# -> Word32#

primop Word32SllOp "uncheckedShiftLWord32#"  GenPrimOp Word32# -> Int# -> Word32#
primop Word32SrlOp "uncheckedShiftRLWord32#" GenPrimOp Word32# -> Int# -> Word32#

primop Word32ToInt32Op "word32ToInt32#" GenPrimOp Word32# -> Int32#
   with code_size = 0

primop Word32EqOp "eqWord32#" Compare Word32# -> Word32# -> Int#
primop Word32GeOp "geWord32#" Compare Word32# -> Word32# -> Int#
primop Word32GtOp "gtWord32#" Compare Word32# -> Word32# -> Int#
primop Word32LeOp "leWord32#" Compare Word32# -> Word32# -> Int#
primop Word32LtOp "ltWord32#" Compare Word32# -> Word32# -> Int#
primop Word32NeOp "neWord32#" Compare Word32# -> Word32# -> Int#

------------------------------------------------------------------------
section "Int64#"
        {Operations on 64-bit signed words.}
------------------------------------------------------------------------

primtype Int64#

primop Int64ToIntOp "int64ToInt#" GenPrimOp Int64# -> Int#
primop IntToInt64Op "intToInt64#" GenPrimOp Int# -> Int64#

primop Int64NegOp "negateInt64#" GenPrimOp Int64# -> Int64#

primop Int64AddOp "plusInt64#" GenPrimOp Int64# -> Int64# -> Int64#
  with
    commutable = True

primop Int64SubOp "subInt64#" GenPrimOp Int64# -> Int64# -> Int64#

primop Int64MulOp "timesInt64#" GenPrimOp Int64# -> Int64# -> Int64#
  with
    commutable = True

primop Int64QuotOp "quotInt64#" GenPrimOp Int64# -> Int64# -> Int64#
  with
    can_fail = True

primop Int64RemOp "remInt64#" GenPrimOp Int64# -> Int64# -> Int64#
  with
    can_fail = True

primop Int64SllOp "uncheckedIShiftL64#"  GenPrimOp Int64# -> Int# -> Int64#
primop Int64SraOp "uncheckedIShiftRA64#" GenPrimOp Int64# -> Int# -> Int64#
primop Int64SrlOp "uncheckedIShiftRL64#" GenPrimOp Int64# -> Int# -> Int64#

primop Int64ToWord64Op "int64ToWord64#" GenPrimOp Int64# -> Word64#
   with code_size = 0

primop Int64EqOp "eqInt64#" Compare Int64# -> Int64# -> Int#
primop Int64GeOp "geInt64#" Compare Int64# -> Int64# -> Int#
primop Int64GtOp "gtInt64#" Compare Int64# -> Int64# -> Int#
primop Int64LeOp "leInt64#" Compare Int64# -> Int64# -> Int#
primop Int64LtOp "ltInt64#" Compare Int64# -> Int64# -> Int#
primop Int64NeOp "neInt64#" Compare Int64# -> Int64# -> Int#

------------------------------------------------------------------------
section "Word64#"
        {Operations on 64-bit unsigned words.}
------------------------------------------------------------------------

primtype Word64#

primop Word64ToWordOp "word64ToWord#" GenPrimOp Word64# -> Word#
primop WordToWord64Op "wordToWord64#" GenPrimOp Word# -> Word64#

primop Word64AddOp "plusWord64#" GenPrimOp Word64# -> Word64# -> Word64#
  with
    commutable = True

primop Word64SubOp "subWord64#" GenPrimOp Word64# -> Word64# -> Word64#

primop Word64MulOp "timesWord64#" GenPrimOp Word64# -> Word64# -> Word64#
  with
    commutable = True

primop Word64QuotOp "quotWord64#" GenPrimOp Word64# -> Word64# -> Word64#
  with
    can_fail = True

primop Word64RemOp "remWord64#" GenPrimOp Word64# -> Word64# -> Word64#
  with
    can_fail = True

primop Word64AndOp "and64#" GenPrimOp Word64# -> Word64# -> Word64#
   with commutable = True

primop Word64OrOp "or64#" GenPrimOp Word64# -> Word64# -> Word64#
   with commutable = True

primop Word64XorOp "xor64#" GenPrimOp Word64# -> Word64# -> Word64#
   with commutable = True

primop Word64NotOp "not64#" GenPrimOp Word64# -> Word64#

primop Word64SllOp "uncheckedShiftL64#"  GenPrimOp Word64# -> Int# -> Word64#
primop Word64SrlOp "uncheckedShiftRL64#" GenPrimOp Word64# -> Int# -> Word64#

primop Word64ToInt64Op "word64ToInt64#" GenPrimOp Word64# -> Int64#
   with code_size = 0

primop Word64EqOp "eqWord64#" Compare Word64# -> Word64# -> Int#
primop Word64GeOp "geWord64#" Compare Word64# -> Word64# -> Int#
primop Word64GtOp "gtWord64#" Compare Word64# -> Word64# -> Int#
primop Word64LeOp "leWord64#" Compare Word64# -> Word64# -> Int#
primop Word64LtOp "ltWord64#" Compare Word64# -> Word64# -> Int#
primop Word64NeOp "neWord64#" Compare Word64# -> Word64# -> Int#

------------------------------------------------------------------------
section "Int#"
        {Operations on native-size integers (32+ bits).}
------------------------------------------------------------------------

primtype Int#

primop   IntAddOp    "+#"    GenPrimOp
   Int# -> Int# -> Int#
   with commutable = True
        fixity = infixl 6

primop   IntSubOp    "-#"    GenPrimOp   Int# -> Int# -> Int#
   with fixity = infixl 6

primop   IntMulOp    "*#"
   GenPrimOp   Int# -> Int# -> Int#
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
   GenPrimOp   Int# -> Int# -> Int#
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
    @(*) :: Integer -> Integer -> Integer@ will be poor.
   }
   with commutable = True

primop   IntQuotOp    "quotInt#"    GenPrimOp
   Int# -> Int# -> Int#
   {Rounds towards zero. The behavior is undefined if the second argument is
    zero.
   }
   with can_fail = True

primop   IntRemOp    "remInt#"    GenPrimOp
   Int# -> Int# -> Int#
   {Satisfies @('quotInt#' x y) '*#' y '+#' ('remInt#' x y) == x@. The
    behavior is undefined if the second argument is zero.
   }
   with can_fail = True

primop   IntQuotRemOp "quotRemInt#"    GenPrimOp
   Int# -> Int# -> (# Int#, Int# #)
   {Rounds towards zero.}
   with can_fail = True

primop   IntAndOp   "andI#"   GenPrimOp    Int# -> Int# -> Int#
   {Bitwise "and".}
   with commutable = True

primop   IntOrOp   "orI#"     GenPrimOp    Int# -> Int# -> Int#
   {Bitwise "or".}
   with commutable = True

primop   IntXorOp   "xorI#"   GenPrimOp    Int# -> Int# -> Int#
   {Bitwise "xor".}
   with commutable = True

primop   IntNotOp   "notI#"   GenPrimOp   Int# -> Int#
   {Bitwise "not", also known as the binary complement.}

primop   IntNegOp    "negateInt#"    GenPrimOp   Int# -> Int#
   {Unary negation.
    Since the negative 'Int#' range extends one further than the
    positive range, 'negateInt#' of the most negative number is an
    identity operation. This way, 'negateInt#' is always its own inverse.}

primop   IntAddCOp   "addIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
         {Add signed integers reporting overflow.
          First member of result is the sum truncated to an 'Int#';
          second member is zero if the true sum fits in an 'Int#',
          nonzero if overflow occurred (the sum is either too large
          or too small to fit in an 'Int#').}
   with code_size = 2
        commutable = True

primop   IntSubCOp   "subIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
         {Subtract signed integers reporting overflow.
          First member of result is the difference truncated to an 'Int#';
          second member is zero if the true difference fits in an 'Int#',
          nonzero if overflow occurred (the difference is either too large
          or too small to fit in an 'Int#').}
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

primop   IntToWordOp "int2Word#" GenPrimOp Int# -> Word#
   with code_size = 0

primop   IntToFloatOp   "int2Float#"      GenPrimOp  Int# -> Float#
   {Convert an 'Int#' to the corresponding 'Float#' with the same
    integral value (up to truncation due to floating-point precision). e.g.
    @'int2Float#' 1# == 1.0#@}
primop   IntToDoubleOp   "int2Double#"          GenPrimOp  Int# -> Double#
   {Convert an 'Int#' to the corresponding 'Double#' with the same
    integral value (up to truncation due to floating-point precision). e.g.
    @'int2Double#' 1# == 1.0##@}

primop   WordToFloatOp   "word2Float#"      GenPrimOp  Word# -> Float#
   {Convert an 'Word#' to the corresponding 'Float#' with the same
    integral value (up to truncation due to floating-point precision). e.g.
    @'word2Float#' 1## == 1.0#@}
primop   WordToDoubleOp   "word2Double#"          GenPrimOp  Word# -> Double#
   {Convert an 'Word#' to the corresponding 'Double#' with the same
    integral value (up to truncation due to floating-point precision). e.g.
    @'word2Double#' 1## == 1.0##@}

primop   IntSllOp   "uncheckedIShiftL#" GenPrimOp  Int# -> Int# -> Int#
         {Shift left.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   IntSraOp   "uncheckedIShiftRA#" GenPrimOp Int# -> Int# -> Int#
         {Shift right arithmetic.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   IntSrlOp   "uncheckedIShiftRL#" GenPrimOp Int# -> Int# -> Int#
         {Shift right logical.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}

------------------------------------------------------------------------
section "Word#"
        {Operations on native-sized unsigned words (32+ bits).}
------------------------------------------------------------------------

primtype Word#

primop   WordAddOp   "plusWord#"   GenPrimOp   Word# -> Word# -> Word#
   with commutable = True

primop   WordAddCOp   "addWordC#"   GenPrimOp   Word# -> Word# -> (# Word#, Int# #)
         {Add unsigned integers reporting overflow.
          The first element of the pair is the result.  The second element is
          the carry flag, which is nonzero on overflow. See also 'plusWord2#'.}
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
          component of the pair. See also 'addWordC#'.}
   with code_size = 2
        commutable = True

primop   WordSubOp   "minusWord#"   GenPrimOp   Word# -> Word# -> Word#

primop   WordMulOp   "timesWord#"   GenPrimOp   Word# -> Word# -> Word#
   with commutable = True

-- Returns (# high, low #)
primop   WordMul2Op  "timesWord2#"   GenPrimOp
   Word# -> Word# -> (# Word#, Word# #)
   with commutable = True

primop   WordQuotOp   "quotWord#"   GenPrimOp   Word# -> Word# -> Word#
   with can_fail = True

primop   WordRemOp   "remWord#"   GenPrimOp   Word# -> Word# -> Word#
   with can_fail = True

primop   WordQuotRemOp "quotRemWord#" GenPrimOp
   Word# -> Word# -> (# Word#, Word# #)
   with can_fail = True

primop   WordQuotRem2Op "quotRemWord2#" GenPrimOp
   Word# -> Word# -> Word# -> (# Word#, Word# #)
         { Takes high word of dividend, then low word of dividend, then divisor.
           Requires that high word < divisor.}
   with can_fail = True

primop   WordAndOp   "and#"   GenPrimOp   Word# -> Word# -> Word#
   with commutable = True

primop   WordOrOp   "or#"   GenPrimOp   Word# -> Word# -> Word#
   with commutable = True

primop   WordXorOp   "xor#"   GenPrimOp   Word# -> Word# -> Word#
   with commutable = True

primop   WordNotOp   "not#"   GenPrimOp   Word# -> Word#

primop   WordSllOp   "uncheckedShiftL#"   GenPrimOp   Word# -> Int# -> Word#
         {Shift left logical.   Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   WordSrlOp   "uncheckedShiftRL#"   GenPrimOp   Word# -> Int# -> Word#
         {Shift right logical.   Result undefined if shift  amount is not
          in the range 0 to word size - 1 inclusive.}

primop   WordToIntOp   "word2Int#"   GenPrimOp   Word# -> Int#
   with code_size = 0

primop   WordGtOp   "gtWord#"   Compare   Word# -> Word# -> Int#
primop   WordGeOp   "geWord#"   Compare   Word# -> Word# -> Int#
primop   WordEqOp   "eqWord#"   Compare   Word# -> Word# -> Int#
primop   WordNeOp   "neWord#"   Compare   Word# -> Word# -> Int#
primop   WordLtOp   "ltWord#"   Compare   Word# -> Word# -> Int#
primop   WordLeOp   "leWord#"   Compare   Word# -> Word# -> Int#

primop   PopCnt8Op   "popCnt8#"   GenPrimOp   Word# -> Word#
    {Count the number of set bits in the lower 8 bits of a word.}
primop   PopCnt16Op   "popCnt16#"   GenPrimOp   Word# -> Word#
    {Count the number of set bits in the lower 16 bits of a word.}
primop   PopCnt32Op   "popCnt32#"   GenPrimOp   Word# -> Word#
    {Count the number of set bits in the lower 32 bits of a word.}
primop   PopCnt64Op   "popCnt64#"   GenPrimOp   Word64# -> Word#
    {Count the number of set bits in a 64-bit word.}
primop   PopCntOp   "popCnt#"   GenPrimOp   Word# -> Word#
    {Count the number of set bits in a word.}

primop   Pdep8Op   "pdep8#"   GenPrimOp   Word# -> Word# -> Word#
    {Deposit bits to lower 8 bits of a word at locations specified by a mask.}
primop   Pdep16Op   "pdep16#"   GenPrimOp   Word# -> Word# -> Word#
    {Deposit bits to lower 16 bits of a word at locations specified by a mask.}
primop   Pdep32Op   "pdep32#"   GenPrimOp   Word# -> Word# -> Word#
    {Deposit bits to lower 32 bits of a word at locations specified by a mask.}
primop   Pdep64Op   "pdep64#"   GenPrimOp   Word64# -> Word64# -> Word64#
    {Deposit bits to a word at locations specified by a mask.}
primop   PdepOp   "pdep#"   GenPrimOp   Word# -> Word# -> Word#
    {Deposit bits to a word at locations specified by a mask.}

primop   Pext8Op   "pext8#"   GenPrimOp   Word# -> Word# -> Word#
    {Extract bits from lower 8 bits of a word at locations specified by a mask.}
primop   Pext16Op   "pext16#"   GenPrimOp   Word# -> Word# -> Word#
    {Extract bits from lower 16 bits of a word at locations specified by a mask.}
primop   Pext32Op   "pext32#"   GenPrimOp   Word# -> Word# -> Word#
    {Extract bits from lower 32 bits of a word at locations specified by a mask.}
primop   Pext64Op   "pext64#"   GenPrimOp   Word64# -> Word64# -> Word64#
    {Extract bits from a word at locations specified by a mask.}
primop   PextOp   "pext#"   GenPrimOp   Word# -> Word# -> Word#
    {Extract bits from a word at locations specified by a mask.}

primop   Clz8Op   "clz8#" GenPrimOp   Word# -> Word#
    {Count leading zeros in the lower 8 bits of a word.}
primop   Clz16Op   "clz16#" GenPrimOp   Word# -> Word#
    {Count leading zeros in the lower 16 bits of a word.}
primop   Clz32Op   "clz32#" GenPrimOp   Word# -> Word#
    {Count leading zeros in the lower 32 bits of a word.}
primop   Clz64Op   "clz64#" GenPrimOp Word64# -> Word#
    {Count leading zeros in a 64-bit word.}
primop   ClzOp     "clz#"   GenPrimOp   Word# -> Word#
    {Count leading zeros in a word.}

primop   Ctz8Op   "ctz8#"  GenPrimOp   Word# -> Word#
    {Count trailing zeros in the lower 8 bits of a word.}
primop   Ctz16Op   "ctz16#" GenPrimOp   Word# -> Word#
    {Count trailing zeros in the lower 16 bits of a word.}
primop   Ctz32Op   "ctz32#" GenPrimOp   Word# -> Word#
    {Count trailing zeros in the lower 32 bits of a word.}
primop   Ctz64Op   "ctz64#" GenPrimOp Word64# -> Word#
    {Count trailing zeros in a 64-bit word.}
primop   CtzOp     "ctz#"   GenPrimOp   Word# -> Word#
    {Count trailing zeros in a word.}

primop   BSwap16Op   "byteSwap16#"   GenPrimOp   Word# -> Word#
    {Swap bytes in the lower 16 bits of a word. The higher bytes are undefined. }
primop   BSwap32Op   "byteSwap32#"   GenPrimOp   Word# -> Word#
    {Swap bytes in the lower 32 bits of a word. The higher bytes are undefined. }
primop   BSwap64Op   "byteSwap64#"   GenPrimOp   Word64# -> Word64#
    {Swap bytes in a 64 bits of a word.}
primop   BSwapOp     "byteSwap#"     GenPrimOp   Word# -> Word#
    {Swap bytes in a word.}

primop   BRev8Op    "bitReverse8#"   GenPrimOp   Word# -> Word#
    {Reverse the order of the bits in a 8-bit word.}
primop   BRev16Op   "bitReverse16#"   GenPrimOp   Word# -> Word#
    {Reverse the order of the bits in a 16-bit word.}
primop   BRev32Op   "bitReverse32#"   GenPrimOp   Word# -> Word#
    {Reverse the order of the bits in a 32-bit word.}
primop   BRev64Op   "bitReverse64#"   GenPrimOp   Word64# -> Word64#
    {Reverse the order of the bits in a 64-bit word.}
primop   BRevOp     "bitReverse#"     GenPrimOp   Word# -> Word#
    {Reverse the order of the bits in a word.}

------------------------------------------------------------------------
section "Narrowings"
        {Explicit narrowing of native-sized ints or words.}
------------------------------------------------------------------------

primop   Narrow8IntOp      "narrow8Int#"      GenPrimOp   Int# -> Int#
primop   Narrow16IntOp     "narrow16Int#"     GenPrimOp   Int# -> Int#
primop   Narrow32IntOp     "narrow32Int#"     GenPrimOp   Int# -> Int#
primop   Narrow8WordOp     "narrow8Word#"     GenPrimOp   Word# -> Word#
primop   Narrow16WordOp    "narrow16Word#"    GenPrimOp   Word# -> Word#
primop   Narrow32WordOp    "narrow32Word#"    GenPrimOp   Word# -> Word#

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

primop   DoubleAddOp   "+##"   GenPrimOp
   Double# -> Double# -> Double#
   with commutable = True
        fixity = infixl 6

primop   DoubleSubOp   "-##"   GenPrimOp   Double# -> Double# -> Double#
   with fixity = infixl 6

primop   DoubleMulOp   "*##"   GenPrimOp
   Double# -> Double# -> Double#
   with commutable = True
        fixity = infixl 7

primop   DoubleDivOp   "/##"   GenPrimOp
   Double# -> Double# -> Double#
   with can_fail = True
        fixity = infixl 7

primop   DoubleNegOp   "negateDouble#"  GenPrimOp   Double# -> Double#

primop   DoubleFabsOp  "fabsDouble#"    GenPrimOp   Double# -> Double#

primop   DoubleToIntOp   "double2Int#"          GenPrimOp  Double# -> Int#
   {Truncates a 'Double#' value to the nearest 'Int#'.
    Results are undefined if the truncation if truncation yields
    a value outside the range of 'Int#'.}

primop   DoubleToFloatOp   "double2Float#" GenPrimOp Double# -> Float#

primop   DoubleExpOp   "expDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleExpM1Op "expm1Double#"    GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleLogOp   "logDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleLog1POp   "log1pDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleSqrtOp   "sqrtDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleSinOp   "sinDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleCosOp   "cosDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleTanOp   "tanDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAsinOp   "asinDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleAcosOp   "acosDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleAtanOp   "atanDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleSinhOp   "sinhDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleCoshOp   "coshDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleTanhOp   "tanhDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAsinhOp   "asinhDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAcoshOp   "acoshDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAtanhOp   "atanhDouble#"      GenPrimOp
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoublePowerOp   "**##" GenPrimOp
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
   Double# -> (# Int64#, Int# #)
   {Decode 'Double#' into mantissa and base-2 exponent.}
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

primop   FloatAddOp   "plusFloat#"      GenPrimOp
   Float# -> Float# -> Float#
   with commutable = True

primop   FloatSubOp   "minusFloat#"      GenPrimOp      Float# -> Float# -> Float#

primop   FloatMulOp   "timesFloat#"      GenPrimOp
   Float# -> Float# -> Float#
   with commutable = True

primop   FloatDivOp   "divideFloat#"      GenPrimOp
   Float# -> Float# -> Float#
   with can_fail = True

primop   FloatNegOp   "negateFloat#"      GenPrimOp    Float# -> Float#

primop   FloatFabsOp  "fabsFloat#"        GenPrimOp    Float# -> Float#

primop   FloatToIntOp   "float2Int#"      GenPrimOp  Float# -> Int#
   {Truncates a 'Float#' value to the nearest 'Int#'.
    Results are undefined if the truncation if truncation yields
    a value outside the range of 'Int#'.}

primop   FloatExpOp   "expFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatExpM1Op   "expm1Float#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatLogOp   "logFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatLog1POp  "log1pFloat#"     GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatSqrtOp   "sqrtFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatSinOp   "sinFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatCosOp   "cosFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatTanOp   "tanFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAsinOp   "asinFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatAcosOp   "acosFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatAtanOp   "atanFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatSinhOp   "sinhFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatCoshOp   "coshFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatTanhOp   "tanhFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAsinhOp   "asinhFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAcoshOp   "acoshFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAtanhOp   "atanhFloat#"      GenPrimOp
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatPowerOp   "powerFloat#"      GenPrimOp
   Float# -> Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatToDoubleOp   "float2Double#" GenPrimOp  Float# -> Double#

primop   FloatDecode_IntOp   "decodeFloat_Int#" GenPrimOp
   Float# -> (# Int#, Int# #)
   {Convert to integers.
    First 'Int#' in result is the mantissa; second is the exponent.}
   with out_of_line = True

------------------------------------------------------------------------
section "Arrays"
        {Operations on 'Array#'.}
------------------------------------------------------------------------

primtype Array# a

primtype MutableArray# s a

primop  NewArrayOp "newArray#" GenPrimOp
   Int# -> v -> State# s -> (# State# s, MutableArray# s v #)
   {Create a new mutable array with the specified number of elements,
    in the specified state thread,
    with each element containing the specified initial value.}
   with
   out_of_line = True
   has_side_effects = True

primop  ReadArrayOp "readArray#" GenPrimOp
   MutableArray# s v -> Int# -> State# s -> (# State# s, v #)
   {Read from specified index of mutable array. Result is not yet evaluated.}
   with
   has_side_effects = True
   can_fail         = True

primop  WriteArrayOp "writeArray#" GenPrimOp
   MutableArray# s v -> Int# -> v -> State# s -> State# s
   {Write to specified index of mutable array.}
   with
   has_side_effects = True
   can_fail         = True
   code_size        = 2 -- card update too

primop  SizeofArrayOp "sizeofArray#" GenPrimOp
   Array# v -> Int#
   {Return the number of elements in the array.}

primop  SizeofMutableArrayOp "sizeofMutableArray#" GenPrimOp
   MutableArray# s v -> Int#
   {Return the number of elements in the array.}

primop  IndexArrayOp "indexArray#" GenPrimOp
   Array# v -> Int# -> (# v #)
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
   MutableArray# s v -> State# s -> (# State# s, Array# v #)
   {Make a mutable array immutable, without copying.}
   with
   has_side_effects = True

primop  UnsafeThawArrayOp  "unsafeThawArray#" GenPrimOp
   Array# v -> State# s -> (# State# s, MutableArray# s v #)
   {Make an immutable array mutable, without copying.}
   with
   out_of_line = True
   has_side_effects = True

primop  CopyArrayOp "copyArray#" GenPrimOp
  Array# v -> Int# -> MutableArray# s v -> Int# -> Int# -> State# s -> State# s
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
  MutableArray# s v -> Int# -> MutableArray# s v -> Int# -> Int# -> State# s -> State# s
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
  Array# v -> Int# -> Int# -> Array# v
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneMutableArrayOp "cloneMutableArray#" GenPrimOp
  MutableArray# s v -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s v #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  FreezeArrayOp "freezeArray#" GenPrimOp
  MutableArray# s v -> Int# -> Int# -> State# s -> (# State# s, Array# v #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  ThawArrayOp "thawArray#" GenPrimOp
  Array# v -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s v #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop CasArrayOp  "casArray#" GenPrimOp
   MutableArray# s v -> Int# -> v -> v -> State# s -> (# State# s, Int#, v #)
   {Given an array, an offset, the expected old value, and
    the new value, perform an atomic compare and swap (i.e. write the new
    value if the current value and the old value are the same pointer).
    Returns 0 if the swap succeeds and 1 if it fails. Additionally, returns
    the element at the offset after the operation completes. This means that
    on a success the new value is returned, and on a failure the actual old
    value (not the expected one) is returned. Implies a full memory barrier.
    The use of a pointer equality on a boxed value makes this function harder
    to use correctly than 'casIntArray#'. All of the difficulties
    of using 'reallyUnsafePtrEquality#' correctly apply to
    'casArray#' as well.
   }
   with
   out_of_line = True
   has_side_effects = True
   can_fail = True -- Might index out of bounds


------------------------------------------------------------------------
section "Small Arrays"

        {Operations on 'SmallArray#'. A 'SmallArray#' works
         just like an 'Array#', but with different space use and
         performance characteristics (that are often useful with small
         arrays). The 'SmallArray#' and 'SmallMutableArray#'
         lack a `card table'. The purpose of a card table is to avoid
         having to scan every element of the array on each GC by
         keeping track of which elements have changed since the last GC
         and only scanning those that have changed. So the consequence
         of there being no card table is that the representation is
         somewhat smaller and the writes are somewhat faster (because
         the card table does not need to be updated). The disadvantage
         of course is that for a 'SmallMutableArray#' the whole
         array has to be scanned on each GC. Thus it is best suited for
         use cases where the mutable array is not long lived, e.g.
         where a mutable array is initialised quickly and then frozen
         to become an immutable 'SmallArray#'.
        }

------------------------------------------------------------------------

primtype SmallArray# a

primtype SmallMutableArray# s a

primop  NewSmallArrayOp "newSmallArray#" GenPrimOp
   Int# -> v -> State# s -> (# State# s, SmallMutableArray# s v #)
   {Create a new mutable array with the specified number of elements,
    in the specified state thread,
    with each element containing the specified initial value.}
   with
   out_of_line = True
   has_side_effects = True

primop  ShrinkSmallMutableArrayOp_Char "shrinkSmallMutableArray#" GenPrimOp
   SmallMutableArray# s v -> Int# -> State# s -> State# s
   {Shrink mutable array to new specified size, in
    the specified state thread. The new size argument must be less than or
    equal to the current size as reported by 'getSizeofSmallMutableArray#'.}
   with out_of_line = True
        has_side_effects = True

primop  ReadSmallArrayOp "readSmallArray#" GenPrimOp
   SmallMutableArray# s v -> Int# -> State# s -> (# State# s, v #)
   {Read from specified index of mutable array. Result is not yet evaluated.}
   with
   has_side_effects = True
   can_fail         = True

primop  WriteSmallArrayOp "writeSmallArray#" GenPrimOp
   SmallMutableArray# s v -> Int# -> v -> State# s -> State# s
   {Write to specified index of mutable array.}
   with
   has_side_effects = True
   can_fail         = True

primop  SizeofSmallArrayOp "sizeofSmallArray#" GenPrimOp
   SmallArray# v -> Int#
   {Return the number of elements in the array.}

primop  SizeofSmallMutableArrayOp "sizeofSmallMutableArray#" GenPrimOp
   SmallMutableArray# s v -> Int#
   {Return the number of elements in the array. Note that this is deprecated
   as it is unsafe in the presence of shrink and resize operations on the
   same small mutable array.}
   with deprecated_msg = { Use 'getSizeofSmallMutableArray#' instead }

primop  GetSizeofSmallMutableArrayOp "getSizeofSmallMutableArray#" GenPrimOp
   SmallMutableArray# s v -> State# s -> (# State# s, Int# #)
   {Return the number of elements in the array.}

primop  IndexSmallArrayOp "indexSmallArray#" GenPrimOp
   SmallArray# v -> Int# -> (# v #)
   {Read from specified index of immutable array. Result is packaged into
    an unboxed singleton; the result itself is not yet evaluated.}
   with
   can_fail         = True

primop  UnsafeFreezeSmallArrayOp "unsafeFreezeSmallArray#" GenPrimOp
   SmallMutableArray# s v -> State# s -> (# State# s, SmallArray# v #)
   {Make a mutable array immutable, without copying.}
   with
   has_side_effects = True

primop  UnsafeThawSmallArrayOp  "unsafeThawSmallArray#" GenPrimOp
   SmallArray# v -> State# s -> (# State# s, SmallMutableArray# s v #)
   {Make an immutable array mutable, without copying.}
   with
   out_of_line = True
   has_side_effects = True

-- The code_size is only correct for the case when the copy family of
-- primops aren't inlined. It would be nice to keep track of both.

primop  CopySmallArrayOp "copySmallArray#" GenPrimOp
  SmallArray# v -> Int# -> SmallMutableArray# s v -> Int# -> Int# -> State# s -> State# s
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
  SmallMutableArray# s v -> Int# -> SmallMutableArray# s v -> Int# -> Int# -> State# s -> State# s
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
  SmallArray# v -> Int# -> Int# -> SmallArray# v
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneSmallMutableArrayOp "cloneSmallMutableArray#" GenPrimOp
  SmallMutableArray# s v -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s v #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  FreezeSmallArrayOp "freezeSmallArray#" GenPrimOp
  SmallMutableArray# s v -> Int# -> Int# -> State# s -> (# State# s, SmallArray# v #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  ThawSmallArrayOp "thawSmallArray#" GenPrimOp
  SmallArray# v -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s v #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop CasSmallArrayOp  "casSmallArray#" GenPrimOp
   SmallMutableArray# s v -> Int# -> v -> v -> State# s -> (# State# s, Int#, v #)
   {Unsafe, machine-level atomic compare and swap on an element within an array.
    See the documentation of 'casArray#'.}
   with
   out_of_line = True
   has_side_effects = True
   can_fail = True -- Might index out of bounds

------------------------------------------------------------------------
section "Byte Arrays"
        {A 'ByteArray#' is a region of
         raw memory in the garbage-collected heap, which is not
         scanned for pointers.
         There are three sets of operations for accessing byte array contents:
         index for reading from immutable byte arrays, and read/write
         for mutable byte arrays.  Each set contains operations for a
         range of useful primitive data types.  Each operation takes
         an offset measured in terms of the size of the primitive type
         being read or written.

         }

------------------------------------------------------------------------

primtype ByteArray#
{
  A boxed, unlifted datatype representing a region of raw memory in the garbage-collected heap,
  which is not scanned for pointers during garbage collection.

  It is created by freezing a 'MutableByteArray#' with 'unsafeFreezeByteArray#'.
  Freezing is essentially a no-op, as 'MutableByteArray#' and 'ByteArray#' share the same heap structure under the hood.

  The immutable and mutable variants are commonly used for scenarios requiring high-performance data structures,
  like @Text@, @Primitive Vector@, @Unboxed Array@, and @ShortByteString@.

  Another application of fundamental importance is 'Integer', which is backed by 'ByteArray#'.

  The representation on the heap of a Byte Array is:

  > +------------+-----------------+-----------------------+
  > |            |                 |                       |
  > |   HEADER   | SIZE (in bytes) |       PAYLOAD         |
  > |            |                 |                       |
  > +------------+-----------------+-----------------------+

  To obtain a pointer to actual payload (e.g., for FFI purposes) use 'byteArrayContents#' or 'mutableByteArrayContents#'.

  Alternatively, enabling the @UnliftedFFITypes@ extension
  allows to mention 'ByteArray#' and 'MutableByteArray#' in FFI type signatures directly.
}

primtype MutableByteArray# s
{ A mutable 'ByteAray#'. It can be created in three ways:

  * 'newByteArray#': Create an unpinned array.
  * 'newPinnedByteArray#': This will create a pinned array,
  * 'newAlignedPinnedByteArray#': This will create a pinned array, with a custom alignment.

  Unpinned arrays can be moved around during garbage collection, so you must not store or pass pointers to these values
  if there is a chance for the garbage collector to kick in. That said, even unpinned arrays can be passed to unsafe FFI calls,
  because no garbage collection happens during these unsafe calls
  (see [Guaranteed Call Safety](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ffi.html#guaranteed-call-safety)
  in the GHC Manual). For safe FFI calls, byte arrays must be not only pinned, but also kept alive by means of the keepAlive# function
  for the duration of a call (that's because garbage collection cannot move a pinned array, but is free to scrap it altogether).
}

primop  NewByteArrayOp_Char "newByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutableByteArray# s #)
   {Create a new mutable byte array of specified size (in bytes), in
    the specified state thread. The size of the memory underlying the
    array will be rounded up to the platform's word size.}
   with out_of_line = True
        has_side_effects = True

primop  NewPinnedByteArrayOp_Char "newPinnedByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutableByteArray# s #)
   {Like 'newByteArray#' but GC guarantees not to move it.}
   with out_of_line = True
        has_side_effects = True

primop  NewAlignedPinnedByteArrayOp_Char "newAlignedPinnedByteArray#" GenPrimOp
   Int# -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
   {Like 'newPinnedByteArray#' but allow specifying an arbitrary
    alignment, which must be a power of two.}
   with out_of_line = True
        has_side_effects = True

primop  MutableByteArrayIsPinnedOp "isMutableByteArrayPinned#" GenPrimOp
   MutableByteArray# s -> Int#
   {Determine whether a 'MutableByteArray#' is guaranteed not to move
   during GC.}
   with out_of_line = True

primop  ByteArrayIsPinnedOp "isByteArrayPinned#" GenPrimOp
   ByteArray# -> Int#
   {Determine whether a 'ByteArray#' is guaranteed not to move during GC.}
   with out_of_line = True

primop  ByteArrayContents_Char "byteArrayContents#" GenPrimOp
   ByteArray# -> Addr#
   {Intended for use with pinned arrays; otherwise very unsafe!}

primop  MutableByteArrayContents_Char "mutableByteArrayContents#" GenPrimOp
   MutableByteArray# s -> Addr#
   {Intended for use with pinned arrays; otherwise very unsafe!}

primop  ShrinkMutableByteArrayOp_Char "shrinkMutableByteArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> State# s
   {Shrink mutable byte array to new specified size (in bytes), in
    the specified state thread. The new size argument must be less than or
    equal to the current size as reported by 'getSizeofMutableByteArray#'.}
   with out_of_line = True
        has_side_effects = True

primop  ResizeMutableByteArrayOp_Char "resizeMutableByteArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s,MutableByteArray# s #)
   {Resize (unpinned) mutable byte array to new specified size (in bytes).
    The returned 'MutableByteArray#' is either the original
    'MutableByteArray#' resized in-place or, if not possible, a newly
    allocated (unpinned) 'MutableByteArray#' (with the original content
    copied over).

    To avoid undefined behaviour, the original 'MutableByteArray#' shall
    not be accessed anymore after a 'resizeMutableByteArray#' has been
    performed.  Moreover, no reference to the old one should be kept in order
    to allow garbage collection of the original 'MutableByteArray#' in
    case a new 'MutableByteArray#' had to be allocated.}
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
   unsafe in the presence of shrink and resize operations on the same mutable byte
   array.}
   with deprecated_msg = { Use 'getSizeofMutableByteArray#' instead }

primop  GetSizeofMutableByteArrayOp "getSizeofMutableByteArray#" GenPrimOp
   MutableByteArray# s -> State# s -> (# State# s, Int# #)
   {Return the number of elements in the array.}

#include "bytearray-ops.txt.pp"

primop  CompareByteArraysOp "compareByteArrays#" GenPrimOp
   ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
   {@'compareByteArrays#' src1 src1_ofs src2 src2_ofs n@ compares
    @n@ bytes starting at offset @src1_ofs@ in the first
    'ByteArray#' @src1@ to the range of @n@ bytes
    (i.e. same length) starting at offset @src2_ofs@ of the second
    'ByteArray#' @src2@.  Both arrays must fully contain the
    specified ranges, but this is not checked.  Returns an 'Int#'
    less than, equal to, or greater than zero if the range is found,
    respectively, to be byte-wise lexicographically less than, to
    match, or be greater than the second range.}
   with
   can_fail = True

primop  CopyByteArrayOp "copyByteArray#" GenPrimOp
  ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {@'copyByteArray#' src src_ofs dst dst_ofs n@ copies the range
   starting at offset @src_ofs@ of length @n@ from the
   'ByteArray#' @src@ to the 'MutableByteArray#' @dst@
   starting at offset @dst_ofs@.  Both arrays must fully contain
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
  {@'setByteArray#' ba off len c@ sets the byte range @[off, off+len)@ of
   the 'MutableByteArray#' to the byte @c@.}
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

primop CasByteArrayOp_Int8 "casInt8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int8# -> Int8# -> State# s -> (# State# s, Int8# #)
   {Given an array, an offset in bytes, the expected old value, and
    the new value, perform an atomic compare and swap i.e. write the new
    value if the current value matches the provided old value. Returns
    the value of the element before the operation. Implies a full memory
    barrier.}
   with has_side_effects = True
        can_fail = True

primop CasByteArrayOp_Int16 "casInt16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int16# -> Int16# -> State# s -> (# State# s, Int16# #)
   {Given an array, an offset in 16 bit units, the expected old value, and
    the new value, perform an atomic compare and swap i.e. write the new
    value if the current value matches the provided old value. Returns
    the value of the element before the operation. Implies a full memory
    barrier.}
   with has_side_effects = True
        can_fail = True

primop CasByteArrayOp_Int32 "casInt32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int32# -> Int32# -> State# s -> (# State# s, Int32# #)
   {Given an array, an offset in 32 bit units, the expected old value, and
    the new value, perform an atomic compare and swap i.e. write the new
    value if the current value matches the provided old value. Returns
    the value of the element before the operation. Implies a full memory
    barrier.}
   with has_side_effects = True
        can_fail = True

primop CasByteArrayOp_Int64 "casInt64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int64# -> Int64# -> State# s -> (# State# s, Int64# #)
   {Given an array, an offset in 64 bit units, the expected old value, and
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
    atomically subtract the value from the element. Returns the value of
    the element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchAndByteArrayOp_Int "fetchAndIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to AND,
    atomically AND the value into the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchNandByteArrayOp_Int "fetchNandIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to NAND,
    atomically NAND the value into the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchOrByteArrayOp_Int "fetchOrIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to OR,
    atomically OR the value into the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchXorByteArrayOp_Int "fetchXorIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to XOR,
    atomically XOR the value into the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

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
         {Result is meaningless if two 'Addr#'s are so far apart that their
         difference doesn't fit in an 'Int#'.}
primop   AddrRemOp "remAddr#" GenPrimOp Addr# -> Int# -> Int#
         {Return the remainder when the 'Addr#' arg, treated like an 'Int#',
          is divided by the 'Int#' arg.}
primop   AddrToIntOp  "addr2Int#"     GenPrimOp   Addr# -> Int#
        {Coerce directly from address to int.}
   with code_size = 0
        deprecated_msg = { This operation is strongly deprecated. }
primop   IntToAddrOp   "int2Addr#"    GenPrimOp  Int# -> Addr#
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
   Addr# -> Int# -> Int8#
   with can_fail = True

primop IndexOffAddrOp_Int16 "indexInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int16#
   with can_fail = True

primop IndexOffAddrOp_Int32 "indexInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> Int32#
   with can_fail = True

primop IndexOffAddrOp_Int64 "indexInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> Int64#
   with can_fail = True

primop IndexOffAddrOp_Word8 "indexWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word8#
   with can_fail = True

primop IndexOffAddrOp_Word16 "indexWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word16#
   with can_fail = True

primop IndexOffAddrOp_Word32 "indexWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> Word32#
   with can_fail = True

primop IndexOffAddrOp_Word64 "indexWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> Word64#
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
   Addr# -> Int# -> State# s -> (# State# s, Int8# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int16 "readInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int16# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int32 "readInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int32# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int64 "readInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int64# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word8 "readWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word8# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word16 "readWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word16# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word32 "readWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word32# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word64 "readWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word64# #)
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
   Addr# -> Int# -> Int8# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int16 "writeInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int16# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int32 "writeInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> Int32# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int64 "writeInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> Int64# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word8 "writeWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word8# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word16 "writeWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word16# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word32 "writeWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> Word32# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word64 "writeWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> Word64# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  InterlockedExchange_Addr "atomicExchangeAddrAddr#" GenPrimOp
   Addr# -> Addr# -> State# s -> (# State# s, Addr# #)
   {The atomic exchange operation. Atomically exchanges the value at the first address
    with the Addr# given as second argument. Implies a read barrier.}
   with has_side_effects = True
        can_fail         = True

primop  InterlockedExchange_Word "atomicExchangeWordAddr#" GenPrimOp
   Addr# -> Word# -> State# s -> (# State# s, Word# #)
   {The atomic exchange operation. Atomically exchanges the value at the address
    with the given value. Returns the old value. Implies a read barrier.}
   with has_side_effects = True
        can_fail         = True

primop  CasAddrOp_Addr "atomicCasAddrAddr#" GenPrimOp
   Addr# -> Addr# -> Addr# -> State# s -> (# State# s, Addr# #)
   { Compare and swap on a word-sized memory location.

     Use as: \s -> atomicCasAddrAddr# location expected desired s

     This version always returns the old value read. This follows the normal
     protocol for CAS operations (and matches the underlying instruction on
     most architectures).

     Implies a full memory barrier.}
   with has_side_effects = True
        can_fail         = True

primop  CasAddrOp_Word "atomicCasWordAddr#" GenPrimOp
   Addr# -> Word# -> Word# -> State# s -> (# State# s, Word# #)
   { Compare and swap on a word-sized and aligned memory location.

     Use as: \s -> atomicCasWordAddr# location expected desired s

     This version always returns the old value read. This follows the normal
     protocol for CAS operations (and matches the underlying instruction on
     most architectures).

     Implies a full memory barrier.}
   with has_side_effects = True
        can_fail         = True

primop  CasAddrOp_Word8 "atomicCasWord8Addr#" GenPrimOp
   Addr# -> Word8# -> Word8# -> State# s -> (# State# s, Word8# #)
   { Compare and swap on a 8 bit-sized and aligned memory location.

     Use as: \s -> atomicCasWordAddr8# location expected desired s

     This version always returns the old value read. This follows the normal
     protocol for CAS operations (and matches the underlying instruction on
     most architectures).

     Implies a full memory barrier.}
   with has_side_effects = True
        can_fail         = True

primop  CasAddrOp_Word16 "atomicCasWord16Addr#" GenPrimOp
   Addr# -> Word16# -> Word16# -> State# s -> (# State# s, Word16# #)
   { Compare and swap on a 16 bit-sized and aligned memory location.

     Use as: \s -> atomicCasWordAddr16# location expected desired s

     This version always returns the old value read. This follows the normal
     protocol for CAS operations (and matches the underlying instruction on
     most architectures).

     Implies a full memory barrier.}
   with has_side_effects = True
        can_fail         = True

primop  CasAddrOp_Word32 "atomicCasWord32Addr#" GenPrimOp
   Addr# -> Word32# -> Word32# -> State# s -> (# State# s, Word32# #)
   { Compare and swap on a 32 bit-sized and aligned memory location.

     Use as: \s -> atomicCasWordAddr32# location expected desired s

     This version always returns the old value read. This follows the normal
     protocol for CAS operations (and matches the underlying instruction on
     most architectures).

     Implies a full memory barrier.}
   with has_side_effects = True
        can_fail         = True

primop  CasAddrOp_Word64 "atomicCasWord64Addr#" GenPrimOp
   Addr# -> Word64# -> Word64# -> State# s -> (# State# s, Word64# #)
   { Compare and swap on a 64 bit-sized and aligned memory location.

     Use as: \s -> atomicCasWordAddr64# location expected desired s

     This version always returns the old value read. This follows the normal
     protocol for CAS operations (and matches the underlying instruction on
     most architectures).

     Implies a full memory barrier.}
   with has_side_effects = True
        can_fail         = True

primop FetchAddAddrOp_Word "fetchAddWordAddr#" GenPrimOp
   Addr# -> Word# -> State# s -> (# State# s, Word# #)
   {Given an address, and a value to add,
    atomically add the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchSubAddrOp_Word "fetchSubWordAddr#" GenPrimOp
   Addr# -> Word# -> State# s -> (# State# s, Word# #)
   {Given an address, and a value to subtract,
    atomically subtract the value from the element. Returns the value of
    the element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchAndAddrOp_Word "fetchAndWordAddr#" GenPrimOp
   Addr# -> Word# -> State# s -> (# State# s, Word# #)
   {Given an address, and a value to AND,
    atomically AND the value into the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchNandAddrOp_Word "fetchNandWordAddr#" GenPrimOp
   Addr# -> Word# -> State# s -> (# State# s, Word# #)
   {Given an address, and a value to NAND,
    atomically NAND the value into the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchOrAddrOp_Word "fetchOrWordAddr#" GenPrimOp
   Addr# -> Word# -> State# s -> (# State# s, Word# #)
   {Given an address, and a value to OR,
    atomically OR the value into the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchXorAddrOp_Word "fetchXorWordAddr#" GenPrimOp
   Addr# -> Word# -> State# s -> (# State# s, Word# #)
   {Given an address, and a value to XOR,
    atomically XOR the value into the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop  AtomicReadAddrOp_Word "atomicReadWordAddr#" GenPrimOp
   Addr# -> State# s -> (# State# s, Word# #)
   {Given an address, read a machine word.  Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop  AtomicWriteAddrOp_Word "atomicWriteWordAddr#" GenPrimOp
   Addr# -> Word# -> State# s -> State# s
   {Given an address, write a machine word. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True


------------------------------------------------------------------------
section "Mutable variables"
        {Operations on MutVar\#s.}
------------------------------------------------------------------------

primtype MutVar# s a
        {A 'MutVar#' behaves like a single-element mutable array.}

primop  NewMutVarOp "newMutVar#" GenPrimOp
   v -> State# s -> (# State# s, MutVar# s v #)
   {Create 'MutVar#' with specified initial value in specified state thread.}
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
   MutVar# s v -> State# s -> (# State# s, v #)
   {Read contents of 'MutVar#'. Result is not yet evaluated.}
   with
   -- See Note [Why MutVar# ops can't fail]
   has_side_effects = True

primop  WriteMutVarOp "writeMutVar#"  GenPrimOp
   MutVar# s v -> v -> State# s -> State# s
   {Write contents of 'MutVar#'.}
   with
   -- See Note [Why MutVar# ops can't fail]
   has_side_effects = True
   code_size = { primOpCodeSizeForeignCall } -- for the write barrier

-- Note [Why not an unboxed tuple in atomicModifyMutVar2#?]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
   { Modify the contents of a 'MutVar#', returning the previous
     contents and the result of applying the given function to the
     previous contents. Note that this isn't strictly
     speaking the correct type for this function; it should really be
     @'MutVar#' s a -> (a -> (a,b)) -> 'State#' s -> (# 'State#' s, a, (a, b) #)@,
     but we don't know about pairs here. }
   with
   out_of_line = True
   has_side_effects = True
   can_fail         = True

primop  AtomicModifyMutVar_Op "atomicModifyMutVar_#" GenPrimOp
   MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)
   { Modify the contents of a 'MutVar#', returning the previous
     contents and the result of applying the given function to the
     previous contents. }
   with
   out_of_line = True
   has_side_effects = True
   can_fail         = True

primop  CasMutVarOp "casMutVar#" GenPrimOp
  MutVar# s v -> v -> v -> State# s -> (# State# s, Int#, v #)
   { Compare-and-swap: perform a pointer equality test between
     the first value passed to this function and the value
     stored inside the 'MutVar#'. If the pointers are equal,
     replace the stored value with the second value passed to this
     function, otherwise do nothing.
     Returns the final value stored inside the 'MutVar#'.
     The 'Int#' indicates whether a swap took place,
     with @1#@ meaning that we didn't swap, and @0#@
     that we did.
     Implies a full memory barrier.
     Because the comparison is done on the level of pointers,
     all of the difficulties of using
     'reallyUnsafePtrEquality#' correctly apply to
     'casMutVar#' as well.
   }
   with
   out_of_line = True
   has_side_effects = True

------------------------------------------------------------------------
section "Exceptions"
------------------------------------------------------------------------

-- Note [Strict IO wrappers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Consider this example, which comes from GHC.IO.Handle.Internals:
--    wantReadableHandle3 f mv b st
--      = case ... of
--          DEFAULT -> case mv of MVar a -> ...
--          0#      -> maskAsyncExceptions# (\st -> case mv of MVar a -> ...)
-- The outer case just decides whether to mask exceptions, but we don't want
-- thereby to hide the strictness in `mv`!  Hence the use of strictOnceApply1Dmd
-- in mask#, unmask# and atomically# (where we use strictManyApply1Dmd to respect
-- that it potentially calls its action multiple times).
--
-- Note [Strictness for catch-style primops]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The catch#-style primops always call their action, just like outlined
-- in Note [Strict IO wrappers].
-- However, it is important that we give their first arg lazyApply1Dmd and not
-- strictOnceApply1Dmd, like for mask#. Here is why. Consider a call
--
--   catch# act handler s
--
-- If `act = raiseIO# ...`, using strictOnceApply1Dmd for `act` would mean that
-- the call forwards the dead-end flag from `act` (see Note [Dead ends] and
-- Note [Precise exceptions and strictness analysis]).
-- This would cause dead code elimination to discard the continuation of the
-- catch# call, among other things. This first came up in #11555.
--
-- Hence catch# uses lazyApply1Dmd in order /not/ to forward the dead-end flag
-- from `act`. (This is a bit brutal, but the language of strictness types is
-- not expressive enough to give it a more precise semantics that is still
-- sound.)
-- For perf reasons we often (but not always) choose to use a wrapper around
-- catch# that is head-strict in `act`: GHC.IO.catchException.
--
-- A similar caveat applies to prompt#, which can be seen as a
-- generalisation of catch# as explained in GHC.Prim#continuations#.
-- The reason is that even if `act` appears dead-ending (e.g., looping)
-- `prompt# tag ma s` might return alright due to a (higher-order) use of
-- `control0#` in `act`. This came up in #25439.

primop  CatchOp "catch#" GenPrimOp
          (State# RealWorld -> (# State# RealWorld, o #) )
       -> (w -> State# RealWorld -> (# State# RealWorld, o #) )
       -> State# RealWorld
       -> (# State# RealWorld, o #)
   { @'catch#' k handler s@ evaluates @k s@, invoking @handler@ on any exceptions
     thrown.

     Note that the result type here isn't quite as unrestricted as the
     polymorphic type might suggest; see the section \"RuntimeRep polymorphism
     in continuation-style primops\" for details. }
   with
   strictness  = { \ _arity -> mkClosedDmdSig [ lazyApply1Dmd
                                                 , lazyApply2Dmd
                                                 , topDmd] topDiv }
                 -- See Note [Strictness for catch-style primops]
   out_of_line = True
   has_side_effects = True

primop  RaiseOp "raise#" GenPrimOp
   v -> p
      -- NB: "v" is the same as "a" except levity-polymorphic,
      -- and "p" is the same as "b" except representation-polymorphic
      -- See Note [Levity and representation polymorphic primops]
   with
   -- In contrast to 'raiseIO#', which throws a *precise* exception,
   -- exceptions thrown by 'raise#' are considered *imprecise*.
   -- See Note [Precise vs imprecise exceptions] in GHC.Types.Demand.
   -- Hence, it has 'botDiv', not 'exnDiv'.
   -- For the same reasons, 'raise#' is marked as "can_fail" (which 'raiseIO#'
   -- is not), but not as "has_side_effects" (which 'raiseIO#' is).
   -- See Note [PrimOp can_fail and has_side_effects] in "GHC.Builtin.PrimOps".
   strictness  = { \ _arity -> mkClosedDmdSig [topDmd] botDiv }
   out_of_line = True
   can_fail = True

primop  RaiseUnderflowOp "raiseUnderflow#" GenPrimOp
   (# #) -> p
   with
   strictness  = { \ _arity -> mkClosedDmdSig [topDmd] botDiv }
   out_of_line = True
   can_fail = True
   code_size = { primOpCodeSizeForeignCall }

primop  RaiseOverflowOp "raiseOverflow#" GenPrimOp
   (# #) -> p
   with
   strictness  = { \ _arity -> mkClosedDmdSig [topDmd] botDiv }
   out_of_line = True
   can_fail = True
   code_size = { primOpCodeSizeForeignCall }

primop  RaiseDivZeroOp "raiseDivZero#" GenPrimOp
   (# #) -> p
   with
   strictness  = { \ _arity -> mkClosedDmdSig [topDmd] botDiv }
   out_of_line = True
   can_fail = True
   code_size = { primOpCodeSizeForeignCall }

primop  RaiseIOOp "raiseIO#" GenPrimOp
   v -> State# RealWorld -> (# State# RealWorld, p #)
   with
   -- See Note [Precise exceptions and strictness analysis] in "GHC.Types.Demand"
   -- for why this is the *only* primop that has 'exnDiv'
   strictness  = { \ _arity -> mkClosedDmdSig [topDmd, topDmd] exnDiv }
   out_of_line = True
   has_side_effects = True

primop  MaskAsyncExceptionsOp "maskAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, o #))
     -> (State# RealWorld -> (# State# RealWorld, o #))
   { @'maskAsyncExceptions#' k s@ evaluates @k s@ such that asynchronous
     exceptions are deferred until after evaluation has finished.

     Note that the result type here isn't quite as unrestricted as the
     polymorphic type might suggest; see the section \"RuntimeRep polymorphism
     in continuation-style primops\" for details. }
   with
   strictness  = { \ _arity -> mkClosedDmdSig [strictOnceApply1Dmd,topDmd] topDiv }
                 -- See Note [Strict IO wrappers]
   out_of_line = True
   has_side_effects = True

primop  MaskUninterruptibleOp "maskUninterruptible#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, o #))
     -> (State# RealWorld -> (# State# RealWorld, o #))
   { @'maskUninterruptible#' k s@ evaluates @k s@ such that asynchronous
     exceptions are deferred until after evaluation has finished.

     Note that the result type here isn't quite as unrestricted as the
     polymorphic type might suggest; see the section \"RuntimeRep polymorphism
     in continuation-style primops\" for details. }
   with
   strictness  = { \ _arity -> mkClosedDmdSig [strictOnceApply1Dmd,topDmd] topDiv }
                 -- See Note [Strict IO wrappers]
   out_of_line = True
   has_side_effects = True

primop  UnmaskAsyncExceptionsOp "unmaskAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, o #))
     -> (State# RealWorld -> (# State# RealWorld, o #))
   { @'unmaskAsyncUninterruptible#' k s@ evaluates @k s@ such that asynchronous
     exceptions are unmasked.

     Note that the result type here isn't quite as unrestricted as the
     polymorphic type might suggest; see the section \"RuntimeRep polymorphism
     in continuation-style primops\" for details. }
   with
   strictness  = { \ _arity -> mkClosedDmdSig [strictOnceApply1Dmd,topDmd] topDiv }
                 -- See Note [Strict IO wrappers]
   out_of_line = True
   has_side_effects = True

primop  MaskStatus "getMaskingState#" GenPrimOp
        State# RealWorld -> (# State# RealWorld, Int# #)
   with
   out_of_line = True
   has_side_effects = True

------------------------------------------------------------------------
section "Continuations"
  { #continuations#

    These operations provide access to first-class delimited continuations,
    which allow a computation to access and manipulate portions of its
    /current continuation/. Operationally, they are implemented by direct
    manipulation of the RTS call stack, which may provide significant
    performance gains relative to manual continuation-passing style (CPS) for
    some programs.

    Intuitively, the delimited control operators 'prompt#' and
    'control0#' can be understood by analogy to 'catch#' and 'raiseIO#',
    respectively:

      * Like 'catch#', 'prompt#' does not do anything on its own, it
        just /delimits/ a subcomputation (the source of the name "delimited
        continuations").

      * Like 'raiseIO#', 'control0#' aborts to the nearest enclosing
        'prompt#' before resuming execution.

    However, /unlike/ 'raiseIO#', 'control0#' does /not/ discard
    the aborted computation: instead, it /captures/ it in a form that allows
    it to be resumed later. In other words, 'control0#' does not
    irreversibly abort the local computation before returning to the enclosing
    'prompt#', it merely suspends it. All local context of the suspended
    computation is packaged up and returned as an ordinary function that can be
    invoked at a later point in time to /continue/ execution, which is why
    the suspended computation is known as a /first-class continuation/.

    In GHC, every continuation prompt is associated with exactly one
    'PromptTag#'. Prompt tags are unique, opaque values created by
    'newPromptTag#' that may only be compared for equality. Both 'prompt#'
    and 'control0#' accept a 'PromptTag#' argument, and 'control0#'
    captures the continuation up to the nearest enclosing use of 'prompt#'
    /with the same tag/. This allows a program to control exactly which
    prompt it will abort to by using different tags, similar to how a program
    can control which 'catch' it will abort to by throwing different types
    of exceptions. Additionally, 'PromptTag#' accepts a single type parameter,
    which is used to relate the expected result type at the point of the
    'prompt#' to the type of the continuation produced by 'control0#'.

    == The gory details

    The high-level explanation provided above should hopefully provide some
    intuition for what these operations do, but it is not very precise; this
    section provides a more thorough explanation.

    The 'prompt#' operation morally has the following type:

@
'prompt#' :: 'PromptTag#' a -> IO a -> IO a
@

    If a computation @/m/@ never calls 'control0#', then
    @'prompt#' /tag/ /m/@ is equivalent to just @/m/@, i.e. the 'prompt#' is
    a no-op. This implies the following law:

    \[
    \mathtt{prompt\#}\ \mathit{tag}\ (\mathtt{pure}\ x) \equiv \mathtt{pure}\ x
    \]

    The 'control0#' operation morally has the following type:

@
'control0#' :: 'PromptTag#' a -> ((IO b -> IO a) -> IO a) -> IO b
@

    @'control0#' /tag/ /f/@ captures the current continuation up to the nearest
    enclosing @'prompt#' /tag/@ and resumes execution from the point of the call
    to 'prompt#', passing the captured continuation to @/f/@. To make that
    somewhat more precise, we can say 'control0#' obeys the following law:

    \[
    \mathtt{prompt\#}\ \mathit{tag}\ (\mathtt{control0\#}\ tag\ f \mathbin{\mathtt{>>=}} k)
      \equiv f\ (\lambda\ m \rightarrow m \mathbin{\mathtt{>>=}} k)
    \]

    However, this law does not fully describe the behavior of 'control0#',
    as it does not account for situations where 'control0#' does not appear
    immediately inside 'prompt#'. Capturing the semantics more precisely
    requires some additional notational machinery; a common approach is to
    use [reduction semantics](https://en.wikipedia.org/wiki/Operational_semantics#Reduction_semantics).
    Assuming an appropriate definition of evaluation contexts \(E\), the
    semantics of 'prompt#' and 'control0#' can be given as follows:

    \[
    \begin{aligned}
    E[\mathtt{prompt\#}\ \mathit{tag}\ (\mathtt{pure}\ v)]
      &\longrightarrow E[\mathtt{pure}\ v] \\[8pt]
    E_1[\mathtt{prompt\#}\ \mathit{tag}\ E_2[\mathtt{control0\#}\ tag\ f]]
      &\longrightarrow E_1[f\ (\lambda\ m \rightarrow E_2[m])] \\[-2pt]
      \mathrm{where}\;\: \mathtt{prompt\#}\ \mathit{tag} &\not\in E_2
    \end{aligned}
    \]

    A full treatment of the semantics and metatheory of delimited control is
    well outside the scope of this documentation, but a good, thorough
    overview (in Haskell) is provided in [A Monadic Framework for Delimited
    Continuations](https://legacy.cs.indiana.edu/~dyb/pubs/monadicDC.pdf) by
    Dybvig et al.

    == Safety and invariants

    Correct uses of 'control0#' must obey the following restrictions:

    1. The behavior of 'control0#' is only well-defined within a /strict
       'State#' thread/, such as those associated with @IO@ and strict @ST@
       computations.

    2. Furthermore, 'control0#' may only be called within the dynamic extent
       of a 'prompt#' with a matching tag somewhere in the /current/ strict
       'State#' thread. Effectively, this means that a matching prompt must
       exist somewhere, and the captured continuation must /not/ contain any
       uses of @unsafePerformIO@, @runST@, @unsafeInterleaveIO@, etc. For
       example, the following program is ill-defined:

        @
        'prompt#' /tag/ $
          evaluate (unsafePerformIO $ 'control0#' /tag/ /f/)
        @

        In this example, the use of 'prompt#' appears in a different 'State#'
        thread from the use of 'control0#', so there is no valid prompt in
        scope to capture up to.

    3. Finally, 'control0#' may not be used within 'State#' threads associated
       with an STM transaction (i.e. those introduced by 'atomically#').

    If the runtime is able to detect that any of these invariants have been
    violated in a way that would compromise internal invariants of the runtime,
    'control0#' will fail by raising an exception. However, such violations
    are only detected on a best-effort basis, as the bookkeeping necessary for
    detecting /all/ illegal uses of 'control0#' would have significant overhead.
    Therefore, although the operations are “safe” from the runtime’s point of
    view (e.g. they will not compromise memory safety or clobber internal runtime
    state), it is still ultimately the programmer’s responsibility to ensure
    these invariants hold to guarantee predictable program behavior.

    In a similar vein, since each captured continuation includes the full local
    context of the suspended computation, it can safely be resumed arbitrarily
    many times without violating any invariants of the runtime system. However,
    use of these operations in an arbitrary 'IO' computation may be unsafe for
    other reasons, as most 'IO' code is not written with reentrancy in mind. For
    example, a computation suspended in the middle of reading a file will likely
    finish reading it when it is resumed; further attempts to resume from the
    same place would then fail because the file handle was already closed.

    In other words, although the RTS ensures that a computation’s control state
    and local variables are properly restored for each distinct resumption of
    a continuation, it makes no attempt to duplicate any local state the
    computation may have been using (and could not possibly do so in general).
    Furthermore, it provides no mechanism for an arbitrary computation to
    protect itself against unwanted reentrancy (i.e. there is no analogue to
    Scheme’s @dynamic-wind@). For those reasons, manipulating the continuation
    is only safe if the caller can be certain that doing so will not violate any
    expectations or invariants of the enclosing computation. }
------------------------------------------------------------------------

primtype PromptTag# a
   { See "GHC.Prim#continuations". }

primop  NewPromptTagOp "newPromptTag#" GenPrimOp
        State# RealWorld -> (# State# RealWorld, PromptTag# a #)
   { See "GHC.Prim#continuations". }
   with
   out_of_line = True
   has_side_effects = True

primop  PromptOp "prompt#" GenPrimOp
        PromptTag# a
     -> (State# RealWorld -> (# State# RealWorld, a #))
     -> State# RealWorld -> (# State# RealWorld, a #)
   { See "GHC.Prim#continuations". }
   with
   strictness = { \ _arity -> mkClosedDmdSig [topDmd, lazyApply1Dmd, topDmd] topDiv }
                 -- See Note [Strictness for catch-style primops]
   out_of_line = True
   has_side_effects = True

primop  Control0Op "control0#" GenPrimOp
        PromptTag# a
     -> (((State# RealWorld -> (# State# RealWorld, p #))
          -> State# RealWorld -> (# State# RealWorld, a #))
         -> State# RealWorld -> (# State# RealWorld, a #))
     -> State# RealWorld -> (# State# RealWorld, p #)
   { See "GHC.Prim#continuations". }
   with
   strictness = { \ _arity -> mkClosedDmdSig [topDmd, lazyApply2Dmd, topDmd] topDiv }
   out_of_line = True
   has_side_effects = True

------------------------------------------------------------------------
section "STM-accessible Mutable Variables"
------------------------------------------------------------------------

primtype TVar# s a

primop  AtomicallyOp "atomically#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, v #) )
   -> State# RealWorld -> (# State# RealWorld, v #)
   with
   strictness  = { \ _arity -> mkClosedDmdSig [strictManyApply1Dmd,topDmd] topDiv }
                 -- See Note [Strict IO wrappers]
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
   State# RealWorld -> (# State# RealWorld, v #)
   with
   strictness  = { \ _arity -> mkClosedDmdSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

primop  CatchRetryOp "catchRetry#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, v #) )
   -> (State# RealWorld -> (# State# RealWorld, v #) )
   -> (State# RealWorld -> (# State# RealWorld, v #) )
   with
   strictness  = { \ _arity -> mkClosedDmdSig [ lazyApply1Dmd
                                                 , lazyApply1Dmd
                                                 , topDmd ] topDiv }
                 -- See Note [Strictness for catch-style primops]
   out_of_line = True
   has_side_effects = True

primop  CatchSTMOp "catchSTM#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, v #) )
   -> (b -> State# RealWorld -> (# State# RealWorld, v #) )
   -> (State# RealWorld -> (# State# RealWorld, v #) )
   with
   strictness  = { \ _arity -> mkClosedDmdSig [ lazyApply1Dmd
                                                 , lazyApply2Dmd
                                                 , topDmd ] topDiv }
                 -- See Note [Strictness for catch-style primops]
   out_of_line = True
   has_side_effects = True

primop  NewTVarOp "newTVar#" GenPrimOp
       v
    -> State# s -> (# State# s, TVar# s v #)
   {Create a new 'TVar#' holding a specified initial value.}
   with
   out_of_line  = True
   has_side_effects = True

primop  ReadTVarOp "readTVar#" GenPrimOp
       TVar# s v
    -> State# s -> (# State# s, v #)
   {Read contents of 'TVar#' inside an STM transaction,
    i.e. within a call to 'atomically#'.
    Does not force evaluation of the result.}
   with
   out_of_line  = True
   has_side_effects = True

primop ReadTVarIOOp "readTVarIO#" GenPrimOp
       TVar# s v
    -> State# s -> (# State# s, v #)
   {Read contents of 'TVar#' outside an STM transaction.
   Does not force evaluation of the result.}
   with
   out_of_line      = True
   has_side_effects = True

primop  WriteTVarOp "writeTVar#" GenPrimOp
       TVar# s v
    -> v
    -> State# s -> State# s
   {Write contents of 'TVar#'.}
   with
   out_of_line      = True
   has_side_effects = True


------------------------------------------------------------------------
section "Synchronized Mutable Variables"
        {Operations on 'MVar#'s. }
------------------------------------------------------------------------

primtype MVar# s a
        { A shared mutable variable (/not/ the same as a 'MutVar#'!).
        (Note: in a non-concurrent implementation, @('MVar#' a)@ can be
        represented by @('MutVar#' (Maybe a))@.) }

primop  NewMVarOp "newMVar#"  GenPrimOp
   State# s -> (# State# s, MVar# s v #)
   {Create new 'MVar#'; initially empty.}
   with
   out_of_line = True
   has_side_effects = True

primop  TakeMVarOp "takeMVar#" GenPrimOp
   MVar# s v -> State# s -> (# State# s, v #)
   {If 'MVar#' is empty, block until it becomes full.
   Then remove and return its contents, and set it empty.}
   with
   out_of_line      = True
   has_side_effects = True

primop  TryTakeMVarOp "tryTakeMVar#" GenPrimOp
   MVar# s v -> State# s -> (# State# s, Int#, v #)
   {If 'MVar#' is empty, immediately return with integer 0 and value undefined.
   Otherwise, return with integer 1 and contents of 'MVar#', and set 'MVar#' empty.}
   with
   out_of_line      = True
   has_side_effects = True

primop  PutMVarOp "putMVar#" GenPrimOp
   MVar# s v -> v -> State# s -> State# s
   {If 'MVar#' is full, block until it becomes empty.
   Then store value arg as its new contents.}
   with
   out_of_line      = True
   has_side_effects = True

primop  TryPutMVarOp "tryPutMVar#" GenPrimOp
   MVar# s v -> v -> State# s -> (# State# s, Int# #)
   {If 'MVar#' is full, immediately return with integer 0.
    Otherwise, store value arg as 'MVar#''s new contents, and return with integer 1.}
   with
   out_of_line      = True
   has_side_effects = True

primop  ReadMVarOp "readMVar#" GenPrimOp
   MVar# s v -> State# s -> (# State# s, v #)
   {If 'MVar#' is empty, block until it becomes full.
   Then read its contents without modifying the MVar, without possibility
   of intervention from other threads.}
   with
   out_of_line      = True
   has_side_effects = True

primop  TryReadMVarOp "tryReadMVar#" GenPrimOp
   MVar# s v -> State# s -> (# State# s, Int#, v #)
   {If 'MVar#' is empty, immediately return with integer 0 and value undefined.
   Otherwise, return with integer 1 and contents of 'MVar#'.}
   with
   out_of_line      = True
   has_side_effects = True

primop  IsEmptyMVarOp "isEmptyMVar#" GenPrimOp
   MVar# s v -> State# s -> (# State# s, Int# #)
   {Return 1 if 'MVar#' is empty; 0 otherwise.}
   with
   out_of_line = True
   has_side_effects = True


------------------------------------------------------------------------
section "Synchronized I/O Ports"
        {Operations on 'IOPort#'s. }
------------------------------------------------------------------------

primtype IOPort# s a
        { A shared I/O port is almost the same as an 'MVar#'.
        The main difference is that IOPort has no deadlock detection or
        deadlock breaking code that forcibly releases the lock. }

primop  NewIOPortOp "newIOPort#"  GenPrimOp
   State# s -> (# State# s, IOPort# s v #)
   {Create new 'IOPort#'; initially empty.}
   with
   out_of_line = True
   has_side_effects = True

primop  ReadIOPortOp "readIOPort#" GenPrimOp
   IOPort# s v -> State# s -> (# State# s, v #)
   {If 'IOPort#' is empty, block until it becomes full.
   Then remove and return its contents, and set it empty.
   Throws an 'IOPortException' if another thread is already
   waiting to read this 'IOPort#'.}
   with
   out_of_line      = True
   has_side_effects = True

primop  WriteIOPortOp "writeIOPort#" GenPrimOp
   IOPort# s v -> v -> State# s -> (# State# s, Int# #)
   {If 'IOPort#' is full, immediately return with integer 0,
    throwing an 'IOPortException'.
    Otherwise, store value arg as 'IOPort#''s new contents,
    and return with integer 1. }
   with
   out_of_line      = True
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
        { 'State#' is the primitive, unlifted type of states.  It has
        one type parameter, thus @'State#' 'RealWorld'@, or @'State#' s@,
        where s is a type variable. The only purpose of the type parameter
        is to keep different state threads separate.  It is represented by
        nothing at all. }

primtype RealWorld
        { 'RealWorld' is deeply magical.  It is /primitive/, but it is not
        /unlifted/ (hence @ptrArg@).  We never manipulate values of type
        'RealWorld'; it's only used in the type system, to parameterise 'State#'. }

primtype ThreadId#
        {(In a non-concurrent implementation, this can be a singleton
        type, whose (unique) value is returned by 'myThreadId#'.  The
        other operations can be omitted.)}

primop  ForkOp "fork#" GenPrimOp
   (State# RealWorld -> (# State# RealWorld, o #))
   -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
   with
   has_side_effects = True
   out_of_line      = True
   strictness  = { \ _arity -> mkClosedDmdSig [ lazyApply1Dmd
                                              , topDmd ] topDiv }

primop  ForkOnOp "forkOn#" GenPrimOp
   Int# -> (State# RealWorld -> (# State# RealWorld, o #))
   -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
   with
   has_side_effects = True
   out_of_line      = True
   strictness  = { \ _arity -> mkClosedDmdSig [ topDmd
                                              , lazyApply1Dmd
                                              , topDmd ] topDiv }

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
   ThreadId# -> ByteArray# -> State# RealWorld -> State# RealWorld
   {Set the label of the given thread. The @ByteArray#@ should contain
    a UTF-8-encoded string.}
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

primop GetThreadLabelOp "threadLabel#" GenPrimOp
   ThreadId# -> State# RealWorld -> (# State# RealWorld, Int#, ByteArray# #)
   {Get the label of the given thread.
    Morally of type @ThreadId# -> IO (Maybe ByteArray#)@, with a @1#@ tag
    denoting @Just@.

    @since 0.10}
   with
   out_of_line      = True

primop  ThreadStatusOp "threadStatus#" GenPrimOp
   ThreadId# -> State# RealWorld -> (# State# RealWorld, Int#, Int#, Int# #)
   {Get the status of the given thread. Result is
    @(ThreadStatus, Capability, Locked)@ where
    @ThreadStatus@ is one of the status constants defined in
    @rts/Constants.h@, @Capability@ is the number of
    the capability which currently owns the thread, and
    @Locked@ is a boolean indicating whether the
    thread is bound to that capability.

    @since 0.9}
   with
   out_of_line = True
   has_side_effects = True

primop ListThreadsOp "listThreads#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, Array# ThreadId# #)
   { Returns an array of the threads started by the program. Note that this
     threads which have finished execution may or may not be present in this
     list, depending upon whether they have been collected by the garbage collector.

     @since 0.10}
   with
   out_of_line = True
   has_side_effects = True

------------------------------------------------------------------------
section "Weak pointers"
------------------------------------------------------------------------

primtype Weak# b

-- N.B. "v" and "w" denote levity-polymorphic type variables.
-- See Note [Levity and representation polymorphic primops]

primop  MkWeakOp "mkWeak#" GenPrimOp
   v -> w -> (State# RealWorld -> (# State# RealWorld, c #))
     -> State# RealWorld -> (# State# RealWorld, Weak# w #)
   { @'mkWeak#' k v finalizer s@ creates a weak reference to value @k@,
     with an associated reference to some value @v@. If @k@ is still
     alive then @v@ can be retrieved using 'deRefWeak#'. Note that
     the type of @k@ must be represented by a pointer (i.e. of kind
     @'TYPE' ''LiftedRep' or @'TYPE' ''UnliftedRep'@). }
   with
   has_side_effects = True
   out_of_line      = True

primop  MkWeakNoFinalizerOp "mkWeakNoFinalizer#" GenPrimOp
   v -> w -> State# RealWorld -> (# State# RealWorld, Weak# w #)
   with
   has_side_effects = True
   out_of_line      = True

primop  AddCFinalizerToWeakOp "addCFinalizerToWeak#" GenPrimOp
   Addr# -> Addr# -> Int# -> Addr# -> Weak# w
          -> State# RealWorld -> (# State# RealWorld, Int# #)
   { @'addCFinalizerToWeak#' fptr ptr flag eptr w@ attaches a C
     function pointer @fptr@ to a weak pointer @w@ as a finalizer. If
     @flag@ is zero, @fptr@ will be called with one argument,
     @ptr@. Otherwise, it will be called with two arguments,
     @eptr@ and @ptr@. 'addCFinalizerToWeak#' returns
     1 on success, or 0 if @w@ is already dead. }
   with
   has_side_effects = True
   out_of_line      = True

primop  DeRefWeakOp "deRefWeak#" GenPrimOp
   Weak# v -> State# RealWorld -> (# State# RealWorld, Int#, v #)
   with
   has_side_effects = True
   out_of_line      = True

primop  FinalizeWeakOp "finalizeWeak#" GenPrimOp
   Weak# v -> State# RealWorld -> (# State# RealWorld, Int#,
              (State# RealWorld -> (# State# RealWorld, b #) ) #)
   { Finalize a weak pointer. The return value is an unboxed tuple
     containing the new state of the world and an "unboxed Maybe",
     represented by an 'Int#' and a (possibly invalid) finalization
     action. An 'Int#' of @1@ indicates that the finalizer is valid. The
     return value @b@ from the finalizer should be ignored. }
   with
   has_side_effects = True
   out_of_line      = True

primop TouchOp "touch#" GenPrimOp
   v -> State# RealWorld -> State# RealWorld
   with
   code_size = { 0 }
   has_side_effects = True

------------------------------------------------------------------------
section "Stable pointers and names"
------------------------------------------------------------------------

primtype StablePtr# a

primtype StableName# a

primop  MakeStablePtrOp "makeStablePtr#" GenPrimOp
   v -> State# RealWorld -> (# State# RealWorld, StablePtr# v #)
   with
   has_side_effects = True
   out_of_line      = True

primop  DeRefStablePtrOp "deRefStablePtr#" GenPrimOp
   StablePtr# v -> State# RealWorld -> (# State# RealWorld, v #)
   with
   has_side_effects = True
   out_of_line      = True

primop  EqStablePtrOp "eqStablePtr#" GenPrimOp
   StablePtr# v -> StablePtr# v -> Int#
   with
   has_side_effects = True

primop  MakeStableNameOp "makeStableName#" GenPrimOp
   v -> State# RealWorld -> (# State# RealWorld, StableName# v #)
   with
   has_side_effects = True
   out_of_line      = True

primop  StableNameToIntOp "stableNameToInt#" GenPrimOp
   StableName# v -> Int#

------------------------------------------------------------------------
section "Compact normal form"

        {Primitives for working with compact regions. The @ghc-compact@
         library and the @compact@ library demonstrate how to use these
         primitives. The documentation below draws a distinction between
         a CNF and a compact block. A CNF contains one or more compact
         blocks. The source file @rts\/sm\/CNF.c@
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
     next compact block and its utilized size, or 'nullAddr#' if the
     argument was the last compact block in the CNF. }
   with
   out_of_line      = True

primop  CompactAllocateBlockOp "compactAllocateBlock#" GenPrimOp
   Word# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)
   { Attempt to allocate a compact block with the capacity (in
     bytes) given by the first argument. The 'Addr#' is a pointer
     to previous compact block of the CNF or 'nullAddr#' to create a
     new CNF with a single compact block.

     The resulting block is not known to the GC until
     'compactFixupPointers#' is called on it, and care must be taken
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
     'Compact#' (a CNF), evaluating any unevaluated components
     at the same time. Note: 'compactAdd#' is not thread-safe, so
     only one thread may call 'compactAdd#' with a particular
     'Compact#' at any given time. The primop does not
     enforce any mutual exclusion; the caller is expected to
     arrange this. }
   with
   has_side_effects = True
   out_of_line      = True

primop CompactAddWithSharing "compactAddWithSharing#" GenPrimOp
   Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)
   { Like 'compactAdd#', but retains sharing and cycles
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

-- `v` and `w` are levity-polymorphic type variables with independent levities.
-- See Note [Levity and representation polymorphic primops]
primop  ReallyUnsafePtrEqualityOp "reallyUnsafePtrEquality#" GenPrimOp
   v -> w -> Int#
   { Returns @1#@ if the given pointers are equal and @0#@ otherwise. }
   with
   can_fail   = True -- See Note [reallyUnsafePtrEquality# can_fail]

-- Note [Pointer comparison operations]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The primop `reallyUnsafePtrEquality#` does a direct pointer
-- equality between two (boxed) values.  Several things to note:
--
-- (PE1) It is levity-polymorphic. It works for TYPE (BoxedRep Lifted) and
--       TYPE (BoxedRep Unlifted). But not TYPE IntRep, for example.
--       This levity-polymorphism comes from the use of the type variables
--       "v" and "w". See Note [Levity and representation polymorphic primops]
--
-- (PE2) It is hetero-typed; you can compare pointers of different types.
--       This is used in various packages such as containers & unordered-containers.
--
-- (PE3) It does not evaluate its arguments. The user of the primop is responsible
--       for doing so.  Consider
--            let { x = p+q; y = q+p } in reallyUnsafePtrEquality# x y
--       Here `x` and `y` point to different closures, so the expression will
--       probably return False; but if `x` and/or `y` were evaluated for some
--       other reason, then it might return True.
--
-- (PE4) It is obviously very dangerous, because replacing equals with equals
--       in the program can change the result.  For example
--           let x = f y in reallyUnsafePtrEquality# x x
--       will probably return True, whereas
--            reallyUnsafePtrEquality# (f y) (f y)
--       will probably return False. ("probably", because it's affected
--       by CSE and inlining).
--
-- (PE5) reallyUnsafePtrEquality# can't fail, but it is marked as such
--       to prevent it from floating out.
--       See Note [reallyUnsafePtrEquality# can_fail]
--
-- The library GHC.Prim.PtrEq (and GHC.Exts) provides
--
--   unsafePtrEquality# ::
--     forall (a :: UnliftedType) (b :: UnliftedType). a -> b -> Int#
--
-- It is still heterotyped (like (PE2)), but it's restricted to unlifted types
-- (unlike (PE1)).  That means that (PE3) doesn't apply: unlifted types are
-- always evaluated, which makes it a bit less unsafe.
--
-- However unsafePtrEquality# is /implemented/ by a call to
-- reallyUnsafePtrEquality#, so using the former is really just a documentation
-- hint to the reader of the code.  GHC behaves no differently.
--
-- The same library provides less Wild-West functions
-- for use in specific cases, namely:
--
--   reallyUnsafePtrEquality :: a -> a -> Int#  -- not levity-polymorphic, nor hetero-typed
--   sameArray# :: Array# a -> Array# a -> Int#
--   sameMutableArray# :: MutableArray# s a -> MutableArray# s a -> Int#
--   sameSmallArray# :: SmallArray# a -> SmallArray# a -> Int#
--   sameSmallMutableArray# :: SmallMutableArray# s a -> SmallMutableArray# s a -> Int#
--   sameByteArray# :: ByteArray# -> ByteArray# -> Int#
--   sameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Int#
--   sameArrayArray# :: ArrayArray# -> ArrayArray# -> Int#
--   sameMutableArrayArray# :: MutableArrayArray# s -> MutableArrayArray# s -> Int#
--   sameMutVar# :: MutVar# s a -> MutVar# s a -> Int#
--   sameTVar# :: TVar# s a -> TVar# s a -> Int#
--   sameMVar# :: MVar# s a -> MVar# s a -> Int#
--   sameIOPort# :: IOPort# s a -> IOPort# s a -> Int#
--   eqStableName# :: StableName# a -> StableName# b -> Int#
--
-- These operations are all specialisations of unsafePtrEquality#.

-- Note [reallyUnsafePtrEquality# can_fail]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- reallyUnsafePtrEquality# can't actually fail, per se, but we mark it
-- can_fail anyway. Until 5a9a1738023a, GHC considered primops okay for
-- speculation only when their arguments were known to be forced. This was
-- unnecessarily conservative, but it prevented reallyUnsafePtrEquality# from
-- floating out of places where its arguments were known to be forced.
-- Unfortunately, GHC could sometimes lose track of whether those arguments
-- were forced, leading to let-can-float invariant failures (see #13027 and the
-- discussion in #11444). Now that ok_for_speculation skips over lifted
-- arguments, we need to explicitly prevent reallyUnsafePtrEquality#
-- from floating out. Imagine if we had
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
section "Controlling object lifetime"
        {Ensuring that objects don't die a premature death.}
------------------------------------------------------------------------

-- See Note [keepAlive# magic] in GHC.CoreToStg.Prep.
-- NB: "v" is the same as "a" except levity-polymorphic,
-- and "p" is the same as "b" except representation-polymorphic.
-- See Note [Levity and representation polymorphic primops]
primop KeepAliveOp "keepAlive#" GenPrimOp
   v -> State# RealWorld -> (State# RealWorld -> p) -> p
   { @'keepAlive#' x s k@ keeps the value @x@ alive during the execution
     of the computation @k@.

     Note that the result type here isn't quite as unrestricted as the
     polymorphic type might suggest; see the section \"RuntimeRep polymorphism
     in continuation-style primops\" for details. }
   with
   out_of_line = True
   strictness = { \ _arity -> mkClosedDmdSig [topDmd, topDmd, strictOnceApply1Dmd] topDiv }
                 -- See Note [Strict IO wrappers]


------------------------------------------------------------------------
section "Tag to enum stuff"
        {Convert back and forth between values of enumerated types
        and small integers.}
------------------------------------------------------------------------

primop  DataToTagOp "dataToTag#" GenPrimOp
   a -> Int#  -- Zero-indexed; the first constructor has tag zero
   { Evaluates the argument and returns the tag of the result.
     Tags are Zero-indexed; the first constructor has tag zero. }
   with
   strictness = { \ _arity -> mkClosedDmdSig [evalDmd] topDiv }
   -- See Note [dataToTag# magic] in GHC.Core.Opt.ConstantFold

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
   Addr# -> (# v #)
   { Convert an 'Addr#' to a followable Any type. }
   with
   code_size = 0

primop   AnyToAddrOp "anyToAddr#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, Addr# #)
   { Retrieve the address of any Haskell value. This is
     essentially an 'unsafeCoerce#', but if implemented as such
     the core lint pass complains and fails to compile.
     As a primop, it is opaque to core/stg, and only appears
     in cmm (where the copy propagation pass will get rid of it).
     Note that "a" must be a value, not a thunk! It's too late
     for strictness analysis to enforce this, so you're on your
     own to guarantee this. Also note that 'Addr#' is not a GC
     pointer - up to you to guarantee that it does not become
     a dangling pointer immediately after you get it.}
   with
   code_size = 0

primop   MkApUpd0_Op "mkApUpd0#" GenPrimOp
   BCO -> (# a #)
   { Wrap a BCO in a @AP_UPD@ thunk which will be updated with the value of
     the BCO when evaluated. }
   with
   out_of_line = True

primop  NewBCOOp "newBCO#" GenPrimOp
   ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> State# s -> (# State# s, BCO #)
   { @'newBCO#' instrs lits ptrs arity bitmap@ creates a new bytecode object. The
     resulting object encodes a function of the given arity with the instructions
     encoded in @instrs@, and a static reference table usage bitmap given by
     @bitmap@. }
   with
   has_side_effects = True
   out_of_line      = True

primop  UnpackClosureOp "unpackClosure#" GenPrimOp
   a -> (# Addr#, ByteArray#, Array# b #)
   { @'unpackClosure#' closure@ copies the closure and pointers in the
     payload of the given closure into two new arrays, and returns a pointer to
     the first word of the closure's info table, a non-pointer array for the raw
     bytes of the closure, and a pointer array for the pointers in the payload. }
   with
   out_of_line = True

primop  ClosureSizeOp "closureSize#" GenPrimOp
   a -> Int#
   { @'closureSize#' closure@ returns the size of the given closure in
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
   { Returns the current 'CostCentreStack' (value is @NULL@ if
     not profiling).  Takes a dummy argument which can be used to
     avoid the call to 'getCurrentCCS#' being floated out by the
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
section "Info Table Origin"
------------------------------------------------------------------------
primop WhereFromOp "whereFrom#" GenPrimOp
   a -> State# s -> (# State# s, Addr# #)
   { Returns the @InfoProvEnt @ for the info table of the given object
     (value is @NULL@ if the table does not exist or there is no information
     about the closure).}
   with
   out_of_line = True

------------------------------------------------------------------------
section "Etc"
        {Miscellaneous built-ins}
------------------------------------------------------------------------

primtype FUN m a b
  {The builtin function type, written in infix form as @a % m -> b@.
   Values of this type are functions taking inputs of type @a@ and
   producing outputs of type @b@. The multiplicity of the input is
   @m@.

   Note that @'FUN' m a b@ permits representation polymorphism in both
   @a@ and @b@, so that types like @'Int#' -> 'Int#'@ can still be
   well-kinded.
  }

pseudoop "realWorld#"
   State# RealWorld
   { The token used in the implementation of the IO monad as a state monad.
     It does not pass any information at runtime.
     See also 'GHC.Magic.runRW#'. }

pseudoop "void#"
   (# #)
   { This is an alias for the unboxed unit tuple constructor.
     In earlier versions of GHC, 'void#' was a value
     of the primitive type 'Void#', which is now defined to be @(# #)@.
   }
   with deprecated_msg = { Use an unboxed unit tuple instead }

primtype Proxy# a
   { The type constructor 'Proxy#' is used to bear witness to some
   type variable. It's used when you want to pass around proxy values
   for doing things like modelling type applications. A 'Proxy#'
   is not only unboxed, it also has a polymorphic kind, and has no
   runtime representation, being totally free. }

pseudoop "proxy#"
   Proxy# a
   { Witness for an unboxed 'Proxy#' value, which has no runtime
   representation. }

pseudoop   "seq"
   a -> b -> b
   { The value of @'seq' a b@ is bottom if @a@ is bottom, and
     otherwise equal to @b@. In other words, it evaluates the first
     argument @a@ to weak head normal form (WHNF). 'seq' is usually
     introduced to improve performance by avoiding unneeded laziness.

     A note on evaluation order: the expression @'seq' a b@ does
     /not/ guarantee that @a@ will be evaluated before @b@.
     The only guarantee given by 'seq' is that the both @a@
     and @b@ will be evaluated before 'seq' returns a value.
     In particular, this means that @b@ may be evaluated before
     @a@. If you need to guarantee a specific order of evaluation,
     you must use the function 'pseq' from the "parallel" package. }
   with fixity = infixr 0
         -- This fixity is only the one picked up by Haddock. If you
         -- change this, do update 'ghcPrimIface' in 'GHC.Iface.Load'.

pseudoop   "unsafeCoerce#"
   a -> b
   { The function 'unsafeCoerce#' allows you to side-step the typechecker entirely. That
        is, it allows you to coerce any type into any other type. If you use this function,
        you had better get it right, otherwise segmentation faults await. It is generally
        used when you want to write a program that you know is well-typed, but where Haskell's
        type system is not expressive enough to prove that it is well typed.

        The following uses of 'unsafeCoerce#' are supposed to work (i.e. not lead to
        spurious compile-time or run-time crashes):

         * Casting any lifted type to 'Any'

         * Casting 'Any' back to the real type

         * Casting an unboxed type to another unboxed type of the same size.
           (Casting between floating-point and integral types does not work.
           See the "GHC.Float" module for functions to do work.)

         * Casting between two types that have the same runtime representation.  One case is when
           the two types differ only in "phantom" type parameters, for example
           @'Ptr' 'Int'@ to @'Ptr' 'Float'@, or @['Int']@ to @['Float']@ when the list is
           known to be empty.  Also, a @newtype@ of a type @T@ has the same representation
           at runtime as @T@.

        Other uses of 'unsafeCoerce#' are undefined.  In particular, you should not use
        'unsafeCoerce#' to cast a T to an algebraic data type D, unless T is also
        an algebraic data type.  For example, do not cast @'Int'->'Int'@ to 'Bool', even if
        you later cast that 'Bool' back to @'Int'->'Int'@ before applying it.  The reasons
        have to do with GHC's internal representation details (for the cognoscenti, data values
        can be entered but function closures cannot).  If you want a safe type to cast things
        to, use 'Any', which is not an algebraic data type.

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
     argument.  The event will be emitted either to the @.eventlog@ file,
     or to stderr, depending on the runtime RTS flags. }
   with
   has_side_effects = True
   out_of_line      = True

primop  TraceEventBinaryOp "traceBinaryEvent#" GenPrimOp
   Addr# -> Int# -> State# s -> State# s
   { Emits an event via the RTS tracing framework.  The contents
     of the event is the binary object passed as the first argument with
     the given length passed as the second argument. The event will be
     emitted to the @.eventlog@ file. }
   with
   has_side_effects = True
   out_of_line      = True

primop  TraceMarkerOp "traceMarker#" GenPrimOp
   Addr# -> State# s -> State# s
   { Emits a marker event via the RTS tracing framework.  The contents
     of the event is the zero-terminated byte string passed as the first
     argument.  The event will be emitted either to the @.eventlog@ file,
     or to stderr, depending on the runtime RTS flags. }
   with
   has_side_effects = True
   out_of_line      = True

primop  SetThreadAllocationCounter "setThreadAllocationCounter#" GenPrimOp
   Int64# -> State# RealWorld -> State# RealWorld
   { Sets the allocation counter for the current thread to the given value. }
   with
   has_side_effects = True
   out_of_line      = True

primtype StackSnapshot#
   { Haskell representation of a @StgStack*@ that was created (cloned)
     with a function in "GHC.Stack.CloneStack". Please check the
     documentation in that module for more detailed explanations. }

------------------------------------------------------------------------
section "Safe coercions"
------------------------------------------------------------------------

pseudoop   "coerce"
   Coercible a b => a -> b
   { The function 'coerce' allows you to safely convert between values of
     types that have the same representation with no run-time overhead. In the
     simplest case you can use it instead of a newtype constructor, to go from
     the newtype's concrete type to the abstract type. But it also works in
     more complicated settings, e.g. converting a list of newtypes to a list of
     concrete types.

     When used in conversions involving a newtype wrapper,
     make sure the newtype constructor is in scope.

     This function is representation-polymorphic, but the
     'RuntimeRep' type argument is marked as 'Inferred', meaning
     that it is not available for visible type application. This means
     the typechecker will accept @'coerce' \@'Int' \@Age 42@.

     === __Examples__

     >>> newtype TTL = TTL Int deriving (Eq, Ord, Show)
     >>> newtype Age = Age Int deriving (Eq, Ord, Show)
     >>> coerce (Age 42) :: TTL
     TTL 42
     >>> coerce (+ (1 :: Int)) (Age 42) :: TTL
     TTL 43
     >>> coerce (map (+ (1 :: Int))) [Age 42, Age 24] :: [TTL]
     [TTL 43,TTL 25]

   }

------------------------------------------------------------------------
section "SIMD Vectors"
        {Operations on SIMD vectors.}
------------------------------------------------------------------------

#define ALL_VECTOR_TYPES \
  [<Int8,Int8#,16>,<Int16,Int16#,8>,<Int32,Int32#,4>,<Int64,Int64#,2> \
  ,<Int8,Int8#,32>,<Int16,Int16#,16>,<Int32,Int32#,8>,<Int64,Int64#,4> \
  ,<Int8,Int8#,64>,<Int16,Int16#,32>,<Int32,Int32#,16>,<Int64,Int64#,8> \
  ,<Word8,Word8#,16>,<Word16,Word16#,8>,<Word32,Word32#,4>,<Word64,Word64#,2> \
  ,<Word8,Word8#,32>,<Word16,Word16#,16>,<Word32,Word32#,8>,<Word64,Word64#,4> \
  ,<Word8,Word8#,64>,<Word16,Word16#,32>,<Word32,Word32#,16>,<Word64,Word64#,8> \
  ,<Float,Float#,4>,<Double,Double#,2> \
  ,<Float,Float#,8>,<Double,Double#,4> \
  ,<Float,Float#,16>,<Double,Double#,8>]

#define SIGNED_VECTOR_TYPES \
  [<Int8,Int8#,16>,<Int16,Int16#,8>,<Int32,Int32#,4>,<Int64,Int64#,2> \
  ,<Int8,Int8#,32>,<Int16,Int16#,16>,<Int32,Int32#,8>,<Int64,Int64#,4> \
  ,<Int8,Int8#,64>,<Int16,Int16#,32>,<Int32,Int32#,16>,<Int64,Int64#,8> \
  ,<Float,Float#,4>,<Double,Double#,2> \
  ,<Float,Float#,8>,<Double,Double#,4> \
  ,<Float,Float#,16>,<Double,Double#,8>]

#define FLOAT_VECTOR_TYPES \
  [<Float,Float#,4>,<Double,Double#,2> \
  ,<Float,Float#,8>,<Double,Double#,4> \
  ,<Float,Float#,16>,<Double,Double#,8>]

#define INT_VECTOR_TYPES \
  [<Int8,Int8#,16>,<Int16,Int16#,8>,<Int32,Int32#,4>,<Int64,Int64#,2> \
  ,<Int8,Int8#,32>,<Int16,Int16#,16>,<Int32,Int32#,8>,<Int64,Int64#,4> \
  ,<Int8,Int8#,64>,<Int16,Int16#,32>,<Int32,Int32#,16>,<Int64,Int64#,8> \
  ,<Word8,Word8#,16>,<Word16,Word16#,8>,<Word32,Word32#,4>,<Word64,Word64#,2> \
  ,<Word8,Word8#,32>,<Word16,Word16#,16>,<Word32,Word32#,8>,<Word64,Word64#,4> \
  ,<Word8,Word8#,64>,<Word16,Word16#,32>,<Word32,Word32#,16>,<Word64,Word64#,8>]

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

primop VecAddOp "plus#" GenPrimOp
   VECTOR -> VECTOR -> VECTOR
   { Add two vectors element-wise. }
   with commutable = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecSubOp "minus#" GenPrimOp
   VECTOR -> VECTOR -> VECTOR
   { Subtract two vectors element-wise. }
   with llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecMulOp "times#" GenPrimOp
   VECTOR -> VECTOR -> VECTOR
   { Multiply two vectors element-wise. }
   with commutable = True
        llvm_only = True
        vector = ALL_VECTOR_TYPES

primop VecDivOp "divide#" GenPrimOp
   VECTOR -> VECTOR -> VECTOR
   { Divide two vectors element-wise. }
   with can_fail = True
        llvm_only = True
        vector = FLOAT_VECTOR_TYPES

primop VecQuotOp "quot#" GenPrimOp
   VECTOR -> VECTOR -> VECTOR
   { Rounds towards zero element-wise. }
   with can_fail = True
        llvm_only = True
        vector = INT_VECTOR_TYPES

primop VecRemOp "rem#" GenPrimOp
   VECTOR -> VECTOR -> VECTOR
   { Satisfies @('quot#' x y) 'times#' y 'plus#' ('rem#' x y) == x@. }
   with can_fail = True
        llvm_only = True
        vector = INT_VECTOR_TYPES

primop VecNegOp "negate#" GenPrimOp
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

  On the PPC native backend, prefetch*N is a No-Op.

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

  The @prefetch*@ family of operations has the order of operations
  determined by passing around the 'State#' token.

  To get a "pure" version of these operations, use 'inlinePerformIO' which is quite safe in this context.

  It is important to note that while the prefetch operations will never change the
  answer to a pure computation, They CAN change the memory locations resident
  in a CPU cache and that may change the performance and timing characteristics
  of an application. The prefetch operations are marked has_side_effects=True
  to reflect that these operations have side effects with respect to the runtime
  performance characteristics of the resulting code. Additionally, if the prefetchValue
  operations did not have this attribute, GHC does a float out transformation that
  results in a let-can-float invariant violation, at least with the current design.
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
   with has_side_effects =  True
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
   with has_side_effects =  True
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
   with has_side_effects =  True
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
   with has_side_effects =  True


-- Note [RuntimeRep polymorphism in continuation-style primops]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  See below.

section "RuntimeRep polymorphism in continuation-style primops"
  {
  Several primops provided by GHC accept continuation arguments with highly polymorphic
  arguments. For instance, consider the type of `catch#`:

    catch# :: forall (r_rep :: RuntimeRep) (r :: TYPE r_rep) w.
              (State# RealWorld -> (# State# RealWorld, r #) )
           -> (w -> State# RealWorld -> (# State# RealWorld, r #) )
           -> State# RealWorld
           -> (# State# RealWorld, r #)

  This type suggests that we could instantiate `catch#` continuation argument
  (namely, the first argument) with something like,

    f :: State# RealWorld -> (# State# RealWorld, (# Int, String, Int8# #) #)

  However, sadly the type does not capture an important limitation of the
  primop. Specifically, due to the operational behavior of `catch#` the result
  type must be representable with a single machine word. In a future GHC
  release we may improve the precision of this type to capture this limitation.

  See #21868.
  }

------------------------------------------------------------------------
---                                                                  ---
------------------------------------------------------------------------

thats_all_folks
