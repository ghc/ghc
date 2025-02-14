{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE #-}
--
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode assembler and linker
module GHC.ByteCode.Asm (
        assembleBCOs,
        bcoFreeNames,
        SizedSeq, sizeSS, ssElts,
        iNTERP_STACK_CHECK_THRESH,
        mkNativeCallInfoLit,

        -- * For testing
        assembleBCO
  ) where

import GHC.Prelude hiding ( any )


import GHC.ByteCode.Instr
import GHC.ByteCode.InfoTable
import GHC.ByteCode.Types
import GHCi.RemoteTypes
import GHC.Runtime.Interpreter
import GHC.Runtime.Heap.Layout ( fromStgWord, StgWord )

import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Literal
import GHC.Types.Unique.DSet
import GHC.Types.SptEntry
import GHC.Types.Unique.FM

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Core.TyCon
import GHC.Data.SizedSeq
import GHC.Data.SmallArray

import GHC.StgToCmm.Layout     ( ArgRep(..) )
import GHC.Cmm.Expr
import GHC.Cmm.Reg             ( GlobalArgRegs(..) )
import GHC.Cmm.CallConv        ( allArgRegsCover )
import GHC.Platform
import GHC.Platform.Profile

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as MTL

import qualified Data.Array.Unboxed as Array
import qualified Data.Array.IO as Array
import Data.Array.Base  ( UArray(..), numElements, unsafeFreeze )

#if ! defined(DEBUG)
import Data.Array.Base  ( unsafeWrite )
#endif

import Foreign hiding (shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Char  (ord)
import Data.Maybe (fromMaybe)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

import qualified Data.List as List ( any )
import GHC.Exts


-- -----------------------------------------------------------------------------
-- Unlinked BCOs

-- CompiledByteCode represents the result of byte-code
-- compiling a bunch of functions and data types

-- | Finds external references.  Remember to remove the names
-- defined by this group of BCOs themselves
bcoFreeNames :: UnlinkedBCO -> UniqDSet Name
bcoFreeNames bco
  = bco_refs bco `uniqDSetMinusUniqSet` mkNameSet [unlinkedBCOName bco]
  where
    bco_refs (UnlinkedBCO _ _ _ _ nonptrs ptrs)
        = unionManyUniqDSets (
             mkUniqDSet [ n | BCOPtrName n <- elemsFlatBag ptrs ] :
             mkUniqDSet [ n | BCONPtrItbl n <- elemsFlatBag nonptrs ] :
             map bco_refs [ bco | BCOPtrBCO bco <- elemsFlatBag ptrs ]
          )

-- -----------------------------------------------------------------------------
-- The bytecode assembler

-- The object format for bytecodes is: 16 bits for the opcode, and 16
-- for each field -- so the code can be considered a sequence of
-- 16-bit ints.  Each field denotes either a stack offset or number of
-- items on the stack (eg SLIDE), and index into the pointer table (eg
-- PUSH_G), an index into the literal table (eg PUSH_I/D/L), or a
-- bytecode address in this BCO.

-- Top level assembler fn.
assembleBCOs
  :: Interp
  -> Profile
  -> FlatBag (ProtoBCO Name)
  -> [TyCon]
  -> [(Name, ByteString)]
  -> Maybe ModBreaks
  -> [SptEntry]
  -> IO CompiledByteCode
assembleBCOs interp profile proto_bcos tycons top_strs modbreaks spt_entries = do
  -- TODO: the profile should be bundled with the interpreter: the rts ways are
  -- fixed for an interpreter
  itblenv <- mkITbls interp profile tycons
  bcos    <- mapM (assembleBCO (profilePlatform profile)) proto_bcos
  return CompiledByteCode
    { bc_bcos = bcos
    , bc_itbls = itblenv
    , bc_ffis = concatMap protoBCOFFIs proto_bcos
    , bc_strs = top_strs
    , bc_breaks = modbreaks
    , bc_spt_entries = spt_entries
    }

-- Note [Allocating string literals]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Our strategy for handling top-level string literal bindings is described in
-- Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode,
-- but not all Addr# literals in a program are guaranteed to be lifted to the
-- top level. Our strategy for handling local Addr# literals is somewhat simpler:
-- after assembling, we find all the BCONPtrStr arguments in the program, malloc
-- memory for them, and bake the resulting addresses into the instruction stream
-- in the form of BCONPtrWord arguments.
--
-- Since we do this when assembling, we only allocate the memory when we compile
-- the module, not each time we relink it. However, we do want to take care to
-- malloc the memory all in one go, since that is more efficient with
-- -fexternal-interpreter, especially when compiling in parallel.
--
-- Note that, as with top-level string literal bindings, this memory is never
-- freed, so it just leaks if the BCO is unloaded. See Note [Generating code for
-- top-level string literal bindings] in GHC.StgToByteCode for some discussion
-- about why.
--

data RunAsmReader = RunAsmReader { isn_array :: {-# UNPACK #-} !(Array.IOUArray Int Word16)
                                  , ptr_array :: {-# UNPACK #-} !(SmallMutableArrayIO BCOPtr)
                                  , lit_array :: {-# UNPACK #-} !(SmallMutableArrayIO BCONPtr )
                                  }

data RunAsmResult = RunAsmResult { final_isn_array :: !(Array.UArray Int Word16)
                                 , final_ptr_array :: !(SmallArray BCOPtr)
                                 , final_lit_array :: !(SmallArray BCONPtr) }

-- How many words we have written so far.
data AsmState = AsmState { nisn :: !Int, nptr :: !Int, nlit :: !Int }


{-# NOINLINE inspectInstrs #-}
-- | Perform analysis of the bytecode to determine
--  1. How many instructions we will produce
--  2. If we are going to need long jumps.
--  3. The offsets that labels refer to
inspectInstrs :: Platform -> Bool -> Word -> [BCInstr] -> InspectState
inspectInstrs platform long_jump e instrs =
  inspectAsm long_jump e (mapM_ (assembleInspectAsm platform) instrs)

{-# NOINLINE runInstrs #-}
-- | Assemble the bytecode from the instructions.
runInstrs ::  Platform -> Bool -> InspectState -> [BCInstr] -> IO RunAsmResult
runInstrs platform long_jumps is_state instrs = do
  -- Produce arrays of exactly the right size, corresponding to the result of inspectInstrs.
  isn_array <- Array.newArray_ (0, (fromIntegral $ instrCount is_state) - 1)
  ptr_array <- newSmallArrayIO (fromIntegral $ ptrCount is_state) undefined
  lit_array <- newSmallArrayIO (fromIntegral $ litCount is_state) undefined
  let env :: LocalLabel -> Word
      env lbl = fromMaybe
        (pprPanic "assembleBCO.findLabel" (ppr lbl))
        (lookupUFM (lblEnv is_state) lbl)
  let initial_state  = AsmState 0 0 0
  let initial_reader = RunAsmReader{..}
  runAsm long_jumps env initial_reader initial_state (mapM_ (\i -> assembleRunAsm platform i) instrs)
  final_isn_array <- unsafeFreeze isn_array
  final_ptr_array <- unsafeFreezeSmallArrayIO ptr_array
  final_lit_array <- unsafeFreezeSmallArrayIO lit_array
  return $ RunAsmResult {..}

assembleRunAsm :: Platform -> BCInstr -> RunAsm ()
assembleRunAsm p i = assembleI @RunAsm p i

assembleInspectAsm :: Platform -> BCInstr -> InspectAsm ()
assembleInspectAsm p i = assembleI @InspectAsm p i

assembleBCO :: Platform -> ProtoBCO Name -> IO UnlinkedBCO
assembleBCO platform
            (ProtoBCO { protoBCOName       = nm
                      , protoBCOInstrs     = instrs
                      , protoBCOBitmap     = bitmap
                      , protoBCOBitmapSize = bsize
                      , protoBCOArity      = arity }) = do
  -- pass 1: collect up the offsets of the local labels.
  let initial_offset = 0

      -- Jump instructions are variable-sized, there are long and short variants
      -- depending on the magnitude of the offset.  However, we can't tell what
      -- size instructions we will need until we have calculated the offsets of
      -- the labels, which depends on the size of the instructions...  So we
      -- first create the label environment assuming that all jumps are short,
      -- and if the final size is indeed small enough for short jumps, we are
      -- done.  Otherwise, we repeat the calculation, and we force all jumps in
      -- this BCO to be long.
      is0 = inspectInstrs platform False initial_offset instrs
      (is1, long_jumps)
        | isLargeInspectState is0
                    = (inspectInstrs platform True initial_offset instrs, True)
        | otherwise = (is0, False)


  -- pass 2: run assembler and generate instructions, literals and pointers
  RunAsmResult{..} <- runInstrs platform long_jumps is1 instrs

  -- precomputed size should be equal to final size
  massertPpr (fromIntegral (instrCount is1) == numElements final_isn_array
              && fromIntegral (ptrCount is1) == sizeofSmallArray final_ptr_array
              && fromIntegral (litCount is1) == sizeofSmallArray final_lit_array)
             (text "bytecode instruction count mismatch")

  let !insns_arr =  mkBCOByteArray $ final_isn_array
      !bitmap_arr = mkBCOByteArray $ mkBitmapArray bsize bitmap
      ul_bco = UnlinkedBCO nm arity insns_arr bitmap_arr (fromSmallArray final_lit_array) (fromSmallArray final_ptr_array)

  -- 8 Aug 01: Finalisers aren't safe when attached to non-primitive
  -- objects, since they might get run too early.  Disable this until
  -- we figure out what to do.
  -- when (notNull malloced) (addFinalizer ul_bco (mapM_ zonk malloced))

  return ul_bco

mkBitmapArray :: Word -> [StgWord] -> UArray Int Word
-- Here the return type must be an array of Words, not StgWords,
-- because the underlying ByteArray# will end up as a component
-- of a BCO object.
mkBitmapArray bsize bitmap
  = Array.listArray (0, length bitmap) $
      fromIntegral bsize : map (fromInteger . fromStgWord) bitmap


data Operand
  = Op Word
  | IOp Int
  | SmallOp Word16
  | LabelOp LocalLabel

wOp :: WordOff -> Operand
wOp = Op . fromIntegral

bOp :: ByteOff -> Operand
bOp = Op . fromIntegral

truncHalfWord :: Platform -> HalfWord -> Operand
truncHalfWord platform w = case platformWordSize platform of
  PW4 | w <= 65535      -> Op (fromIntegral w)
  PW8 | w <= 4294967295 -> Op (fromIntegral w)
  _ -> pprPanic "GHC.ByteCode.Asm.truncHalfWord" (ppr w)


ptr :: MonadAssembler m => BCOPtr -> m Word
ptr = ioptr . return

type LabelEnv = LocalLabel -> Word

largeOp :: Bool -> Operand -> Bool
largeOp long_jumps op = case op of
   SmallOp _ -> False
   Op w      -> isLargeW w
   IOp i     -> isLargeI i
   LabelOp _ -> long_jumps

newtype RunAsm a = RunAsm' { runRunAsm :: Bool
                                       -> LabelEnv
                                       -> RunAsmReader
                                       -> AsmState
                                       -> IO (AsmState, a) }

pattern RunAsm :: (Bool -> LabelEnv -> RunAsmReader -> AsmState -> IO (AsmState, a))
                  -> RunAsm a
pattern RunAsm m <- RunAsm' m
  where
    RunAsm m = RunAsm' (oneShot $ \a -> oneShot $ \b -> oneShot $ \c -> oneShot $ \d -> m a b c d)
{-# COMPLETE RunAsm #-}

instance Functor RunAsm where
  fmap f (RunAsm x) = RunAsm (\a b c !s -> fmap (fmap f) (x a b c s))

instance Applicative RunAsm where
  pure x = RunAsm $ \_ _ _ !s -> pure (s, x)
  (RunAsm f) <*> (RunAsm x) = RunAsm $ \a b c !s -> do
                                  (!s', f') <- f a b c s
                                  (!s'', x') <- x a b c s'
                                  return (s'', f' x')
  {-# INLINE (<*>) #-}


instance Monad RunAsm where
  return  = pure
  (RunAsm m) >>= f = RunAsm $ \a b c !s -> m a b c s >>= \(s', r) -> runRunAsm (f r) a b c s'
  {-# INLINE (>>=) #-}

runAsm :: Bool -> LabelEnv -> RunAsmReader -> AsmState -> RunAsm a -> IO a
runAsm long_jumps e r s (RunAsm'{runRunAsm}) = fmap snd $ runRunAsm long_jumps e r s

expand :: PlatformWordSize -> Bool -> Operand -> RunAsm ()
expand word_size largeArgs o = do
  e <- askEnv
  case o of
    (SmallOp w) -> writeIsn w
    (LabelOp w) -> let !r = e w in handleLargeArg r
    (Op w) -> handleLargeArg w
    (IOp i) -> handleLargeArg i

  where
    handleLargeArg :: Integral a => a -> RunAsm ()
    handleLargeArg w  =
      if largeArgs
        then largeArg word_size (fromIntegral w)
        else writeIsn (fromIntegral w)

lift :: IO a -> RunAsm a
lift io = RunAsm $ \_ _ _ s -> io >>= \a -> pure (s, a)

askLongJumps :: RunAsm Bool
askLongJumps = RunAsm $ \a _ _ s -> pure (s, a)

askEnv :: RunAsm LabelEnv
askEnv = RunAsm $ \_ b _ s -> pure (s, b)

writePtr :: BCOPtr -> RunAsm Word
writePtr w
            = RunAsm $ \_ _ (RunAsmReader{..}) asm -> do
              writeSmallArrayIO ptr_array (nptr asm) w
              let !n' = nptr asm + 1
              let !asm' = asm { nptr = n' }
              return (asm', fromIntegral (nptr asm))

writeLit :: BCONPtr -> RunAsm Word
writeLit w = RunAsm $ \_ _ (RunAsmReader{..}) asm -> do
              writeSmallArrayIO lit_array (nlit asm) w
              let !n' = nlit asm + 1
              let !asm' = asm { nlit = n' }
              return (asm', fromIntegral (nlit asm))

writeLits :: OneOrTwo BCONPtr -> RunAsm Word
writeLits (OnlyOne l) = writeLit l
writeLits (OnlyTwo l1 l2) = writeLit l1 <* writeLit l2

writeIsn :: Word16 -> RunAsm ()
writeIsn w = RunAsm $ \_ _ (RunAsmReader{..}) asm -> do
#if defined(DEBUG)
              Array.writeArray isn_array (nisn asm) w
#else
              unsafeWrite isn_array (nisn asm) w
#endif
              let !n' = nisn asm + 1
              let !asm' = asm { nisn = n' }
              return (asm', ())

{-# INLINE any #-}
-- Any is unrolled manually so that the call in `emit` can be eliminated without
-- relying on SpecConstr (which does not work across modules).
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f [x] = f x
any f [x,y] = f x || f y
any f [x,y,z] = f x || f y || f z
any f [x1,x2,x3,x4] = f x1 || f x2 || f x3 || f x4
any f [x1,x2,x3,x4, x5] = f x1 || f x2 || f x3 || f x4 || f x5
any f [x1,x2,x3,x4,x5,x6] = f x1 || f x2 || f x3 || f x4 || f x5 || f x6
any f xs = List.any f xs

{-# INLINE mapM6_ #-}
mapM6_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM6_ _ [] = return ()
mapM6_ f [x] = () <$ f x
mapM6_ f [x,y] = () <$ f x <* f y
mapM6_ f [x,y,z] = () <$ f x <* f y <* f z
mapM6_ f [a1,a2,a3,a4] = () <$ f a1 <* f a2 <* f a3 <* f a4
mapM6_ f [a1,a2,a3,a4,a5] = () <$ f a1 <* f a2 <* f a3 <* f a4 <* f a5
mapM6_ f [a1,a2,a3,a4,a5,a6] = () <$ f a1 <* f a2 <* f a3 <* f a4 <* f a5 <* f a6
mapM6_ f xs = mapM_ f xs

instance MonadAssembler RunAsm where
  ioptr p_io = do
    p <- lift p_io
    writePtr p
  lit lits = writeLits lits

  label _ = return ()

  emit pwordsize w ops = do
    long_jumps <- askLongJumps
    -- See the definition of `any` above
    let largeArgs = any (largeOp long_jumps) ops
    let opcode
          | largeArgs = largeArgInstr w
          | otherwise = w
    writeIsn opcode
    mapM6_ (expand pwordsize largeArgs) ops

  {-# INLINE emit #-}
  {-# INLINE label #-}
  {-# INLINE lit #-}
  {-# INLINE ioptr #-}

type LabelEnvMap = UniqFM LocalLabel Word

data InspectState = InspectState
  { instrCount :: !Word
  , ptrCount :: !Word
  , litCount :: !Word
  , lblEnv :: LabelEnvMap
  }

instance Outputable InspectState where
  ppr (InspectState i p l m) = text "InspectState" <+> ppr [ppr i, ppr p, ppr l, ppr (sizeUFM m)]

isLargeInspectState :: InspectState -> Bool
isLargeInspectState InspectState{..} =
  isLargeW (fromIntegral $ sizeUFM lblEnv)
    || isLargeW instrCount

newtype InspectEnv = InspectEnv { _inspectLongJumps :: Bool
                                }

newtype InspectAsm a = InspectAsm' { runInspectAsm :: InspectEnv -> InspectState -> (# InspectState,  a #) }

pattern InspectAsm :: (InspectEnv -> InspectState -> (# InspectState, a #))
                   -> InspectAsm a
pattern InspectAsm m <- InspectAsm' m
  where
    InspectAsm m = InspectAsm' (oneShot $ \a -> oneShot $ \b -> m a b)
{-# COMPLETE InspectAsm #-}

instance Functor InspectAsm where
  fmap f (InspectAsm k) = InspectAsm $ \a b -> case k a b of
                                                  (# b', c #) -> (# b', f c #)

instance Applicative InspectAsm where
  pure x = InspectAsm $ \_ s -> (# s, x #)
  (InspectAsm f) <*> (InspectAsm x) = InspectAsm $ \a b -> case f a b of
                                                              (# s', f' #) ->
                                                                case x a s' of
                                                                  (# s'', x' #) -> (# s'', f' x' #)

instance Monad InspectAsm where
  return = pure
  (InspectAsm m) >>= f = InspectAsm $ \ a b -> case m a b of
                                                (# s', a' #) -> runInspectAsm (f a') a s'

get_ :: InspectAsm InspectState
get_ = InspectAsm $ \_ b -> (# b, b #)

put_ :: InspectState -> InspectAsm ()
put_ !s = InspectAsm $ \_ _ -> (# s, () #)

modify_ :: (InspectState -> InspectState) -> InspectAsm ()
modify_ f = InspectAsm $ \_ s -> let !s' = f s in (# s', () #)

ask_ :: InspectAsm InspectEnv
ask_ = InspectAsm $ \a b -> (# b, a #)

inspectAsm :: Bool -> Word -> InspectAsm () -> InspectState
inspectAsm long_jumps initial_offset (InspectAsm s) =
  case s (InspectEnv long_jumps) (InspectState initial_offset 0 0 emptyUFM) of
    (# res, () #) -> res
{-# INLINE inspectAsm #-}



instance MonadAssembler InspectAsm where
  ioptr _ = do
    s <- get_
    let n = ptrCount s
    put_ (s { ptrCount = n + 1 })
    return n

  lit ls = do
    s <- get_
    let n = litCount s
    put_ (s { litCount = n + oneTwoLength ls })
    return n

  label lbl = modify_ (\s -> let !count = instrCount s in let !env' = addToUFM (lblEnv s) lbl count in s { lblEnv = env' })

  emit pwordsize _ ops = do
    InspectEnv long_jumps <- ask_
    -- Size is written in this way as `mapM6_` is also used by RunAsm, and guaranteed
    -- to unroll for arguments up to size 6.
    let size = (MTL.execState (mapM6_ (\x -> MTL.modify (count' x +)) ops) 0) + 1
        largeOps = any (largeOp long_jumps) ops
        bigSize = largeArg16s pwordsize
        count' = if largeOps then countLarge bigSize else countSmall bigSize

    s <- get_
    put_ (s { instrCount = instrCount s + size })

  {-# INLINE emit #-}
  {-# INLINE label #-}
  {-# INLINE lit #-}
  {-# INLINE ioptr #-}

count :: Word -> Bool -> Operand -> Word
count _ _ (SmallOp _)          = 1
count big largeOps (LabelOp _) = if largeOps then big else 1
count big largeOps (Op _)      = if largeOps then big else 1
count big largeOps (IOp _)     = if largeOps then big else 1
{-# INLINE count #-}

countSmall, countLarge :: Word -> Operand -> Word
countLarge big x = count big True x
countSmall big x = count big False x


-- Bring in all the bci_ bytecode constants.
#include "Bytecodes.h"

largeArgInstr :: Word16 -> Word16
largeArgInstr bci = bci_FLAG_LARGE_ARGS .|. bci

{-# INLINE largeArg #-}
largeArg :: PlatformWordSize -> Word64 -> RunAsm ()
largeArg wsize w = case wsize of
   PW8 ->  do writeIsn (fromIntegral (w `shiftR` 48))
              writeIsn (fromIntegral (w `shiftR` 32))
              writeIsn (fromIntegral (w `shiftR` 16))
              writeIsn (fromIntegral w)
   PW4 -> assertPpr (w < fromIntegral (maxBound :: Word32))
                    (text "largeArg too big:" <+> ppr w) $ do
          writeIsn (fromIntegral (w `shiftR` 16))
          writeIsn (fromIntegral w)

largeArg16s :: PlatformWordSize -> Word
largeArg16s pwordsize = case pwordsize of
   PW8 -> 4
   PW4 -> 2

data OneOrTwo a = OnlyOne a | OnlyTwo a a deriving (Functor)

oneTwoLength :: OneOrTwo a -> Word
oneTwoLength (OnlyOne {}) = 1
oneTwoLength (OnlyTwo {}) = 2

class Monad m => MonadAssembler m where
  ioptr :: IO BCOPtr -> m Word
  lit :: OneOrTwo BCONPtr -> m Word
  label :: LocalLabel -> m ()
  emit :: PlatformWordSize -> Word16 -> [Operand] -> m ()

lit1 :: MonadAssembler m => BCONPtr -> m Word
lit1 p = lit (OnlyOne p)

{-# SPECIALISE assembleI :: Platform -> BCInstr -> InspectAsm () #-}
{-# SPECIALISE assembleI :: Platform -> BCInstr -> RunAsm () #-}

assembleI :: forall m . MonadAssembler m
          => Platform
          -> BCInstr
          -> m ()
assembleI platform i = case i of
  STKCHECK n               -> emit_ bci_STKCHECK [Op n]
  PUSH_L o1                -> emit_ bci_PUSH_L [wOp o1]
  PUSH_LL o1 o2            -> emit_ bci_PUSH_LL [wOp o1, wOp o2]
  PUSH_LLL o1 o2 o3        -> emit_ bci_PUSH_LLL [wOp o1, wOp o2, wOp o3]
  PUSH8 o1                 -> emit_ bci_PUSH8 [bOp o1]
  PUSH16 o1                -> emit_ bci_PUSH16 [bOp o1]
  PUSH32 o1                -> emit_ bci_PUSH32 [bOp o1]
  PUSH8_W o1               -> emit_ bci_PUSH8_W [bOp o1]
  PUSH16_W o1              -> emit_ bci_PUSH16_W [bOp o1]
  PUSH32_W o1              -> emit_ bci_PUSH32_W [bOp o1]
  PUSH_G nm                -> do p <- ptr (BCOPtrName nm)
                                 emit_ bci_PUSH_G [Op p]
  PUSH_PRIMOP op           -> do p <- ptr (BCOPtrPrimOp op)
                                 emit_ bci_PUSH_G [Op p]
  PUSH_BCO proto           -> do let ul_bco = assembleBCO platform proto
                                 p <- ioptr (liftM BCOPtrBCO ul_bco)
                                 emit_ bci_PUSH_G [Op p]
  PUSH_ALTS proto pk
                           -> do let ul_bco = assembleBCO platform proto
                                 p <- ioptr (liftM BCOPtrBCO ul_bco)
                                 emit_ (push_alts pk) [Op p]
  PUSH_ALTS_TUPLE proto call_info tuple_proto
                           -> do let ul_bco = assembleBCO platform proto
                                     ul_tuple_bco = assembleBCO platform
                                                                tuple_proto
                                 p <- ioptr (liftM BCOPtrBCO ul_bco)
                                 p_tup <- ioptr (liftM BCOPtrBCO ul_tuple_bco)
                                 info <- word (fromIntegral $
                                              mkNativeCallInfoSig platform call_info)
                                 emit_ bci_PUSH_ALTS_T
                                      [Op p, Op info, Op p_tup]
  PUSH_PAD8                -> emit_ bci_PUSH_PAD8 []
  PUSH_PAD16               -> emit_ bci_PUSH_PAD16 []
  PUSH_PAD32               -> emit_ bci_PUSH_PAD32 []
  PUSH_UBX8 lit            -> do np <- literal lit
                                 emit_ bci_PUSH_UBX8 [Op np]
  PUSH_UBX16 lit           -> do np <- literal lit
                                 emit_ bci_PUSH_UBX16 [Op np]
  PUSH_UBX32 lit           -> do np <- literal lit
                                 emit_ bci_PUSH_UBX32 [Op np]
  PUSH_UBX lit nws         -> do np <- literal lit
                                 emit_ bci_PUSH_UBX [Op np, wOp nws]
  -- see Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode
  PUSH_ADDR nm             -> do np <- lit1 (BCONPtrAddr nm)
                                 emit_ bci_PUSH_UBX [Op np, SmallOp 1]

  PUSH_APPLY_N             -> emit_ bci_PUSH_APPLY_N []
  PUSH_APPLY_V             -> emit_ bci_PUSH_APPLY_V []
  PUSH_APPLY_F             -> emit_ bci_PUSH_APPLY_F []
  PUSH_APPLY_D             -> emit_ bci_PUSH_APPLY_D []
  PUSH_APPLY_L             -> emit_ bci_PUSH_APPLY_L []
  PUSH_APPLY_P             -> emit_ bci_PUSH_APPLY_P []
  PUSH_APPLY_PP            -> emit_ bci_PUSH_APPLY_PP []
  PUSH_APPLY_PPP           -> emit_ bci_PUSH_APPLY_PPP []
  PUSH_APPLY_PPPP          -> emit_ bci_PUSH_APPLY_PPPP []
  PUSH_APPLY_PPPPP         -> emit_ bci_PUSH_APPLY_PPPPP []
  PUSH_APPLY_PPPPPP        -> emit_ bci_PUSH_APPLY_PPPPPP []

  SLIDE     n by           -> emit_ bci_SLIDE [wOp n, wOp by]
  ALLOC_AP  n              -> emit_ bci_ALLOC_AP [truncHalfWord platform n]
  ALLOC_AP_NOUPD n         -> emit_ bci_ALLOC_AP_NOUPD [truncHalfWord platform n]
  ALLOC_PAP arity n        -> emit_ bci_ALLOC_PAP [truncHalfWord platform arity, truncHalfWord platform n]
  MKAP      off sz         -> emit_ bci_MKAP [wOp off, truncHalfWord platform sz]
  MKPAP     off sz         -> emit_ bci_MKPAP [wOp off, truncHalfWord platform sz]
  UNPACK    n              -> emit_ bci_UNPACK [wOp n]
  PACK      dcon sz        -> do itbl_no <- lit1 (BCONPtrItbl (getName dcon))
                                 emit_ bci_PACK [Op itbl_no, wOp sz]
  LABEL     lbl            -> label lbl
  TESTLT_I  i l            -> do np <- int i
                                 emit_ bci_TESTLT_I [Op np, LabelOp l]
  TESTEQ_I  i l            -> do np <- int i
                                 emit_ bci_TESTEQ_I [Op np, LabelOp l]
  TESTLT_W  w l            -> do np <- word w
                                 emit_ bci_TESTLT_W [Op np, LabelOp l]
  TESTEQ_W  w l            -> do np <- word w
                                 emit_ bci_TESTEQ_W [Op np, LabelOp l]
  TESTLT_I64  i l          -> do np <- word64 (fromIntegral i)
                                 emit_ bci_TESTLT_I64 [Op np, LabelOp l]
  TESTEQ_I64  i l          -> do np <- word64 (fromIntegral i)
                                 emit_ bci_TESTEQ_I64 [Op np, LabelOp l]
  TESTLT_I32  i l          -> do np <- word (fromIntegral i)
                                 emit_ bci_TESTLT_I32 [Op np, LabelOp l]
  TESTEQ_I32 i l           -> do np <- word (fromIntegral i)
                                 emit_ bci_TESTEQ_I32 [Op np, LabelOp l]
  TESTLT_I16  i l          -> do np <- word (fromIntegral i)
                                 emit_ bci_TESTLT_I16 [Op np, LabelOp l]
  TESTEQ_I16 i l           -> do np <- word (fromIntegral i)
                                 emit_ bci_TESTEQ_I16 [Op np, LabelOp l]
  TESTLT_I8  i l           -> do np <- word (fromIntegral i)
                                 emit_ bci_TESTLT_I8 [Op np, LabelOp l]
  TESTEQ_I8 i l            -> do np <- word (fromIntegral i)
                                 emit_ bci_TESTEQ_I8 [Op np, LabelOp l]
  TESTLT_W64  w l          -> do np <- word64 w
                                 emit_ bci_TESTLT_W64 [Op np, LabelOp l]
  TESTEQ_W64  w l          -> do np <- word64 w
                                 emit_ bci_TESTEQ_W64 [Op np, LabelOp l]
  TESTLT_W32  w l          -> do np <- word (fromIntegral w)
                                 emit_ bci_TESTLT_W32 [Op np, LabelOp l]
  TESTEQ_W32  w l          -> do np <- word (fromIntegral w)
                                 emit_ bci_TESTEQ_W32 [Op np, LabelOp l]
  TESTLT_W16  w l          -> do np <- word (fromIntegral w)
                                 emit_ bci_TESTLT_W16 [Op np, LabelOp l]
  TESTEQ_W16  w l          -> do np <- word (fromIntegral w)
                                 emit_ bci_TESTEQ_W16 [Op np, LabelOp l]
  TESTLT_W8  w l           -> do np <- word (fromIntegral w)
                                 emit_ bci_TESTLT_W8 [Op np, LabelOp l]
  TESTEQ_W8  w l           -> do np <- word (fromIntegral w)
                                 emit_ bci_TESTEQ_W8 [Op np, LabelOp l]
  TESTLT_F  f l            -> do np <- float f
                                 emit_ bci_TESTLT_F [Op np, LabelOp l]
  TESTEQ_F  f l            -> do np <- float f
                                 emit_ bci_TESTEQ_F [Op np, LabelOp l]
  TESTLT_D  d l            -> do np <- double d
                                 emit_ bci_TESTLT_D [Op np, LabelOp l]
  TESTEQ_D  d l            -> do np <- double d
                                 emit_ bci_TESTEQ_D [Op np, LabelOp l]
  TESTLT_P  i l            -> emit_ bci_TESTLT_P [SmallOp i, LabelOp l]
  TESTEQ_P  i l            -> emit_ bci_TESTEQ_P [SmallOp i, LabelOp l]
  CASEFAIL                 -> emit_ bci_CASEFAIL []
  SWIZZLE   stkoff n       -> emit_ bci_SWIZZLE [wOp stkoff, IOp n]
  JMP       l              -> emit_ bci_JMP [LabelOp l]
  ENTER                    -> emit_ bci_ENTER []
  RETURN rep               -> emit_ (return_non_tuple rep) []
  RETURN_TUPLE             -> emit_ bci_RETURN_T []
  CCALL off m_addr i       -> do np <- addr m_addr
                                 emit_ bci_CCALL [wOp off, Op np, SmallOp i]
  PRIMCALL                 -> emit_ bci_PRIMCALL []
  BRK_FUN arr tick_mod tickx info_mod infox cc ->
                              do p1 <- ptr (BCOPtrBreakArray arr)
                                 tick_addr <- addr tick_mod
                                 info_addr <- addr info_mod
                                 np <- addr cc
                                 emit_ bci_BRK_FUN [ Op p1
                                                  , Op tick_addr, Op info_addr
                                                  , SmallOp tickx, SmallOp infox
                                                  , Op np
                                                  ]
#if MIN_VERSION_rts(1,0,3)
  BCO_NAME name            -> do np <- lit1 (BCONPtrStr name)
                                 emit_ bci_BCO_NAME [Op np]
#endif



  where
    emit_ = emit word_size

    literal :: Literal -> m Word
    literal (LitLabel fs _)   = litlabel fs
    literal LitNullAddr       = word 0
    literal (LitFloat r)      = float (fromRational r)
    literal (LitDouble r)     = double (fromRational r)
    literal (LitChar c)       = int (ord c)
    literal (LitString bs)    = lit1 (BCONPtrStr bs)
       -- LitString requires a zero-terminator when emitted
    literal (LitNumber nt i) = case nt of
      LitNumInt     -> word (fromIntegral i)
      LitNumWord    -> word (fromIntegral i)
      LitNumInt8    -> word8 (fromIntegral i)
      LitNumWord8   -> word8 (fromIntegral i)
      LitNumInt16   -> word16 (fromIntegral i)
      LitNumWord16  -> word16 (fromIntegral i)
      LitNumInt32   -> word32 (fromIntegral i)
      LitNumWord32  -> word32 (fromIntegral i)
      LitNumInt64   -> word64 (fromIntegral i)
      LitNumWord64  -> word64 (fromIntegral i)
      LitNumBigNat  -> panic "GHC.ByteCode.Asm.literal: LitNumBigNat"

    -- We can lower 'LitRubbish' to an arbitrary constant, but @NULL@ is most
    -- likely to elicit a crash (rather than corrupt memory) in case absence
    -- analysis messed up.
    literal (LitRubbish {}) = word 0

    litlabel fs = lit1 (BCONPtrLbl fs)
    addr (RemotePtr a) = word (fromIntegral a)
    words ws = lit (fmap BCONPtrWord ws)
    word w = words (OnlyOne w)
    word2 w1 w2 = words (OnlyTwo w1 w2)
    word_size  = platformWordSize platform
    word_size_bits = platformWordSizeInBits platform

    -- Make lists of host-sized words for literals, so that when the
    -- words are placed in memory at increasing addresses, the
    -- bit pattern is correct for the host's word size and endianness.
    --
    -- Note that we only support host endianness == target endianness for now,
    -- even with the external interpreter. This would need to be fixed to
    -- support host endianness /= target endianness
    int :: Int -> m Word
    int  i = word (fromIntegral i)

    float :: Float -> m Word
    float f = word32 (castFloatToWord32 f)

    double :: Double -> m Word
    double d = word64 (castDoubleToWord64 d)

    word64 :: Word64 -> m Word
    word64 ww = case word_size of
       PW4 ->
        let !wl = fromIntegral ww
            !wh = fromIntegral (ww `unsafeShiftR` 32)
        in case platformByteOrder platform of
            LittleEndian -> word2 wl wh
            BigEndian    -> word2 wh wl
       PW8 -> word (fromIntegral ww)

    word8 :: Word8 -> m Word
    word8  x = case platformByteOrder platform of
      LittleEndian -> word (fromIntegral x)
      BigEndian    -> word (fromIntegral x `unsafeShiftL` (word_size_bits - 8))

    word16 :: Word16 -> m Word
    word16 x = case platformByteOrder platform of
      LittleEndian -> word (fromIntegral x)
      BigEndian    -> word (fromIntegral x `unsafeShiftL` (word_size_bits - 16))

    word32 :: Word32 -> m Word
    word32 x = case platformByteOrder platform of
      LittleEndian -> word (fromIntegral x)
      BigEndian    -> case word_size of
        PW4 -> word (fromIntegral x)
        PW8 -> word (fromIntegral x `unsafeShiftL` 32)


isLargeW :: Word -> Bool
isLargeW n = n > 65535

isLargeI :: Int -> Bool
isLargeI n = n > 32767 || n < -32768

push_alts :: ArgRep -> Word16
push_alts V   = bci_PUSH_ALTS_V
push_alts P   = bci_PUSH_ALTS_P
push_alts N   = bci_PUSH_ALTS_N
push_alts L   = bci_PUSH_ALTS_L
push_alts F   = bci_PUSH_ALTS_F
push_alts D   = bci_PUSH_ALTS_D
push_alts V16 = error "push_alts: vector"
push_alts V32 = error "push_alts: vector"
push_alts V64 = error "push_alts: vector"

return_non_tuple :: ArgRep -> Word16
return_non_tuple V   = bci_RETURN_V
return_non_tuple P   = bci_RETURN_P
return_non_tuple N   = bci_RETURN_N
return_non_tuple L   = bci_RETURN_L
return_non_tuple F   = bci_RETURN_F
return_non_tuple D   = bci_RETURN_D
return_non_tuple V16 = error "return_non_tuple: vector"
return_non_tuple V32 = error "return_non_tuple: vector"
return_non_tuple V64 = error "return_non_tuple: vector"

{-
  we can only handle up to a fixed number of words on the stack,
  because we need a stg_ctoi_tN stack frame for each size N. See
  Note [unboxed tuple bytecodes and tuple_BCO].

  If needed, you can support larger tuples by adding more in
  Jumps.cmm, StgMiscClosures.cmm, Interpreter.c and MiscClosures.h and
  raising this limit.

  Note that the limit is the number of words passed on the stack.
  If the calling convention passes part of the tuple in registers, the
  maximum number of tuple elements may be larger. Elements can also
  take multiple words on the stack (for example Double# on a 32 bit
  platform).
 -}
maxTupleReturnNativeStackSize :: WordOff
maxTupleReturnNativeStackSize = 62

{-
  Construct the call_info word that stg_ctoi_t, stg_ret_t and stg_primcall
  use to convert arguments between the native calling convention and the
  interpreter.

  See Note [GHCi and native call registers] for more information.
 -}
mkNativeCallInfoSig :: Platform -> NativeCallInfo -> Word32
mkNativeCallInfoSig platform NativeCallInfo{..}
  | nativeCallType == NativeTupleReturn && nativeCallStackSpillSize > maxTupleReturnNativeStackSize
  = pprPanic "mkNativeCallInfoSig: tuple too big for the bytecode compiler"
             (ppr nativeCallStackSpillSize <+> text "stack words." <+>
              text "Use -fobject-code to get around this limit"
             )
  | otherwise
  = -- 24 bits for register bitmap
    assertPpr (length argRegs <= 24) (text "too many registers for bitmap:" <+> ppr (length argRegs))

    -- 8 bits for continuation offset (only for NativeTupleReturn)
    assertPpr (cont_offset < 255) (text "continuation offset too large:" <+> ppr cont_offset)

    -- all regs accounted for
    assertPpr (all (`elem` (map fst argRegs)) (regSetToList nativeCallRegs))
      ( vcat
        [ text "not all registers accounted for"
        , text "argRegs:" <+> ppr argRegs
        , text "nativeCallRegs:" <+> ppr nativeCallRegs
        ] ) $
      -- SIMD GHCi TODO: the above assertion doesn't account for register overlap;
      -- it will need to be adjusted for SIMD vector support in the bytecode interpreter.

    foldl' reg_bit 0 argRegs .|. (cont_offset `shiftL` 24)
  where
    cont_offset :: Word32
    cont_offset
      | nativeCallType == NativeTupleReturn = fromIntegral nativeCallStackSpillSize
      | otherwise                           = 0 -- there is no continuation for primcalls

    reg_bit :: Word32 -> (GlobalReg, Int) -> Word32
    reg_bit x (r, n)
      | r `elemRegSet` nativeCallRegs = x .|. 1 `shiftL` n
      | otherwise                     = x
    argRegs = zip (allArgRegsCover platform SCALAR_ARG_REGS) [0..]
      -- The bytecode interpreter does not (currently) handle vector registers,
      -- so we only use the scalar argument-passing registers here.

mkNativeCallInfoLit :: Platform -> NativeCallInfo -> Literal
mkNativeCallInfoLit platform call_info =
  mkLitWord platform . fromIntegral $ mkNativeCallInfoSig platform call_info

iNTERP_STACK_CHECK_THRESH :: Int
iNTERP_STACK_CHECK_THRESH = INTERP_STACK_CHECK_THRESH
