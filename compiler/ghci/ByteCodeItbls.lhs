%
% (c) The University of Glasgow 2000-2006
%
ByteCodeItbls: Generate infotables for interpreter-made bytecodes

\begin{code}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module ByteCodeItbls ( ItblEnv, ItblPtr, mkITbls ) where

#include "HsVersions.h"

import Name		( Name, getName )
import NameEnv
import SMRep		( typeCgRep )
import DataCon		( DataCon, dataConRepArgTys )
import TyCon		( TyCon, tyConFamilySize, isDataTyCon, tyConDataCons )
import Constants	( mIN_PAYLOAD_SIZE, wORD_SIZE )
import CgHeapery	( mkVirtHeapOffsets )
import FastString	( FastString(..) )
import Util             ( lengthIs, listLengthCmp )

import Foreign
import Foreign.C
import Data.Bits	( Bits(..), shiftR )

import GHC.Exts		( Int(I#), addr2Int# )
import GHC.Ptr		( Ptr(..) )
\end{code}

%************************************************************************
%*									*
\subsection{Manufacturing of info tables for DataCons}
%*									*
%************************************************************************

\begin{code}
type ItblPtr = Ptr StgInfoTable
type ItblEnv = NameEnv (Name, ItblPtr)
	-- We need the Name in the range so we know which
	-- elements to filter out when unloading a module

mkItblEnv :: [(Name,ItblPtr)] -> ItblEnv
mkItblEnv pairs = mkNameEnv [(n, (n,p)) | (n,p) <- pairs]


-- Make info tables for the data decls in this module
mkITbls :: [TyCon] -> IO ItblEnv
mkITbls [] = return emptyNameEnv
mkITbls (tc:tcs) = do itbls  <- mkITbl tc
                      itbls2 <- mkITbls tcs
                      return (itbls `plusNameEnv` itbls2)

mkITbl :: TyCon -> IO ItblEnv
mkITbl tc
   | not (isDataTyCon tc) 
   = return emptyNameEnv
   | dcs `lengthIs` n -- paranoia; this is an assertion.
   = make_constr_itbls dcs
     where
        dcs = tyConDataCons tc
        n   = tyConFamilySize tc

#include "../includes/ClosureTypes.h"
cONSTR :: Int	-- Defined in ClosureTypes.h
cONSTR = CONSTR 

-- Assumes constructors are numbered from zero, not one
make_constr_itbls :: [DataCon] -> IO ItblEnv
make_constr_itbls cons
   | listLengthCmp cons 8 /= GT -- <= 8 elements in the list
   = do is <- mapM mk_vecret_itbl (zip cons [0..])
	return (mkItblEnv is)
   | otherwise
   = do is <- mapM mk_dirret_itbl (zip cons [0..])
	return (mkItblEnv is)
     where
        mk_vecret_itbl (dcon, conNo)
           = mk_itbl dcon conNo (vecret_entry conNo)
        mk_dirret_itbl (dcon, conNo)
           = mk_itbl dcon conNo stg_interp_constr_entry

        mk_itbl :: DataCon -> Int -> Ptr () -> IO (Name,ItblPtr)
        mk_itbl dcon conNo entry_addr
           = let rep_args = [ (typeCgRep arg,arg) 
			    | arg <- dataConRepArgTys dcon ]
		 (tot_wds, ptr_wds, _) = mkVirtHeapOffsets False{-not a THUNK-} rep_args

                 ptrs  = ptr_wds
                 nptrs = tot_wds - ptr_wds
                 nptrs_really
                    | ptrs + nptrs >= mIN_PAYLOAD_SIZE = nptrs
                    | otherwise = mIN_PAYLOAD_SIZE - ptrs
                 itbl  = StgInfoTable {
#ifndef GHCI_TABLES_NEXT_TO_CODE
                           entry = entry_addr,
#endif
                           ptrs  = fromIntegral ptrs, 
                           nptrs = fromIntegral nptrs_really,
                           tipe  = fromIntegral cONSTR,
                           srtlen = fromIntegral conNo
#ifdef GHCI_TABLES_NEXT_TO_CODE
                         , code  = code
#endif
                        }
                 -- Make a piece of code to jump to "entry_label".
                 -- This is the only arch-dependent bit.
                 code = mkJumpToAddr entry_addr
             in
                 do addr <- malloc_exec (sizeOf itbl)
                    --putStrLn ("SIZE of itbl is " ++ show (sizeOf itbl))
                    --putStrLn ("# ptrs  of itbl is " ++ show ptrs)
                    --putStrLn ("# nptrs of itbl is " ++ show nptrs_really)
                    poke addr itbl
                    return (getName dcon, addr
#ifdef GHCI_TABLES_NEXT_TO_CODE
                                               `plusPtr` (2 * wORD_SIZE)
#endif
                           )


-- Make code which causes a jump to the given address.  This is the
-- only arch-dependent bit of the itbl story.  The returned list is
-- itblCodeLength elements (bytes) long.

-- For sparc_TARGET_ARCH, i386_TARGET_ARCH, etc.
#include "nativeGen/NCG.h"

itblCodeLength :: Int
itblCodeLength = length (mkJumpToAddr undefined)

mkJumpToAddr :: Ptr () -> [ItblCode]

ptrToInt (Ptr a#) = I# (addr2Int# a#)

#if sparc_TARGET_ARCH
-- After some consideration, we'll try this, where
-- 0x55555555 stands in for the address to jump to.
-- According to ghc/includes/MachRegs.h, %g3 is very
-- likely indeed to be baggable.
--
--   0000 07155555              sethi   %hi(0x55555555), %g3
--   0004 8610E155              or      %g3, %lo(0x55555555), %g3
--   0008 81C0C000              jmp     %g3
--   000c 01000000              nop

type ItblCode = Word32
mkJumpToAddr a
   = let w32 = fromIntegral (ptrToInt a)

         hi22, lo10 :: Word32 -> Word32
         lo10 x = x .&. 0x3FF
         hi22 x = (x `shiftR` 10) .&. 0x3FFFF

     in  [ 0x07000000 .|. (hi22 w32),
           0x8610E000 .|. (lo10 w32),
           0x81C0C000,
           0x01000000 ]

#elif powerpc_TARGET_ARCH
-- We'll use r12, for no particular reason.
-- 0xDEADBEEF stands for the adress:
-- 3D80DEAD lis r12,0xDEAD
-- 618CBEEF ori r12,r12,0xBEEF
-- 7D8903A6 mtctr r12
-- 4E800420 bctr

type ItblCode = Word32
mkJumpToAddr a =
    let w32 = fromIntegral (ptrToInt a)
	hi16 x = (x `shiftR` 16) .&. 0xFFFF
	lo16 x = x .&. 0xFFFF
    in	[
	0x3D800000 .|. hi16 w32,
	0x618C0000 .|. lo16 w32,
	0x7D8903A6, 0x4E800420
	]

#elif i386_TARGET_ARCH
-- Let the address to jump to be 0xWWXXYYZZ.
-- Generate   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
-- which is
-- B8 ZZ YY XX WW FF E0

type ItblCode = Word8
mkJumpToAddr a
   = let w32 = fromIntegral (ptrToInt a) :: Word32
         insnBytes :: [Word8]
         insnBytes
            = [0xB8, byte0 w32, byte1 w32, 
                     byte2 w32, byte3 w32, 
               0xFF, 0xE0]
     in
         insnBytes

#elif x86_64_TARGET_ARCH
-- Generates:
-- 	jmpq *.L1(%rip)
-- 	.align 8
-- .L1:	
-- 	.quad <addr>
--
-- We need a full 64-bit pointer (we can't assume the info table is
-- allocated in low memory).  Assuming the info pointer is aligned to
-- an 8-byte boundary, the addr will also be aligned.

type ItblCode = Word8
mkJumpToAddr a
   = let w64 = fromIntegral (ptrToInt a) :: Word64
         insnBytes :: [Word8]
         insnBytes
            = [0xff, 0x25, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
	       byte0 w64, byte1 w64, byte2 w64, byte3 w64,
	       byte4 w64, byte5 w64, byte6 w64, byte7 w64]
     in
         insnBytes

#elif alpha_TARGET_ARCH
type ItblCode = Word32
mkJumpToAddr a
    = [ 0xc3800000      -- br   at, .+4
      , 0xa79c000c      -- ldq  at, 12(at)
      , 0x6bfc0000      -- jmp  (at)    # with zero hint -- oh well
      , 0x47ff041f      -- nop
      , fromIntegral (w64 .&. 0x0000FFFF)
      , fromIntegral ((w64 `shiftR` 32) .&. 0x0000FFFF) ]
    where w64 = fromIntegral (ptrToInt a) :: Word64

#else
type ItblCode = Word32
mkJumpToAddr a
    = undefined
#endif


byte0, byte1, byte2, byte3, byte4, byte5, byte6, byte7
   :: (Integral w, Bits w) => w -> Word8
byte0 w = fromIntegral w
byte1 w = fromIntegral (w `shiftR` 8)
byte2 w = fromIntegral (w `shiftR` 16)
byte3 w = fromIntegral (w `shiftR` 24)
byte4 w = fromIntegral (w `shiftR` 32)
byte5 w = fromIntegral (w `shiftR` 40)
byte6 w = fromIntegral (w `shiftR` 48)
byte7 w = fromIntegral (w `shiftR` 56)


vecret_entry 0 = stg_interp_constr1_entry
vecret_entry 1 = stg_interp_constr2_entry
vecret_entry 2 = stg_interp_constr3_entry
vecret_entry 3 = stg_interp_constr4_entry
vecret_entry 4 = stg_interp_constr5_entry
vecret_entry 5 = stg_interp_constr6_entry
vecret_entry 6 = stg_interp_constr7_entry
vecret_entry 7 = stg_interp_constr8_entry

#ifndef __HADDOCK__
-- entry point for direct returns for created constr itbls
foreign import ccall "&stg_interp_constr_entry" stg_interp_constr_entry :: Ptr ()
-- and the 8 vectored ones
foreign import ccall "&stg_interp_constr1_entry" stg_interp_constr1_entry :: Ptr ()
foreign import ccall "&stg_interp_constr2_entry" stg_interp_constr2_entry :: Ptr ()
foreign import ccall "&stg_interp_constr3_entry" stg_interp_constr3_entry :: Ptr ()
foreign import ccall "&stg_interp_constr4_entry" stg_interp_constr4_entry :: Ptr ()
foreign import ccall "&stg_interp_constr5_entry" stg_interp_constr5_entry :: Ptr ()
foreign import ccall "&stg_interp_constr6_entry" stg_interp_constr6_entry :: Ptr ()
foreign import ccall "&stg_interp_constr7_entry" stg_interp_constr7_entry :: Ptr ()
foreign import ccall "&stg_interp_constr8_entry" stg_interp_constr8_entry :: Ptr ()
#endif




-- Ultra-minimalist version specially for constructors
#if SIZEOF_VOID_P == 8
type HalfWord = Word32
#else
type HalfWord = Word16
#endif

data StgInfoTable = StgInfoTable {
#ifndef GHCI_TABLES_NEXT_TO_CODE
   entry  :: Ptr (),
#endif
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: HalfWord,
   srtlen :: HalfWord
#ifdef GHCI_TABLES_NEXT_TO_CODE
 , code   :: [ItblCode]
#endif
  }

instance Storable StgInfoTable where

   sizeOf itbl 
      = sum
        [
#ifndef GHCI_TABLES_NEXT_TO_CODE
         fieldSz entry itbl,
#endif
         fieldSz ptrs itbl,
         fieldSz nptrs itbl,
         fieldSz tipe itbl,
         fieldSz srtlen itbl
#ifdef GHCI_TABLES_NEXT_TO_CODE
        ,fieldSz (head.code) itbl * itblCodeLength
#endif
        ]

   alignment itbl 
      = SIZEOF_VOID_P

   poke a0 itbl
      = runState (castPtr a0)
      $ do
#ifndef GHCI_TABLES_NEXT_TO_CODE
           store (entry  itbl)
#endif
           store (ptrs   itbl)
           store (nptrs  itbl)
           store (tipe   itbl)
           store (srtlen itbl)
#ifdef GHCI_TABLES_NEXT_TO_CODE
           sequence_ (map store (code itbl))
#endif

   peek a0
      = runState (castPtr a0)
      $ do
#ifndef GHCI_TABLES_NEXT_TO_CODE
           entry  <- load
#endif
           ptrs   <- load
           nptrs  <- load
           tipe   <- load
           srtlen <- load
#ifdef GHCI_TABLES_NEXT_TO_CODE
           code   <- sequence (replicate itblCodeLength load)
#endif
           return 
              StgInfoTable { 
#ifndef GHCI_TABLES_NEXT_TO_CODE
                 entry  = entry,
#endif
                 ptrs   = ptrs,
                 nptrs  = nptrs, 
                 tipe   = tipe,
                 srtlen = srtlen
#ifdef GHCI_TABLES_NEXT_TO_CODE
                ,code   = code
#endif
              }

fieldSz :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

newtype State s m a = State (s -> m (s, a))

instance Monad m => Monad (State s m) where
  return a      = State (\s -> return (s, a))
  State m >>= k = State (\s -> m s >>= \(s', a) -> case k a of State n -> n s')
  fail str      = State (\s -> fail str)

class (Monad m, Monad (t m)) => MonadT t m where
  lift :: m a -> t m a

instance Monad m => MonadT (State s) m where
  lift m        = State (\s -> m >>= \a -> return (s, a))

runState :: (Monad m) => s -> State s m a -> m a
runState s (State m) = m s >>= return . snd

type PtrIO = State (Ptr Word8) IO

advance :: Storable a => PtrIO (Ptr a)
advance = State adv where
    adv addr = case castPtr addr of { addrCast -> return
        (addr `plusPtr` sizeOfPointee addrCast, addrCast) }

sizeOfPointee :: (Storable a) => Ptr a -> Int
sizeOfPointee addr = sizeOf (typeHack addr)
    where typeHack = undefined :: Ptr a -> a

store :: Storable a => a -> PtrIO ()
store x = do addr <- advance
             lift (poke addr x)

load :: Storable a => PtrIO a
load = do addr <- advance
          lift (peek addr)

foreign import ccall unsafe "allocateExec"
  _allocateExec :: CUInt -> IO (Ptr a)

malloc_exec :: Int -> IO (Ptr a)
malloc_exec bytes = _allocateExec (fromIntegral bytes)

\end{code}
