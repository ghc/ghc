%
% (c) The University of Glasgow 2000-2006
%
ByteCodeItbls: Generate infotables for interpreter-made bytecodes

\begin{code}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

#ifndef GHCI_TABLES_NEXT_TO_CODE
{-# OPTIONS_GHC -Wwarn #-}
-- There are lots of warnings when GHCI_TABLES_NEXT_TO_CODE is off.
-- It would be nice to fix this properly, but for now we turn -Werror
-- off.
#endif

module ByteCodeItbls ( ItblEnv, ItblPtr(..), itblCode, mkITbls
                     , StgInfoTable(..)
                     ) where

#include "HsVersions.h"

import DynFlags
import Name             ( Name, getName )
import NameEnv
import DataCon          ( DataCon, dataConRepArgTys, dataConIdentity )
import TyCon            ( TyCon, tyConFamilySize, isDataTyCon, tyConDataCons )
import Type             ( flattenRepType, repType, typePrimRep )
import StgCmmLayout     ( mkVirtHeapOffsets )
import Util

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Foreign
import Foreign.C

import GHC.Exts         ( Int(I#), addr2Int# )
import GHC.Ptr          ( Ptr(..) )
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Manufacturing of info tables for DataCons}
%*                                                                      *
%************************************************************************

\begin{code}
newtype ItblPtr = ItblPtr (Ptr ()) deriving Show

itblCode :: DynFlags -> ItblPtr -> Ptr ()
itblCode dflags (ItblPtr ptr)
 | ghciTablesNextToCode = castPtr ptr `plusPtr` conInfoTableSizeB dflags
 | otherwise            = castPtr ptr

-- XXX bogus
conInfoTableSizeB :: DynFlags -> Int
conInfoTableSizeB dflags = 3 * wORD_SIZE dflags

type ItblEnv = NameEnv (Name, ItblPtr)
        -- We need the Name in the range so we know which
        -- elements to filter out when unloading a module

mkItblEnv :: [(Name,ItblPtr)] -> ItblEnv
mkItblEnv pairs = mkNameEnv [(n, (n,p)) | (n,p) <- pairs]


-- Make info tables for the data decls in this module
mkITbls :: DynFlags -> [TyCon] -> IO ItblEnv
mkITbls _ [] = return emptyNameEnv
mkITbls dflags (tc:tcs) = do itbls  <- mkITbl dflags tc
                             itbls2 <- mkITbls dflags tcs
                             return (itbls `plusNameEnv` itbls2)

mkITbl :: DynFlags -> TyCon -> IO ItblEnv
mkITbl dflags tc
   | not (isDataTyCon tc) 
   = return emptyNameEnv
   | dcs `lengthIs` n -- paranoia; this is an assertion.
   = make_constr_itbls dflags dcs
     where
        dcs = tyConDataCons tc
        n   = tyConFamilySize tc

mkITbl _ _ = error "Unmatched patter in mkITbl: assertion failed!"

#include "../includes/rts/storage/ClosureTypes.h"
cONSTR :: Int   -- Defined in ClosureTypes.h
cONSTR = CONSTR 

-- Assumes constructors are numbered from zero, not one
make_constr_itbls :: DynFlags -> [DataCon] -> IO ItblEnv
make_constr_itbls dflags cons
   = do is <- mapM mk_dirret_itbl (zip cons [0..])
        return (mkItblEnv is)
     where
        mk_dirret_itbl (dcon, conNo)
           = mk_itbl dcon conNo stg_interp_constr_entry

        mk_itbl :: DataCon -> Int -> Ptr () -> IO (Name,ItblPtr)
        mk_itbl dcon conNo entry_addr = do
           let rep_args = [ (typePrimRep rep_arg,rep_arg) | arg <- dataConRepArgTys dcon, rep_arg <- flattenRepType (repType arg) ]
               (tot_wds, ptr_wds, _) = mkVirtHeapOffsets dflags False{-not a THUNK-} rep_args

               ptrs'  = ptr_wds
               nptrs' = tot_wds - ptr_wds
               nptrs_really
                  | ptrs' + nptrs' >= mIN_PAYLOAD_SIZE dflags = nptrs'
                  | otherwise = mIN_PAYLOAD_SIZE dflags - ptrs'
               code' = mkJumpToAddr entry_addr
               itbl  = StgInfoTable {
#ifndef GHCI_TABLES_NEXT_TO_CODE
                           entry = entry_addr,
#endif
                           ptrs  = fromIntegral ptrs', 
                           nptrs = fromIntegral nptrs_really,
                           tipe  = fromIntegral cONSTR,
                           srtlen = fromIntegral conNo
#ifdef GHCI_TABLES_NEXT_TO_CODE
                         , code  = code'
#endif
                        }
           qNameCString <- newArray0 0 $ dataConIdentity dcon 
           let conInfoTbl = StgConInfoTable {
                                 conDesc = qNameCString,
                                 infoTable = itbl
                            }
               -- Make a piece of code to jump to "entry_label".
               -- This is the only arch-dependent bit.
           addrCon <- newExecConItbl dflags conInfoTbl
                    --putStrLn ("SIZE of itbl is " ++ show (sizeOf itbl))
                    --putStrLn ("# ptrs  of itbl is " ++ show ptrs)
                    --putStrLn ("# nptrs of itbl is " ++ show nptrs_really)
           return (getName dcon, ItblPtr (castFunPtrToPtr addrCon))


-- Make code which causes a jump to the given address.  This is the
-- only arch-dependent bit of the itbl story.  The returned list is
-- itblCodeLength elements (bytes) long.

-- For sparc_TARGET_ARCH, i386_TARGET_ARCH, etc.
#include "nativeGen/NCG.h"

itblCodeLength :: Int
itblCodeLength = length (mkJumpToAddr undefined)

mkJumpToAddr :: Ptr () -> [ItblCode]

ptrToInt :: Ptr a -> Int
ptrToInt (Ptr a#) = I# (addr2Int# a#)

#if sparc_TARGET_ARCH
-- After some consideration, we'll try this, where
-- 0x55555555 stands in for the address to jump to.
-- According to includes/rts/MachRegs.h, %g3 is very
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
-- 0xDEADBEEF stands for the address:
-- 3D80DEAD lis r12,0xDEAD
-- 618CBEEF ori r12,r12,0xBEEF
-- 7D8903A6 mtctr r12
-- 4E800420 bctr

type ItblCode = Word32
mkJumpToAddr a =
    let w32 = fromIntegral (ptrToInt a)
        hi16 x = (x `shiftR` 16) .&. 0xFFFF
        lo16 x = x .&. 0xFFFF
    in  [
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
--      jmpq *.L1(%rip)
--      .align 8
-- .L1: 
--      .quad <addr>
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

#if defined(i386_TARGET_ARCH) || defined(x86_64_TARGET_ARCH)
byte0, byte1, byte2, byte3 :: (Integral w, Bits w) => w -> Word8
byte0 w = fromIntegral w
byte1 w = fromIntegral (w `shiftR` 8)
byte2 w = fromIntegral (w `shiftR` 16)
byte3 w = fromIntegral (w `shiftR` 24)
#endif

#if defined(x86_64_TARGET_ARCH)
byte4, byte5, byte6, byte7 :: (Integral w, Bits w) => w -> Word8
byte4 w = fromIntegral (w `shiftR` 32)
byte5 w = fromIntegral (w `shiftR` 40)
byte6 w = fromIntegral (w `shiftR` 48)
byte7 w = fromIntegral (w `shiftR` 56)
#endif

-- entry point for direct returns for created constr itbls
foreign import ccall "&stg_interp_constr_entry" stg_interp_constr_entry :: Ptr ()




-- Ultra-minimalist version specially for constructors
#if SIZEOF_VOID_P == 8
type HalfWord = Word32
#else
type HalfWord = Word16
#endif

data StgConInfoTable = StgConInfoTable {
   conDesc   :: Ptr Word8,
   infoTable :: StgInfoTable
}

sizeOfConItbl :: StgConInfoTable -> Int
sizeOfConItbl conInfoTable
      = sum [ fieldSz conDesc conInfoTable
            , fieldSz infoTable conInfoTable ]

pokeConItbl :: DynFlags -> Ptr StgConInfoTable -> Ptr StgConInfoTable -> StgConInfoTable
            -> IO ()
pokeConItbl dflags wr_ptr ex_ptr itbl
      = flip evalStateT (castPtr wr_ptr) $ do
#ifdef GHCI_TABLES_NEXT_TO_CODE
           store (conDesc itbl `minusPtr` (ex_ptr `plusPtr` conInfoTableSizeB dflags))
#endif
           store (infoTable itbl)
#ifndef GHCI_TABLES_NEXT_TO_CODE
           store (conDesc itbl)
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

   alignment _ 
      = SIZEOF_VOID_P

   poke a0 itbl
      = flip evalStateT (castPtr a0)
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
      = flip evalStateT (castPtr a0)
      $ do
#ifndef GHCI_TABLES_NEXT_TO_CODE
           entry'  <- load
#endif
           ptrs'   <- load
           nptrs'  <- load
           tipe'   <- load
           srtlen' <- load
#ifdef GHCI_TABLES_NEXT_TO_CODE
           code'   <- sequence (replicate itblCodeLength load)
#endif
           return 
              StgInfoTable { 
#ifndef GHCI_TABLES_NEXT_TO_CODE
                 entry  = entry',
#endif
                 ptrs   = ptrs',
                 nptrs  = nptrs',
                 tipe   = tipe',
                 srtlen = srtlen'
#ifdef GHCI_TABLES_NEXT_TO_CODE
                ,code   = code'
#endif
              }

fieldSz :: Storable b => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

type PtrIO = StateT (Ptr Word8) IO

advance :: Storable a => PtrIO (Ptr a)
advance = state adv
    where adv addr = case castPtr addr of
                     addrCast ->
                         (addrCast, addr `plusPtr` sizeOfPointee addrCast)

sizeOfPointee :: (Storable a) => Ptr a -> Int
sizeOfPointee addr = sizeOf (typeHack addr)
    where typeHack = undefined :: Ptr a -> a

store :: Storable a => a -> PtrIO ()
store x = do addr <- advance
             lift (poke addr x)

load :: Storable a => PtrIO a
load = do addr <- advance
          lift (peek addr)


newExecConItbl :: DynFlags -> StgConInfoTable -> IO (FunPtr ())
newExecConItbl dflags obj
   = alloca $ \pcode -> do
        wr_ptr <- _allocateExec (fromIntegral (sizeOfConItbl obj)) pcode
        ex_ptr <- peek pcode
        pokeConItbl dflags wr_ptr ex_ptr obj
        return (castPtrToFunPtr ex_ptr)

foreign import ccall unsafe "allocateExec"
  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)  
\end{code}
