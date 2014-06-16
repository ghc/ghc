%
% (c) The University of Glasgow 2000-2006
%
ByteCodeItbls: Generate infotables for interpreter-made bytecodes

\begin{code}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module ByteCodeItbls ( ItblEnv, ItblPtr(..), itblCode, mkITbls, peekItbl
                     , StgInfoTable(..)
                     ) where

#include "HsVersions.h"

import DynFlags
import Panic
import Platform
import Name             ( Name, getName )
import NameEnv
import DataCon          ( DataCon, dataConRepArgTys, dataConIdentity )
import TyCon            ( TyCon, tyConFamilySize, isDataTyCon, tyConDataCons )
import Type             ( flattenRepType, repType, typePrimRep )
import StgCmmLayout     ( mkVirtHeapOffsets )
import Util

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Maybe
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
               code' = mkJumpToAddr dflags entry_addr
               itbl  = StgInfoTable {
                           entry = if ghciTablesNextToCode
                                   then Nothing
                                   else Just entry_addr,
                           ptrs  = fromIntegral ptrs',
                           nptrs = fromIntegral nptrs_really,
                           tipe  = fromIntegral cONSTR,
                           srtlen = fromIntegral conNo,
                           code  = if ghciTablesNextToCode
                                   then Just code'
                                   else Nothing
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
-- only arch-dependent bit of the itbl story.

-- For sparc_TARGET_ARCH, i386_TARGET_ARCH, etc.
#include "nativeGen/NCG.h"

type ItblCodes = Either [Word8] [Word32]

ptrToInt :: Ptr a -> Int
ptrToInt (Ptr a#) = I# (addr2Int# a#)

mkJumpToAddr :: DynFlags -> Ptr () -> ItblCodes
mkJumpToAddr dflags a = case platformArch (targetPlatform dflags) of
    ArchSPARC ->
        -- After some consideration, we'll try this, where
        -- 0x55555555 stands in for the address to jump to.
        -- According to includes/rts/MachRegs.h, %g3 is very
        -- likely indeed to be baggable.
        --
        --   0000 07155555              sethi   %hi(0x55555555), %g3
        --   0004 8610E155              or      %g3, %lo(0x55555555), %g3
        --   0008 81C0C000              jmp     %g3
        --   000c 01000000              nop

        let w32 = fromIntegral (ptrToInt a)

            hi22, lo10 :: Word32 -> Word32
            lo10 x = x .&. 0x3FF
            hi22 x = (x `shiftR` 10) .&. 0x3FFFF

        in Right [ 0x07000000 .|. (hi22 w32),
                   0x8610E000 .|. (lo10 w32),
                   0x81C0C000,
                   0x01000000 ]

    ArchPPC ->
        -- We'll use r12, for no particular reason.
        -- 0xDEADBEEF stands for the address:
        -- 3D80DEAD lis r12,0xDEAD
        -- 618CBEEF ori r12,r12,0xBEEF
        -- 7D8903A6 mtctr r12
        -- 4E800420 bctr

        let w32 = fromIntegral (ptrToInt a)
            hi16 x = (x `shiftR` 16) .&. 0xFFFF
            lo16 x = x .&. 0xFFFF
        in Right [ 0x3D800000 .|. hi16 w32,
                   0x618C0000 .|. lo16 w32,
                   0x7D8903A6, 0x4E800420 ]

    ArchX86 ->
        -- Let the address to jump to be 0xWWXXYYZZ.
        -- Generate   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
        -- which is
        -- B8 ZZ YY XX WW FF E0

        let w32 = fromIntegral (ptrToInt a) :: Word32
            insnBytes :: [Word8]
            insnBytes
               = [0xB8, byte0 w32, byte1 w32,
                        byte2 w32, byte3 w32,
                  0xFF, 0xE0]
        in
            Left insnBytes

    ArchX86_64 ->
        -- Generates:
        --      jmpq *.L1(%rip)
        --      .align 8
        -- .L1:
        --      .quad <addr>
        --
        -- which looks like:
        --     8:   ff 25 02 00 00 00     jmpq   *0x2(%rip)      # 10 <f+0x10>
        -- with addr at 10.
        --
        -- We need a full 64-bit pointer (we can't assume the info table is
        -- allocated in low memory).  Assuming the info pointer is aligned to
        -- an 8-byte boundary, the addr will also be aligned.

        let w64 = fromIntegral (ptrToInt a) :: Word64
            insnBytes :: [Word8]
            insnBytes
               = [0xff, 0x25, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
                  byte0 w64, byte1 w64, byte2 w64, byte3 w64,
                  byte4 w64, byte5 w64, byte6 w64, byte7 w64]
        in
            Left insnBytes

    ArchAlpha ->
        let w64 = fromIntegral (ptrToInt a) :: Word64
        in Right [ 0xc3800000      -- br   at, .+4
                 , 0xa79c000c      -- ldq  at, 12(at)
                 , 0x6bfc0000      -- jmp  (at)    # with zero hint -- oh well
                 , 0x47ff041f      -- nop
                 , fromIntegral (w64 .&. 0x0000FFFF)
                 , fromIntegral ((w64 `shiftR` 32) .&. 0x0000FFFF) ]

    ArchARM { } ->
        -- Generates Thumb sequence,
        --      ldr r1, [pc, #0]
        --      bx r1
        --
        -- which looks like:
        --     00000000 <.addr-0x8>:
        --     0:       4900        ldr    r1, [pc]      ; 8 <.addr>
        --     4:       4708        bx     r1
        let w32 = fromIntegral (ptrToInt a) :: Word32
        in Left [ 0x49, 0x00
                , 0x47, 0x08
                , byte0 w32, byte1 w32, byte2 w32, byte3 w32]

    arch ->
        panic ("mkJumpToAddr not defined for " ++ show arch)

byte0, byte1, byte2, byte3 :: (Integral w, Bits w) => w -> Word8
byte0 w = fromIntegral w
byte1 w = fromIntegral (w `shiftR` 8)
byte2 w = fromIntegral (w `shiftR` 16)
byte3 w = fromIntegral (w `shiftR` 24)
byte4, byte5, byte6, byte7 :: (Integral w, Bits w) => w -> Word8
byte4 w = fromIntegral (w `shiftR` 32)
byte5 w = fromIntegral (w `shiftR` 40)
byte6 w = fromIntegral (w `shiftR` 48)
byte7 w = fromIntegral (w `shiftR` 56)

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

sizeOfConItbl :: DynFlags -> StgConInfoTable -> Int
sizeOfConItbl dflags conInfoTable
      = sum [ fieldSz conDesc conInfoTable
            , sizeOfItbl dflags (infoTable conInfoTable) ]

pokeConItbl :: DynFlags -> Ptr StgConInfoTable -> Ptr StgConInfoTable -> StgConInfoTable
            -> IO ()
pokeConItbl dflags wr_ptr ex_ptr itbl
      = flip evalStateT (castPtr wr_ptr) $ do
           when ghciTablesNextToCode $
               store (conDesc itbl `minusPtr` (ex_ptr `plusPtr` conInfoTableSizeB dflags))
           store' (sizeOfItbl dflags) (pokeItbl dflags) (infoTable itbl)
           unless ghciTablesNextToCode $ store (conDesc itbl)

data StgInfoTable = StgInfoTable {
   entry  :: Maybe (Ptr ()), -- Just <=> not ghciTablesNextToCode
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: HalfWord,
   srtlen :: HalfWord,
   code   :: Maybe ItblCodes -- Just <=> ghciTablesNextToCode
  }

sizeOfItbl :: DynFlags -> StgInfoTable -> Int
sizeOfItbl dflags itbl
      = sum
        [
         if ghciTablesNextToCode then 0 else fieldSz (fromJust . entry) itbl,
         fieldSz ptrs itbl,
         fieldSz nptrs itbl,
         fieldSz tipe itbl,
         fieldSz srtlen itbl,
         if ghciTablesNextToCode then case mkJumpToAddr dflags undefined of
                                      Left  xs -> sizeOf (head xs) * length xs
                                      Right xs -> sizeOf (head xs) * length xs
                                 else 0
        ]

pokeItbl :: DynFlags -> Ptr StgInfoTable -> StgInfoTable -> IO ()
pokeItbl _ a0 itbl
      = flip evalStateT (castPtr a0)
      $ do
           case entry itbl of
               Nothing -> return ()
               Just e  -> store e
           store (ptrs   itbl)
           store (nptrs  itbl)
           store (tipe   itbl)
           store (srtlen itbl)
           case code itbl of
               Nothing -> return ()
               Just (Left  xs) -> mapM_ store xs
               Just (Right xs) -> mapM_ store xs

peekItbl :: DynFlags -> Ptr StgInfoTable -> IO StgInfoTable
peekItbl dflags a0
      = flip evalStateT (castPtr a0)
      $ do
           entry'  <- if ghciTablesNextToCode
                      then return Nothing
                      else liftM Just load
           ptrs'   <- load
           nptrs'  <- load
           tipe'   <- load
           srtlen' <- load
           code'   <- if ghciTablesNextToCode
                      then liftM Just $ case mkJumpToAddr dflags undefined of
                                        Left xs ->
                                            liftM Left $ sequence (replicate (length xs) load)
                                        Right xs ->
                                            liftM Right $ sequence (replicate (length xs) load)
                      else return Nothing
           return
              StgInfoTable {
                 entry  = entry',
                 ptrs   = ptrs',
                 nptrs  = nptrs',
                 tipe   = tipe',
                 srtlen = srtlen'
                ,code   = code'
              }

fieldSz :: Storable b => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

type PtrIO = StateT (Ptr Word8) IO

advance :: Storable a => PtrIO (Ptr a)
advance = advance' sizeOf

advance' :: (a -> Int) -> PtrIO (Ptr a)
advance' fSizeOf = state adv
    where adv addr = case castPtr addr of
                     addrCast ->
                         (addrCast,
                          addr `plusPtr` sizeOfPointee fSizeOf addrCast)

sizeOfPointee :: (a -> Int) -> Ptr a -> Int
sizeOfPointee fSizeOf addr = fSizeOf (typeHack addr)
    where typeHack = undefined :: Ptr a -> a

store :: Storable a => a -> PtrIO ()
store = store' sizeOf poke

store' :: (a -> Int) -> (Ptr a -> a -> IO ()) -> a -> PtrIO ()
store' fSizeOf fPoke x = do addr <- advance' fSizeOf
                            lift (fPoke addr x)

load :: Storable a => PtrIO a
load = do addr <- advance
          lift (peek addr)

newExecConItbl :: DynFlags -> StgConInfoTable -> IO (FunPtr ())
newExecConItbl dflags obj
   = alloca $ \pcode -> do
        let sz = fromIntegral (sizeOfConItbl dflags obj)
        wr_ptr <- _allocateExec sz pcode
        ex_ptr <- peek pcode
        pokeConItbl dflags wr_ptr ex_ptr obj
        _flushExec sz ex_ptr -- Cache flush (if needed)
        return (castPtrToFunPtr ex_ptr)

foreign import ccall unsafe "allocateExec"
  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)

foreign import ccall unsafe "flushExec"
  _flushExec :: CUInt -> Ptr a -> IO ()
\end{code}
