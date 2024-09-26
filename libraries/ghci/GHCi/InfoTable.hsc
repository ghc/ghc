{-# LANGUAGE CPP, MagicHash, ScopedTypeVariables #-}

-- Get definitions for the structs, constants & config etc.
#include "Rts.h"

-- |
-- Run-time info table support.  This module provides support for
-- creating and reading info tables /in the running program/.
-- We use the RTS data structures directly via hsc2hs.
--
module GHCi.InfoTable
  (
    mkConInfoTable
  ) where

import Prelude hiding (fail) -- See note [Why do we import Prelude here?]

import Foreign
import Foreign.C
import GHC.Ptr
import GHC.Exts
import GHC.Exts.Heap
import Data.ByteString (ByteString)
import Control.Monad.Fail
import qualified Data.ByteString as BS
import GHC.Platform.Host (hostPlatformArch)
import GHC.Platform.ArchOS

-- NOTE: Must return a pointer acceptable for use in the header of a closure.
-- If tables_next_to_code is enabled, then it must point the 'code' field.
-- Otherwise, it should point to the start of the StgInfoTable.
mkConInfoTable
   :: Bool    -- TABLES_NEXT_TO_CODE
   -> Int     -- ptr words
   -> Int     -- non-ptr words
   -> Int     -- constr tag
   -> Int     -- pointer tag
   -> ByteString  -- con desc
   -> IO (Ptr StgInfoTable)
      -- resulting info table is allocated with allocateExecPage(), and
      -- should be freed with freeExecPage().

mkConInfoTable tables_next_to_code ptr_words nonptr_words tag ptrtag con_desc = do
  let entry_addr = interpConstrEntry !! ptrtag
  code' <- if tables_next_to_code
    then Just <$> mkJumpToAddr entry_addr
    else pure Nothing
  let
     itbl  = StgInfoTable {
                 entry = if tables_next_to_code
                         then Nothing
                         else Just entry_addr,
                 ptrs  = fromIntegral ptr_words,
                 nptrs = fromIntegral nonptr_words,
                 tipe  = CONSTR,
                 srtlen = fromIntegral tag,
                 code  = code'
              }
  castFunPtrToPtr <$> newExecConItbl tables_next_to_code itbl con_desc


-- -----------------------------------------------------------------------------
-- Building machine code fragments for a constructor's entry code

funPtrToInt :: FunPtr a -> Int
funPtrToInt (FunPtr a) = I## (addr2Int## a)

mkJumpToAddr :: MonadFail m => EntryFunPtr-> m ItblCodes
mkJumpToAddr a = case hostPlatformArch of
    ArchPPC -> pure $
        -- We'll use r12, for no particular reason.
        -- 0xDEADBEEF stands for the address:
        -- 3D80DEAD lis r12,0xDEAD
        -- 618CBEEF ori r12,r12,0xBEEF
        -- 7D8903A6 mtctr r12
        -- 4E800420 bctr

        let w32 = fromIntegral (funPtrToInt a)
            hi16 x = (x `shiftR` 16) .&. 0xFFFF
            lo16 x = x .&. 0xFFFF
        in Right [ 0x3D800000 .|. hi16 w32,
                   0x618C0000 .|. lo16 w32,
                   0x7D8903A6, 0x4E800420 ]

    ArchX86 -> pure $
        -- Let the address to jump to be 0xWWXXYYZZ.
        -- Generate   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
        -- which is
        -- B8 ZZ YY XX WW FF E0

        let w32 = fromIntegral (funPtrToInt a) :: Word32
            insnBytes :: [Word8]
            insnBytes
               = [0xB8, byte0 w32, byte1 w32,
                        byte2 w32, byte3 w32,
                  0xFF, 0xE0]
        in
            Left insnBytes

    ArchX86_64 -> pure $
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

        let w64 = fromIntegral (funPtrToInt a) :: Word64
            insnBytes :: [Word8]
            insnBytes
               = [0xff, 0x25, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
                  byte0 w64, byte1 w64, byte2 w64, byte3 w64,
                  byte4 w64, byte5 w64, byte6 w64, byte7 w64]
        in
            Left insnBytes

    ArchAlpha -> pure $
        let w64 = fromIntegral (funPtrToInt a) :: Word64
        in Right [ 0xc3800000      -- br   at, .+4
                 , 0xa79c000c      -- ldq  at, 12(at)
                 , 0x6bfc0000      -- jmp  (at)    # with zero hint -- oh well
                 , 0x47ff041f      -- nop
                 , fromIntegral (w64 .&. 0x0000FFFF)
                 , fromIntegral ((w64 `shiftR` 32) .&. 0x0000FFFF) ]

    ArchARM {} -> pure $
        -- Generates Arm sequence,
        --      ldr r1, [pc, #0]
        --      bx r1
        --
        -- which looks like:
        --     00000000 <.addr-0x8>:
        --     0:       00109fe5    ldr    r1, [pc]      ; 8 <.addr>
        --     4:       11ff2fe1    bx     r1
        let w32 = fromIntegral (funPtrToInt a) :: Word32
        in Left [ 0x00, 0x10, 0x9f, 0xe5
                , 0x11, 0xff, 0x2f, 0xe1
                , byte0 w32, byte1 w32, byte2 w32, byte3 w32]

    ArchAArch64 {} -> pure $
        -- Generates:
        --
        --      ldr     x1, label
        --      br      x1
        -- label:
        --      .quad <addr>
        --
        -- which looks like:
        --     0:       58000041        ldr     x1, <label>
        --     4:       d61f0020        br      x1
       let w64 = fromIntegral (funPtrToInt a) :: Word64
       in Right [ 0x58000041
                , 0xd61f0020
                , fromIntegral w64
                , fromIntegral (w64 `shiftR` 32) ]

    ArchPPC_64 ELF_V1 -> pure $
        -- We use the compiler's register r12 to read the function
        -- descriptor and the linker's register r11 as a temporary
        -- register to hold the function entry point.
        -- In the medium code model the function descriptor
        -- is located in the first two gigabytes, i.e. the address
        -- of the function pointer is a non-negative 32 bit number.
        -- 0x0EADBEEF stands for the address of the function pointer:
        --    0:   3d 80 0e ad     lis     r12,0x0EAD
        --    4:   61 8c be ef     ori     r12,r12,0xBEEF
        --    8:   e9 6c 00 00     ld      r11,0(r12)
        --    c:   e8 4c 00 08     ld      r2,8(r12)
        --   10:   7d 69 03 a6     mtctr   r11
        --   14:   e9 6c 00 10     ld      r11,16(r12)
        --   18:   4e 80 04 20     bctr
       let  w32 = fromIntegral (funPtrToInt a)
            hi16 x = (x `shiftR` 16) .&. 0xFFFF
            lo16 x = x .&. 0xFFFF
       in Right [ 0x3D800000 .|. hi16 w32,
                  0x618C0000 .|. lo16 w32,
                  0xE96C0000,
                  0xE84C0008,
                  0x7D6903A6,
                  0xE96C0010,
                  0x4E800420]

    ArchPPC_64 ELF_V2 -> pure $
        -- The ABI requires r12 to point to the function's entry point.
        -- We use the medium code model where code resides in the first
        -- two gigabytes, so loading a non-negative32 bit address
        -- with lis followed by ori is fine.
        -- 0x0EADBEEF stands for the address:
        -- 3D800EAD lis r12,0x0EAD
        -- 618CBEEF ori r12,r12,0xBEEF
        -- 7D8903A6 mtctr r12
        -- 4E800420 bctr

        let w32 = fromIntegral (funPtrToInt a)
            hi16 x = (x `shiftR` 16) .&. 0xFFFF
            lo16 x = x .&. 0xFFFF
        in Right [ 0x3D800000 .|. hi16 w32,
                   0x618C0000 .|. lo16 w32,
                   0x7D8903A6, 0x4E800420 ]

    ArchS390X -> pure $
        -- Let 0xAABBCCDDEEFFGGHH be the address to jump to.
        -- The following code loads the address into scratch
        -- register r1 and jumps to it.
        --
        --    0:   C0 1E AA BB CC DD       llihf   %r1,0xAABBCCDD
        --    6:   C0 19 EE FF GG HH       iilf    %r1,0xEEFFGGHH
        --   12:   07 F1                   br      %r1

        let w64 = fromIntegral (funPtrToInt a) :: Word64
        in Left [ 0xC0, 0x1E, byte7 w64, byte6 w64, byte5 w64, byte4 w64,
                  0xC0, 0x19, byte3 w64, byte2 w64, byte1 w64, byte0 w64,
                  0x07, 0xF1 ]

    ArchRISCV64 -> pure $
        let w64 = fromIntegral (funPtrToInt a) :: Word64
        in Right [ 0x00000297          -- auipc t0,0
                 , 0x01053283          -- ld    t0,16(t0)
                 , 0x00028067          -- jr    t0
                 , 0x00000013          -- nop
                 , fromIntegral w64
                 , fromIntegral (w64 `shiftR` 32) ]

    ArchLoongArch64 -> pure $
        let w64 = fromIntegral (funPtrToInt a) :: Word64
        in Right [ 0x1c00000c          -- pcaddu12i $t0,0
                 , 0x28c0418c          -- ld.d      $t0,$t0,16
                 , 0x4c000180          -- jr        $t0
                 , 0x03400000          -- nop
                 , fromIntegral w64
                 , fromIntegral (w64 `shiftR` 32) ]

    arch ->
      -- The arch isn't supported. You either need to add your architecture as a
      -- distinct case, or use non-TABLES_NEXT_TO_CODE mode.
      fail $ "mkJumpToAddr: arch is not supported with TABLES_NEXT_TO_CODE ("
             ++ show arch ++ ")"

byte0 :: (Integral w) => w -> Word8
byte0 w = fromIntegral w

byte1, byte2, byte3, byte4, byte5, byte6, byte7
       :: (Integral w, Bits w) => w -> Word8
byte1 w = fromIntegral (w `shiftR` 8)
byte2 w = fromIntegral (w `shiftR` 16)
byte3 w = fromIntegral (w `shiftR` 24)
byte4 w = fromIntegral (w `shiftR` 32)
byte5 w = fromIntegral (w `shiftR` 40)
byte6 w = fromIntegral (w `shiftR` 48)
byte7 w = fromIntegral (w `shiftR` 56)


-- -----------------------------------------------------------------------------
-- read & write intfo tables

-- entry point for direct returns for created constr itbls
foreign import ccall "&stg_interp_constr1_entry" stg_interp_constr1_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr2_entry" stg_interp_constr2_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr3_entry" stg_interp_constr3_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr4_entry" stg_interp_constr4_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr5_entry" stg_interp_constr5_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr6_entry" stg_interp_constr6_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr7_entry" stg_interp_constr7_entry :: EntryFunPtr

interpConstrEntry :: [EntryFunPtr]
interpConstrEntry = [ error "pointer tag 0"
                    , stg_interp_constr1_entry
                    , stg_interp_constr2_entry
                    , stg_interp_constr3_entry
                    , stg_interp_constr4_entry
                    , stg_interp_constr5_entry
                    , stg_interp_constr6_entry
                    , stg_interp_constr7_entry ]

data StgConInfoTable = StgConInfoTable {
   conDesc   :: Ptr Word8,
   infoTable :: StgInfoTable
}


pokeConItbl
  :: Bool -> Ptr StgConInfoTable -> Ptr StgConInfoTable -> StgConInfoTable
  -> IO ()
pokeConItbl tables_next_to_code wr_ptr _ex_ptr itbl = do
  if tables_next_to_code
    then do
      -- Write the offset to the con_desc from the end of the standard InfoTable
      -- at the first byte.
      let con_desc_offset = conDesc itbl `minusPtr` (_ex_ptr `plusPtr` conInfoTableSizeB)
      (#poke StgConInfoTable, con_desc) wr_ptr con_desc_offset
    else do
      -- Write the con_desc address after the end of the info table.
      -- Use itblSize because CPP will not pick up PROFILING when calculating
      -- the offset.
      pokeByteOff wr_ptr itblSize (conDesc itbl)
  pokeItbl (wr_ptr `plusPtr` (#offset StgConInfoTable, i)) (infoTable itbl)

sizeOfEntryCode :: MonadFail m => Bool -> m Int
sizeOfEntryCode tables_next_to_code
  | not tables_next_to_code = pure 0
  | otherwise = do
     code' <- mkJumpToAddr undefined
     pure $ case code' of
       Left  (xs :: [Word8])  -> sizeOf (undefined :: Word8)  * length xs
       Right (xs :: [Word32]) -> sizeOf (undefined :: Word32) * length xs

-- Note: Must return proper pointer for use in a closure
newExecConItbl :: Bool -> StgInfoTable -> ByteString -> IO (FunPtr ())
newExecConItbl tables_next_to_code obj con_desc = do
    sz0 <- sizeOfEntryCode tables_next_to_code
    let lcon_desc = BS.length con_desc + 1{- null terminator -}
        -- SCARY
        -- This size represents the number of bytes in an StgConInfoTable.
        sz = fromIntegral $ conInfoTableSizeB + sz0
            -- Note: we need to allocate the conDesc string next to the info
            -- table, because on a 64-bit platform we reference this string
            -- with a 32-bit offset relative to the info table, so if we
            -- allocated the string separately it might be out of range.

        -- Just use plain malloc on platforms without TNTC, since we
        -- don't need to allocate executable memory anyway. This is
        -- much faster than mmap(), and is crucial for wasm since it
        -- doesn't support mmap() at all, not to mention executable
        -- memory.
        fill_exec_buffer = if tables_next_to_code
          then fillExecBuffer
          else \n cont -> do
            p <- mallocBytes $ fromIntegral n
            cont p p
            pure p

    ex_ptr <- fill_exec_buffer (sz + fromIntegral lcon_desc) $ \wr_ptr ex_ptr -> do
        let cinfo = StgConInfoTable { conDesc = ex_ptr `plusPtr` fromIntegral sz
                                    , infoTable = obj }
        pokeConItbl tables_next_to_code wr_ptr ex_ptr cinfo
        BS.useAsCStringLen con_desc $ \(src, len) ->
            copyBytes (castPtr wr_ptr `plusPtr` fromIntegral sz) src len
        let null_off = fromIntegral sz + fromIntegral (BS.length con_desc)
        poke (castPtr wr_ptr `plusPtr` null_off) (0 :: Word8)

    pure $ if tables_next_to_code
      then castPtrToFunPtr $ ex_ptr `plusPtr` conInfoTableSizeB
      else castPtrToFunPtr ex_ptr

-- | Allocate a buffer of a given size, use the given action to fill it with
-- data, and mark it as executable. The action is given a writable pointer and
-- the executable pointer. Returns a pointer to the executable code.
fillExecBuffer :: CSize -> (Ptr a -> Ptr a -> IO ()) -> IO (Ptr a)

#if MIN_VERSION_rts(1,0,2)

data ExecPage

foreign import ccall unsafe "allocateExecPage"
  _allocateExecPage :: IO (Ptr ExecPage)

foreign import ccall unsafe "freezeExecPage"
  _freezeExecPage :: Ptr ExecPage -> IO ()

fillExecBuffer sz cont
    -- we can only allocate single pages. This assumes a 4k page size which
    -- isn't strictly correct but is a reasonable conservative lower bound.
  | sz > 4096 = fail "withExecBuffer: Too large"
  | otherwise = do
        pg <- _allocateExecPage
        cont (castPtr pg) (castPtr pg)
        _freezeExecPage pg
        return (castPtr pg)

#elif MIN_VERSION_rts(1,0,1)

foreign import ccall unsafe "allocateExec"
  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)

foreign import ccall unsafe "flushExec"
  _flushExec :: CUInt -> Ptr a -> IO ()

fillExecBuffer sz cont = alloca $ \pcode -> do
    wr_ptr <- _allocateExec (fromIntegral sz) pcode
    ex_ptr <- peek pcode
    cont wr_ptr ex_ptr
    _flushExec (fromIntegral sz) ex_ptr -- Cache flush (if needed)
    return (ex_ptr)

#else

#error Sorry, rts versions <= 1.0 are not supported

#endif

-- -----------------------------------------------------------------------------
-- Constants and config

wORD_SIZE :: Int
wORD_SIZE = (#const SIZEOF_HSINT)

conInfoTableSizeB :: Int
conInfoTableSizeB = wORD_SIZE + itblSize
