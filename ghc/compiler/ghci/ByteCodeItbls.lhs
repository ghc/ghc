%
% (c) The University of Glasgow 2000
%
\section[ByteCodeItbls]{Generate infotables for interpreter-made bytecodes}

\begin{code}
module ByteCodeItbls ( ItblEnv, ItblPtr, mkITbls ) where

#include "HsVersions.h"

import Name		( Name, getName )
import FiniteMap	( FiniteMap, listToFM, emptyFM, plusFM )
import Type		( typePrimRep )
import DataCon		( DataCon, dataConRepArgTys )
import TyCon		( TyCon, tyConFamilySize, isDataTyCon, tyConDataCons )
import Constants	( mIN_SIZE_NonUpdHeapObject )
import ClosureInfo	( mkVirtHeapOffsets )
import FastString	( FastString(..) )

import Foreign		( Storable(..), Word8, Word16, Word32, Ptr(..), 
			  malloc, castPtr, plusPtr, Addr )
import Addr		( addrToInt )
import Bits		( Bits(..), shiftR )

import PrelBase		( Int(..) )
import PrelIOBase	( IO(..) )

\end{code}

%************************************************************************
%*									*
\subsection{Manufacturing of info tables for DataCons}
%*									*
%************************************************************************

\begin{code}

type ItblPtr = Ptr StgInfoTable
type ItblEnv = FiniteMap Name ItblPtr


-- Make info tables for the data decls in this module
mkITbls :: [TyCon] -> IO ItblEnv
mkITbls [] = return emptyFM
mkITbls (tc:tcs) = do itbls  <- mkITbl tc
                      itbls2 <- mkITbls tcs
                      return (itbls `plusFM` itbls2)

mkITbl :: TyCon -> IO ItblEnv
mkITbl tc
   | not (isDataTyCon tc) 
   = return emptyFM
   | n == length dcs  -- paranoia; this is an assertion.
   = make_constr_itbls dcs
     where
        dcs = tyConDataCons tc
        n   = tyConFamilySize tc

cONSTR :: Int
cONSTR = 1  -- as defined in ghc/includes/ClosureTypes.h

-- Assumes constructors are numbered from zero, not one
make_constr_itbls :: [DataCon] -> IO ItblEnv
make_constr_itbls cons
   | length cons <= 8
   = do is <- mapM mk_vecret_itbl (zip cons [0..])
	return (listToFM is)
   | otherwise
   = do is <- mapM mk_dirret_itbl (zip cons [0..])
	return (listToFM is)
     where
        mk_vecret_itbl (dcon, conNo)
           = mk_itbl dcon conNo (vecret_entry conNo)
        mk_dirret_itbl (dcon, conNo)
           = mk_itbl dcon conNo stg_interp_constr_entry

        mk_itbl :: DataCon -> Int -> Addr -> IO (Name,ItblPtr)
        mk_itbl dcon conNo entry_addr
           = let (tot_wds, ptr_wds, _) 
                    = mkVirtHeapOffsets typePrimRep (dataConRepArgTys dcon)
                 ptrs  = ptr_wds
                 nptrs = tot_wds - ptr_wds
                 nptrs_really
                    | ptrs + nptrs >= mIN_SIZE_NonUpdHeapObject = nptrs
                    | otherwise = mIN_SIZE_NonUpdHeapObject - ptrs
                 itbl  = StgInfoTable {
                           ptrs  = fromIntegral ptrs, 
                           nptrs = fromIntegral nptrs_really,
                           tipe  = fromIntegral cONSTR,
                           srtlen = fromIntegral conNo,
                           code0 = fromIntegral code0, code1 = fromIntegral code1,
                           code2 = fromIntegral code2, code3 = fromIntegral code3,
                           code4 = fromIntegral code4, code5 = fromIntegral code5,
                           code6 = fromIntegral code6, code7 = fromIntegral code7,
                           code8 = fromIntegral code8, code9 = fromIntegral code9,
                           codeA = fromIntegral codeA, codeB = fromIntegral codeB,
                           codeC = fromIntegral codeC, codeD = fromIntegral codeD,
                           codeE = fromIntegral codeE, codeF = fromIntegral codeF 
                        }
                 -- Make a piece of code to jump to "entry_label".
                 -- This is the only arch-dependent bit.
                 [code0,code1,code2,code3,code4,code5,code6,code7,
                  code8,code9,codeA,codeB,codeC,codeD,codeE,codeF]
                    = mkJumpToAddr entry_addr
             in
                 do addr <- malloc
                    --putStrLn ("SIZE of itbl is " ++ show (sizeOf itbl))
                    --putStrLn ("# ptrs  of itbl is " ++ show ptrs)
                    --putStrLn ("# nptrs of itbl is " ++ show nptrs_really)
                    poke addr itbl
                    return (getName dcon, addr `plusPtr` 8)


-- Make code which causes a jump to the given address.  This is the
-- only arch-dependent bit of the itbl story.  The returned list is
-- 16 elements long, since on sparc 4 words (i.e. 4 insns) are needed.

-- For sparc_TARGET_ARCH, i386_TARGET_ARCH, etc.
#include "nativeGen/NCG.h"

mkJumpToAddr :: Addr -> [Word8]

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

mkJumpToAddr a
   = let w32 = fromIntegral (addrToInt a)
         insn1 = 0x07000000 .|. (hi22 w32)
         insn2 = 0x8610E000 .|. (lo10 w32)
         insn3 = 0x81C0C000
         insn4 = 0x01000000 

         -- big-endianly ...
         w2bytes :: Word32 -> [Word8]
         w2bytes w
            = map fromIntegral [byte 3 w, byte 2 w, byte 1 w, byte 0 w]

         hi22, lo10 :: Word32 -> Word32
         lo10 x = x .&. 0x3FF
         hi22 x = (x `shiftR` 10) .&. 0x3FFFF
      
         insnBytes
            = concat (map w2bytes [insn1, insn2, insn3, insn4])
     in
         insnBytes
#endif

#if i386_TARGET_ARCH
-- Let the address to jump to be 0xWWXXYYZZ.
-- Generate   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
-- which is
-- B8 ZZ YY XX WW FF E0
mkJumpToAddr a
   = let w32 = fromIntegral (addrToInt a)
         insnBytes
            = take 16 (
                 [0xB8, byte 0 w32, byte 1 w32, 
                        byte 2 w32, byte 3 w32, 
                  0xFF, 0xE0]
                 ++ let nops = 0x90 : nops in nops
              )
     in
         insnBytes
#endif


byte :: Int -> Word32 -> Word32
byte 0 w = w .&. 0xFF
byte 1 w = (w `shiftR` 8) .&. 0xFF
byte 2 w = (w `shiftR` 16) .&. 0xFF
byte 3 w = (w `shiftR` 24) .&. 0xFF


vecret_entry 0 = stg_interp_constr1_entry
vecret_entry 1 = stg_interp_constr2_entry
vecret_entry 2 = stg_interp_constr3_entry
vecret_entry 3 = stg_interp_constr4_entry
vecret_entry 4 = stg_interp_constr5_entry
vecret_entry 5 = stg_interp_constr6_entry
vecret_entry 6 = stg_interp_constr7_entry
vecret_entry 7 = stg_interp_constr8_entry

-- entry point for direct returns for created constr itbls
foreign label "stg_interp_constr_entry" stg_interp_constr_entry :: Addr
-- and the 8 vectored ones
foreign label "stg_interp_constr1_entry" stg_interp_constr1_entry :: Addr
foreign label "stg_interp_constr2_entry" stg_interp_constr2_entry :: Addr
foreign label "stg_interp_constr3_entry" stg_interp_constr3_entry :: Addr
foreign label "stg_interp_constr4_entry" stg_interp_constr4_entry :: Addr
foreign label "stg_interp_constr5_entry" stg_interp_constr5_entry :: Addr
foreign label "stg_interp_constr6_entry" stg_interp_constr6_entry :: Addr
foreign label "stg_interp_constr7_entry" stg_interp_constr7_entry :: Addr
foreign label "stg_interp_constr8_entry" stg_interp_constr8_entry :: Addr





-- Ultra-minimalist version specially for constructors
data StgInfoTable = StgInfoTable {
   ptrs   :: Word16,
   nptrs  :: Word16,
   srtlen :: Word16,
   tipe   :: Word16,
   code0, code1, code2, code3, code4, code5, code6, code7,
   code8, code9, codeA, codeB, codeC, codeD, codeE, codeF :: Word8
}


instance Storable StgInfoTable where

   sizeOf itbl 
      = (sum . map (\f -> f itbl))
        [fieldSz ptrs, fieldSz nptrs, fieldSz srtlen, fieldSz tipe,
         fieldSz code0, fieldSz code1, fieldSz code2, fieldSz code3, 
         fieldSz code4, fieldSz code5, fieldSz code6, fieldSz code7,
         fieldSz code8, fieldSz code9, fieldSz codeA, fieldSz codeB, 
         fieldSz codeC, fieldSz codeD, fieldSz codeE, fieldSz codeF]

   alignment itbl 
      = (sum . map (\f -> f itbl))
        [fieldAl ptrs, fieldAl nptrs, fieldAl srtlen, fieldAl tipe,
         fieldAl code0, fieldAl code1, fieldAl code2, fieldAl code3, 
         fieldAl code4, fieldAl code5, fieldAl code6, fieldAl code7,
         fieldAl code8, fieldAl code9, fieldAl codeA, fieldAl codeB, 
         fieldAl codeC, fieldAl codeD, fieldAl codeE, fieldAl codeF]

   poke a0 itbl
      = do a1  <- store (ptrs   itbl) (castPtr a0)
           a2  <- store (nptrs  itbl) a1
           a3  <- store (tipe   itbl) a2
           a4  <- store (srtlen itbl) a3
           a5  <- store (code0  itbl) a4
           a6  <- store (code1  itbl) a5
           a7  <- store (code2  itbl) a6
           a8  <- store (code3  itbl) a7
           a9  <- store (code4  itbl) a8
           aA  <- store (code5  itbl) a9
           aB  <- store (code6  itbl) aA
           aC  <- store (code7  itbl) aB
           aD  <- store (code8  itbl) aC
           aE  <- store (code9  itbl) aD
           aF  <- store (codeA  itbl) aE
           a10 <- store (codeB  itbl) aF
           a11 <- store (codeC  itbl) a10
           a12 <- store (codeD  itbl) a11
           a13 <- store (codeE  itbl) a12
           a14 <- store (codeF  itbl) a13
           return ()

   peek a0
      = do (a1, ptrs)   <- load (castPtr a0)
           (a2, nptrs)  <- load a1
           (a3, tipe)   <- load a2
           (a4, srtlen) <- load a3
           (a5, code0)  <- load a4
           (a6, code1)  <- load a5
           (a7, code2)  <- load a6
           (a8, code3)  <- load a7
           (a9, code4)  <- load a8
           (aA, code5)  <- load a9
           (aB, code6)  <- load aA
           (aC, code7)  <- load aB
           (aD, code8)  <- load aC
           (aE, code9)  <- load aD
           (aF, codeA)  <- load aE
           (a10,codeB)  <- load aF
           (a11,codeC)  <- load a10
           (a12,codeD)  <- load a11
           (a13,codeE)  <- load a12
           (a14,codeF)  <- load a13
           return 
              StgInfoTable { 
                 ptrs = ptrs, nptrs = nptrs, 
                 srtlen = srtlen, tipe = tipe,
                 code0 = code0, code1 = code1, code2 = code2, code3 = code3,
                 code4 = code4, code5 = code5, code6 = code6, code7 = code7,
                 code8 = code8, code9 = code9, codeA = codeA, codeB = codeB,
                 codeC = codeC, codeD = codeD, codeE = codeE, codeF = codeF
              }

fieldSz :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

fieldAl :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldAl sel x = alignment (sel x)

store :: Storable a => a -> Ptr a -> IO (Ptr b)
store x addr = do poke addr x
                  return (castPtr (addr `plusPtr` sizeOf x))

load :: Storable a => Ptr a -> IO (Ptr b, a)
load addr = do x <- peek addr
               return (castPtr (addr `plusPtr` sizeOf x), x)

\end{code}
