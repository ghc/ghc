%
% (c) The University of Glasgow 2001
%
\section[ByteCodeGen]{Generate machine-code sequences for foreign import}

\begin{code}
module ByteCodeFFI ( taggedSizeW, untaggedSizeW, mkMarshalCode ) where

#include "HsVersions.h"

import Outputable
import PrimRep		( PrimRep(..), getPrimRepSize, isFollowableRep )
import ForeignCall	( CCallConv(..) )

-- DON'T remove apparently unused imports here .. there is ifdeffery
-- below
import Bits		( Bits(..), shiftR, shiftL )

import Word		( Word8, Word32 )
import Addr		( Addr(..), writeWord8OffAddr )
import Foreign		( Ptr(..), mallocBytes )
import IOExts		( trace )

\end{code}

%************************************************************************
%*									*
\subsection{The sizes of things.  These are platform-independent.}
%*									*
%************************************************************************

\begin{code}

-- When I push one of these on the H stack, how much does Sp move by?
taggedSizeW :: PrimRep -> Int
taggedSizeW pr
   | isFollowableRep pr = 1 {-it's a pointer, Jim-}
   | otherwise          = 1 {-the tag-} + getPrimRepSize pr

-- The plain size of something, without tag.
untaggedSizeW :: PrimRep -> Int
untaggedSizeW pr
   | isFollowableRep pr = 1
   | otherwise          = getPrimRepSize pr

-- How big is this thing's tag?
sizeOfTagW :: PrimRep -> Int
sizeOfTagW pr
   | isFollowableRep pr = 0
   | otherwise          = 1

-- Blast a bunch of bytes into malloc'd memory and return the addr.
sendBytesToMallocville :: [Word8] -> IO Addr
sendBytesToMallocville bytes
   = do let n = length bytes
        (Ptr a#) <- mallocBytes n
        mapM ( \(off,byte) -> writeWord8OffAddr (A# a#) off byte )
             (zip [0 ..] bytes)
        return (A# a#)
\end{code}

%************************************************************************
%*									*
\subsection{The platform-dependent marshall-code-generator.}
%*									*
%************************************************************************

\begin{code}

-- For sparc_TARGET_ARCH, i386_TARGET_ARCH, etc.
#include "nativeGen/NCG.h"

{-
Make a piece of code which expects to see the Haskell stack
looking like this.  It is given a pointer to the lowest word in
the stack -- presumably the tag of the placeholder.
                 
                  <arg_n>
                  ...
                  <arg_1>
                  Addr# address_of_C_fn
                  <placeholder-for-result#> (must be an unboxed type)

We cope with both ccall and stdcall for the C fn.  However, this code
itself expects only to be called using the ccall convention -- that is,
we don't clear our own (single) arg off the C stack.
-}
mkMarshalCode :: CCallConv
              -> (Int, PrimRep) -> Int -> [(Int, PrimRep)] 
              -> IO Addr
mkMarshalCode cconv (r_offW, r_rep) addr_offW arg_offs_n_reps
   = let bytes = mkMarshalCode_wrk cconv (r_offW, r_rep) 
                                   addr_offW arg_offs_n_reps
     in  sendBytesToMallocville bytes




mkMarshalCode_wrk :: CCallConv 
                  -> (Int, PrimRep) -> Int -> [(Int, PrimRep)] 
                  -> [Word8]

mkMarshalCode_wrk cconv (r_offW, r_rep) addr_offW arg_offs_n_reps

#if i386_TARGET_ARCH

   = let -- Don't change this without first consulting Intel Corp :-)
         bytes_per_word = 4

         -- addr and result bits offsetsW
         offset_of_addr_bitsW = addr_offW + sizeOfTagW AddrRep
         offset_of_res_bitsW  = r_offW + sizeOfTagW r_rep

         offsets_to_pushW
            = concat
              [ let -- where this arg's bits start
                    a_bits_offW = a_offW + sizeOfTagW a_rep
                in 
                    -- reversed because x86 is little-endian
                    reverse 
                    [a_bits_offW .. a_bits_offW + untaggedSizeW a_rep - 1]

                -- reversed because args are pushed L -> R onto C stack
                | (a_offW, a_rep) <- reverse arg_offs_n_reps
              ]

         -- some helpers to assemble x86 insns.
         movl_offespmem_esi offB	-- movl   offB(%esp), %esi
            = [0x8B, 0xB4, 0x24] ++ lit32 offB
         movl_offesimem_ecx offB	-- movl   offB(%esi), %ecx
            = [0x8B, 0x8E] ++ lit32 offB
         save_regs			-- pushl  all intregs except %esp
            = [0x50, 0x53, 0x51, 0x52, 0x56, 0x57, 0x55]
         restore_regs			-- popl   ditto
            = [0x5D, 0x5F, 0x5E, 0x5A, 0x59, 0x5B, 0x58]
         pushl_ecx			-- pushl  %ecx
            = [0x51]
         call_star_ecx			-- call   * %ecx
            = [0xFF, 0xD1]
         add_lit_esp lit		-- addl   $lit, %esp
            = [0x81, 0xC4] ++ lit32 lit
         movl_eax_offesimem offB	-- movl   %eax, offB(%esi)
            = [0x89, 0x86] ++ lit32 offB
         ret				-- ret
            = [0xC3]
         fstpl_offesimem offB		-- fstpl   offB(%esi)
            = [0xDD, 0x9E] ++ lit32 offB
         fstps_offesimem offB		-- fstps   offB(%esi)
            = [0xD9, 0x9E] ++ lit32 offB
         lit32 :: Int -> [Word8]
         lit32 i = let w32 = (fromIntegral i) :: Word32
                   in  map (fromIntegral . ( .&. 0xFF))
                           [w32, w32 `shiftR` 8, 
                            w32 `shiftR` 16,  w32 `shiftR` 24]
         {-
             2 0000 8BB42478    movl    0x12345678(%esp), %esi
             2      563412
             3 0007 8B8E7856    movl    0x12345678(%esi), %ecx
             3      3412
             4              
             5 000d 50535152    pushl %eax ; pushl %ebx ; pushl %ecx ; pushl  %edx
             6 0011 565755      pushl %esi ; pushl %edi ; pushl %ebp
             7              
             8 0014 5D5F5E      popl %ebp ; popl %edi ; popl %esi 
             9 0017 5A595B58    popl %edx ; popl %ecx ; popl %ebx ; popl %eax
            10              
            11 001b 51          pushl %ecx
            12 001c FFD1        call * %ecx
            13              
            14 001e 81C47856    addl $0x12345678, %esp
            14      3412
            15 0024 89867856    movl %eax, 0x12345678(%esi)
            15      3412
            16 002a 89967856    movl %edx, 0x12345678(%esi)
            16      3412
            17           
            18 0030 DD967856    fstl    0x12345678(%esi)
            18      3412
            19 0036 DD9E7856    fstpl   0x12345678(%esi)
            19      3412
            20 003c D9967856    fsts    0x12345678(%esi)
            20      3412
            21 0042 D99E7856    fstps   0x12345678(%esi)
            18              
            19 0030 C3          ret
            20              

         -}

     in
     --trace (show (map fst arg_offs_n_reps))
     (
     {- On entry, top of C stack 0(%esp) is the RA and 4(%esp) is 
        arg passed from the interpreter.

        Push all callee saved regs.  Push all of them anyway ...
           pushl       %eax
           pushl       %ebx
           pushl       %ecx
           pushl       %edx
           pushl       %esi
           pushl       %edi
           pushl       %ebp
     -}
     save_regs

     {- Now 28+0(%esp) is RA and 28+4(%esp) is the arg (the H stack ptr).
        We'll use %esi as a temporary to point at the H stack, and
        %ecx as a temporary to copy via.

           movl        28+4(%esp), %esi
     -}
     ++ movl_offespmem_esi 32

     {- For each arg in args_offs_n_reps, examine the associated PrimRep 
        to determine how many payload (non-tag) words there are, and 
        whether or not there is a tag.  This gives a bunch of offsets on 
        the H stack to copy to the C stack:

           movl        off1(%esi), %ecx
           pushl       %ecx
     -}
     ++ concatMap (\offW -> movl_offesimem_ecx (bytes_per_word * offW) 
                            ++ pushl_ecx) 
                  offsets_to_pushW

     {- Get the addr to call into %ecx, bearing in mind that there's 
        an Addr# tag at the indicated location, and do the call:

           movl        4*(1 /*tag*/ +addr_offW)(%esi), %ecx
           call        * %ecx
     -}
     ++ movl_offesimem_ecx (bytes_per_word * offset_of_addr_bitsW)
     ++ call_star_ecx

     {- Nuke the args just pushed and re-establish %esi at the 
        H-stack ptr:

           addl        $4*number_of_args_pushed, %esp (ccall only)
           movl        28+4(%esp), %esi
     -}
     ++ (if   cconv /= StdCallConv
         then add_lit_esp (bytes_per_word * length offsets_to_pushW)
         else [])
     ++ movl_offespmem_esi 32

     {- Depending on what the return type is, get the result 
        from %eax or %edx:%eax or %st(0).

           movl        %eax, 4(%esi)        -- assuming tagged result
        or
           movl        %edx, 4(%esi)
           movl        %eax, 8(%esi)
        or
           fstpl       4(%esi)
        or
           fstps       4(%esi)
     -}
     ++ case r_rep of
           CharRep   -> movl_eax_offesimem 4
           IntRep    -> movl_eax_offesimem 4
           WordRep   -> movl_eax_offesimem 4
           AddrRep   -> movl_eax_offesimem 4
           DoubleRep -> fstpl_offesimem 4
           FloatRep  -> fstps_offesimem 4
           VoidRep   -> []
           other     -> pprPanic "ByteCodeFFI.mkMarshalCode_wrk(x86)" 
                                 (ppr r_rep)

     {- Restore all the pushed regs and go home.

           pushl        %ebp
           pushl        %edi
           pushl        %esi
           pushl        %edx
           pushl        %ecx
           pushl        %ebx
           pushl        %eax

           ret
     -}
     ++ restore_regs
     ++ ret
     )

#elif sparc_TARGET_ARCH

   = let -- At least for sparc V8
         bytes_per_word = 4

         -- speaks for itself
         w32_to_w8s_bigEndian :: Word32 -> [Word8]
         w32_to_w8s_bigEndian w
            =  [fromIntegral (0xFF .&. (w `shiftR` 24)),
                fromIntegral (0xFF .&. (w `shiftR` 16)),
                fromIntegral (0xFF .&. (w `shiftR` 8)),
                fromIntegral (0xFF .&. w)]

         -- addr and result bits offsetsW
         offset_of_addr_bitsW = addr_offW + sizeOfTagW AddrRep
         offset_of_res_bitsW  = r_offW + sizeOfTagW r_rep

         offsets_to_pushW
            = concat
              [ let -- where this arg's bits start
                    a_bits_offW = a_offW + sizeOfTagW a_rep
                in 
                    [a_bits_offW .. a_bits_offW + untaggedSizeW a_rep - 1]

                | (a_offW, a_rep) <- arg_offs_n_reps
              ]

         total_argWs    = length offsets_to_pushW
         argWs_on_stack = if total_argWs > 6 then total_argWs - 6 
                                             else 0

         -- The stack pointer must be kept 8-byte aligned, which means
         -- we need to calculate this quantity too
         argWs_on_stack_ROUNDED_UP
            | odd argWs_on_stack = 1 + argWs_on_stack
            | otherwise          = argWs_on_stack

         -- some helpers to assemble sparc insns.
         -- REGS
         iReg, oReg, gReg, fReg :: Int -> Word32
         iReg = fromIntegral . (+ 24)
         oReg = fromIntegral . (+ 8)
         gReg = fromIntegral . (+ 0)
         fReg = fromIntegral

         sp = oReg 6
         i0 = iReg 0
         i7 = iReg 7
         o0 = oReg 0
         o1 = oReg 1
         o7 = oReg 7
         g0 = gReg 0
         g1 = gReg 1
         f0 = fReg 0
         f1 = fReg 1

         -- INSN templates
         insn_r_r_i :: Word32 -> Word32 -> Word32 -> Int -> Word32
         insn_r_r_i op3 rs1 rd imm13
            = (3 `shiftL` 30) 
              .|. (rs1 `shiftL` 25)
              .|. (op3 `shiftL` 19)
              .|. (rd `shiftL` 14) 
              .|. (1 `shiftL` 13) 
              .|. mkSimm13 imm13

         insn_r_i_r :: Word32 -> Word32 -> Int -> Word32 -> Word32
         insn_r_i_r op3 rs1 imm13 rd
            = (2 `shiftL` 30) 
              .|. (rd `shiftL` 25)
              .|. (op3 `shiftL` 19)
              .|. (rs1 `shiftL` 14) 
              .|. (1 `shiftL` 13) 
              .|. mkSimm13 imm13

         mkSimm13 :: Int -> Word32
         mkSimm13 imm13 
            = let imm13w = (fromIntegral imm13) :: Word32
              in  imm13w .&. 0x1FFF             

         -- REAL (non-synthetic) insns
         -- or %rs1, %rs2, %rd
         mkOR :: Word32 -> Word32 -> Word32 -> Word32
         mkOR rs1 rs2 rd 
            = (2 `shiftL` 30) 
              .|. (rd `shiftL` 25)
              .|. (op3_OR `shiftL` 19)
              .|. (rs1 `shiftL` 14) 
              .|. (0 `shiftL` 13) 
              .|. rs2
              where op3_OR = 2 :: Word32

         -- ld(int)   [%rs + imm13], %rd
         mkLD rs1 imm13 rd = insn_r_r_i 0x00{-op3_LD-} rd rs1 imm13

         -- st(int)   %rs, [%rd + imm13]
         mkST   = insn_r_r_i 0x04 -- op3_ST

         -- st(float) %rs, [%rd + imm13]
         mkSTF  = insn_r_r_i 0x24 -- op3_STF

         -- jmpl     %rs + imm13, %rd
         mkJMPL = insn_r_i_r 0x38 -- op3_JMPL

         -- save     %rs + imm13, %rd
         mkSAVE = insn_r_i_r 0x3C -- op3_SAVE

         -- restore  %rs + imm13, %rd
         mkRESTORE = insn_r_i_r 0x3D -- op3_RESTORE

         -- SYNTHETIC insns
         mkNOP             = mkOR g0 g0 g0
         mkCALL reg        = mkJMPL reg 0 o7
         mkRET             = mkJMPL i7 8 g0
         mkRESTORE_TRIVIAL = mkRESTORE g0 0 g0
     in
     --trace (show (map fst arg_offs_n_reps))
     concatMap w32_to_w8s_bigEndian (

     {- On entry, %o0 is the arg passed from the interpreter.  After
        the initial save insn, it will be in %i0.  Studying the sparc
        docs one would have thought that the minimum frame size is 92
        bytes, but gcc always uses at least 112, and indeed there are
        segfaults a-plenty with 92.  So I use 112 here as well.  I
        don't understand why, tho.  
     -}
     [mkSAVE sp (- ({-92-}112 + 4*argWs_on_stack_ROUNDED_UP)) sp]

     {- For each arg in args_offs_n_reps, examine the associated PrimRep 
        to determine how many payload (non-tag) words there are, and 
        whether or not there is a tag.  This gives a bunch of offsets on 
        the H stack.  Move the first 6 words into %o0 .. %o5 and the
        rest on the stack, starting at [%sp+92].  Use %g1 as a temp.
     -}
     ++ let doArgW (offW, wordNo)
              | wordNo < 6
              = [mkLD i0 (bytes_per_word * offW) (oReg wordNo)]
              | otherwise
              = [mkLD i0 (bytes_per_word * offW) g1,
                 mkST g1 sp (92 + bytes_per_word * (wordNo - 6))]
        in  
            concatMap doArgW (zip offsets_to_pushW [0 ..])

     {- Get the addr to call into %g1, bearing in mind that there's 
        an Addr# tag at the indicated location, and do the call:

           ld     [4*(1 /*tag*/ +addr_offW) + %i0], %g1
           call   %g1
     -}
     ++ [mkLD i0 (bytes_per_word * offset_of_addr_bitsW) g1,
         mkCALL g1,
         mkNOP]

     {- Depending on what the return type is, get the result 
        from %o0 or %o1:%o0 or %f0 or %f1:%f0.

           st          %o0, [%i0 + 4]        -- 32 bit int
        or
           st          %o0, [%i0 + 4]        -- 64 bit int
           st          %o1, [%i0 + 8]        -- or the other way round?
        or
           st          %f0, [%i0 + 4]        -- 32 bit float
        or
           st          %f0, [%i0 + 4]        -- 64 bit float
           st          %f1, [%i0 + 8]        -- or the other way round?

     -}
     ++ let i32 = [mkST o0 i0 4]
            i64 = [mkST o0 i0 4, mkST o1 i0 8]
            f32 = [mkSTF f0 i0 4]
            f64 = [mkSTF f0 i0 4, mkSTF f1 i0 8]
        in
            case r_rep of
               CharRep   -> i32
               IntRep    -> i32
               WordRep   -> i32
               AddrRep   -> i32
               DoubleRep -> f64
               FloatRep  -> f32
               VoidRep   -> []
               other     -> pprPanic "ByteCodeFFI.mkMarshalCode_wrk(sparc)" 
                                     (ppr r_rep)

     ++ [mkRET,
         mkRESTORE_TRIVIAL]  -- this is in the delay slot of the RET
     )

#else 

   = undefined

#endif

\end{code}

