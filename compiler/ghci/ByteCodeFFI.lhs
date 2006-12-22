%
% (c) The University of Glasgow 2001-2006
%

ByteCodeGen: Generate machine-code sequences for foreign import

\begin{code}
module ByteCodeFFI ( mkMarshalCode, moan64, newExec ) where

#include "HsVersions.h"

import Outputable
import SMRep
import ForeignCall
import Panic

-- DON'T remove apparently unused imports here .. 
-- there is ifdeffery below
import Control.Exception ( throwDyn )
import Data.Bits	( Bits(..), shiftR, shiftL )
import Data.List        ( mapAccumL )

import Data.Word	( Word8, Word32 )
import Foreign		( Ptr, FunPtr, castPtrToFunPtr,
			  Storable, sizeOf, pokeArray )
import Foreign.C	( CUInt )
import System.IO.Unsafe ( unsafePerformIO )
import System.IO	( hPutStrLn, stderr )
-- import Debug.Trace	( trace )
\end{code}

%************************************************************************
%*									*
\subsection{The platform-dependent marshall-code-generator.}
%*									*
%************************************************************************

\begin{code}

moan64 :: String -> SDoc -> a
moan64 msg pp_rep
   = unsafePerformIO (
        hPutStrLn stderr (
        "\nGHCi's bytecode generation machinery can't handle 64-bit\n" ++
        "code properly yet.  You can work around this for the time being\n" ++
        "by compiling this module and all those it imports to object code,\n" ++
        "and re-starting your GHCi session.  The panic below contains information,\n" ++
        "intended for the GHC implementors, about the exact place where GHC gave up.\n"
        )
     )
     `seq`
     pprPanic msg pp_rep


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
              -> (Int, CgRep) -> Int -> [(Int, CgRep)] 
              -> IO (FunPtr ())
mkMarshalCode cconv (r_offW, r_rep) addr_offW arg_offs_n_reps
   = let bytes = mkMarshalCode_wrk cconv (r_offW, r_rep) 
                                   addr_offW arg_offs_n_reps
     in  newExec bytes

newExec :: Storable a => [a] -> IO (FunPtr ())
newExec code
   = do ptr <- _allocateExec (fromIntegral $ codeSize undefined code)
        pokeArray ptr code
        return (castPtrToFunPtr ptr)
   where
   codeSize :: Storable a => a -> [a] -> Int
   codeSize dummy array = sizeOf(dummy) * length array

foreign import ccall unsafe "allocateExec"
  _allocateExec :: CUInt -> IO (Ptr a)  

mkMarshalCode_wrk :: CCallConv 
                  -> (Int, CgRep) -> Int -> [(Int, CgRep)] 
                  -> [Word8]

mkMarshalCode_wrk cconv (r_offW, r_rep) addr_offW arg_offs_n_reps

#if i386_TARGET_ARCH

   = let -- Don't change this without first consulting Intel Corp :-)
         bytes_per_word = 4

         offsets_to_pushW
            = concat
              [   -- reversed because x86 is little-endian
                  reverse [a_offW .. a_offW + cgRepSizeW a_rep - 1]

                -- reversed because args are pushed L -> R onto C stack
                | (a_offW, a_rep) <- reverse arg_offs_n_reps
              ]
         
         arguments_size = bytes_per_word * length offsets_to_pushW
#if darwin_TARGET_OS
             -- Darwin: align stack frame size to a multiple of 16 bytes
         stack_frame_size = (arguments_size + 15) .&. complement 15
         stack_frame_pad = stack_frame_size - arguments_size
#else
         stack_frame_size = arguments_size
#endif

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
         movl_edx_offesimem offB	-- movl   %edx, offB(%esi)
            = [0x89, 0x96] ++ lit32 offB
         ret				-- ret
            = [0xC3]
         fstpl_offesimem offB		-- fstpl   offB(%esi)
            = [0xDD, 0x9E] ++ lit32 offB
         fstps_offesimem offB		-- fstps   offB(%esi)
            = [0xD9, 0x9E] ++ lit32 offB
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

#if darwin_TARGET_OS
     {- On Darwin, add some padding so that the stack stays aligned. -}
     ++ (if stack_frame_pad /= 0
            then add_lit_esp (-stack_frame_pad)
            else [])
#endif

     {- For each arg in args_offs_n_reps, examine the associated
        CgRep to determine how many words there are.  This gives a
        bunch of offsets on the H stack to copy to the C stack:

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
     ++ movl_offesimem_ecx (bytes_per_word * addr_offW)
     ++ call_star_ecx

     {- Nuke the args just pushed and re-establish %esi at the 
        H-stack ptr:

           addl        $4*number_of_args_pushed, %esp (ccall only)
           movl        28+4(%esp), %esi
     -}
     ++ (if   cconv /= StdCallConv
         then add_lit_esp stack_frame_size
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
     ++ let i32 = movl_eax_offesimem 0
            i64 = movl_eax_offesimem 0 ++ movl_edx_offesimem 4
            f32 = fstps_offesimem 0
            f64 = fstpl_offesimem 0
        in
        case r_rep of
           NonPtrArg -> i32
           DoubleArg -> f64  
           FloatArg  -> f32
           LongArg   -> i64
           VoidArg   -> []
           other     -> moan64 "ByteCodeFFI.mkMarshalCode_wrk(x86)" 
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

#elif x86_64_TARGET_ARCH

   =
     -- the address of the H stack is in %rdi.  We need to move it out, so
     -- we can use %rdi as an arg reg for the following call:
    pushq_rbp ++
    movq_rdi_rbp ++
	
     -- ####### load / push the args

     let
	(stack_args, fregs_unused, reg_loads) = 
	   load_arg_regs arg_offs_n_reps int_loads float_loads []

	tot_arg_size = bytes_per_word * length stack_args

	-- On entry to the called function, %rsp should be aligned
	-- on a 16-byte boundary +8 (i.e. the first stack arg after
	-- the return address is 16-byte aligned).  In STG land
	-- %rsp is kept 16-byte aligned (see StgCRun.c), so we just
	-- need to make sure we push a multiple of 16-bytes of args,
	-- plus the return address, to get the correct alignment.
	(real_size, adjust_rsp)
	  | tot_arg_size `rem` 16 == 0 	  = (tot_arg_size, [])
	  | otherwise           	  = (tot_arg_size + 8, subq_lit_rsp 8)

	(stack_pushes, stack_words) =
		push_args stack_args [] 0

	-- we need to know the number of SSE regs used in the call, see later
	n_sse_regs_used = length float_loads - length fregs_unused
     in
        concat reg_loads
     ++ adjust_rsp
     ++ concat stack_pushes -- push in reverse order

     -- ####### make the call

	-- use %r10 to make the call, because we don't have to save it.
        --      movq 8*addr_offW(%rbp), %r10
     ++ movq_rbpoff_r10 (bytes_per_word * addr_offW)

	-- The x86_64 ABI requires us to set %al to the number of SSE
	-- registers that contain arguments, if the called routine
	-- is a varargs function.  We don't know whether it's a
	-- varargs function or not, so we have to assume it is.
	--
	-- It's not safe to omit this assignment, even if the number
	-- of SSE regs in use is zero.  If %al is larger than 8
	-- on entry to a varargs function, seg faults ensue.
     ++ movq_lit_rax n_sse_regs_used
     ++ call_star_r10

	-- pop the args from the stack, only in ccall mode 
	-- (in stdcall the callee does it).
     ++ (if   cconv /= StdCallConv
         then addq_lit_rsp real_size
         else [])

     -- ####### place the result in the right place and return

     ++ assign_result
     ++ popq_rbp
     ++ ret

  where
     bytes_per_word = 8

     -- int arg regs: rdi,rsi,rdx,rcx,r8,r9
     -- flt arg regs: xmm0..xmm7
     int_loads   = [ movq_rbpoff_rdi, movq_rbpoff_rsi, movq_rbpoff_rdx,
 		     movq_rbpoff_rcx, movq_rbpoff_r8,  movq_rbpoff_r9 ]
     float_loads = [ (mov_f32_rbpoff_xmm n, mov_f64_rbpoff_xmm n) | n <- [0..7] ]

     load_arg_regs args [] [] code     =  (args, [], code)
     load_arg_regs [] iregs fregs code =  ([], fregs, code)
     load_arg_regs ((off,rep):args) iregs fregs code
	| FloatArg  <- rep, ((mov_f32,_):frest) <- fregs =
		load_arg_regs args iregs frest (mov_f32 (bytes_per_word * off) : code)
	| DoubleArg <- rep, ((_,mov_f64):frest) <- fregs =
		load_arg_regs args iregs frest (mov_f64 (bytes_per_word * off) : code)
	| (mov_reg:irest) <- iregs =
		load_arg_regs args irest fregs (mov_reg (bytes_per_word * off) : code)
	| otherwise =
		 push_this_arg
	where
	   push_this_arg = ((off,rep):args',fregs', code')
		where (args',fregs',code') = load_arg_regs args iregs fregs code

     push_args [] code pushed_words = (code, pushed_words)
     push_args ((off,rep):args) code pushed_words
	| FloatArg  <- rep =
		push_args args (push_f32_rbpoff (bytes_per_word * off) : code) 
			(pushed_words+1)
	| DoubleArg <- rep =
		push_args args (push_f64_rbpoff (bytes_per_word * off) : code)
			(pushed_words+1)
	| otherwise =
		push_args args (pushq_rbpoff (bytes_per_word * off) : code)
			(pushed_words+1)


     assign_result = 
	case r_rep of
	  DoubleArg -> f64
	  FloatArg  -> f32
          VoidArg   -> []
	  _other    -> i64
	where
	  i64 = movq_rax_rbpoff 0
	  f32 = mov_f32_xmm0_rbpoff 0
	  f64 = mov_f64_xmm0_rbpoff 0

-- ######### x86_64 machine code:

--   0:   48 89 fd                mov    %rdi,%rbp
--   3:   48 8b bd 78 56 34 12    mov    0x12345678(%rbp),%rdi
--   a:   48 8b b5 78 56 34 12    mov    0x12345678(%rbp),%rsi
--  11:   48 8b 95 78 56 34 12    mov    0x12345678(%rbp),%rdx
--  18:   48 8b 8d 78 56 34 12    mov    0x12345678(%rbp),%rcx
--  1f:   4c 8b 85 78 56 34 12    mov    0x12345678(%rbp),%r8
--  26:   4c 8b 8d 78 56 34 12    mov    0x12345678(%rbp),%r9
--  2d:   4c 8b 95 78 56 34 12    mov    0x12345678(%rbp),%r10
--  34:   48 c7 c0 78 56 34 12    mov    $0x12345678,%rax
--  3b:   48 89 85 78 56 34 12    mov    %rax,0x12345678(%rbp)
--  42:   f3 0f 10 85 78 56 34 12 movss  0x12345678(%rbp),%xmm0
--  4a:   f2 0f 10 85 78 56 34 12 movsd  0x12345678(%rbp),%xmm0
--  52:   f3 0f 11 85 78 56 34 12 movss  %xmm0,0x12345678(%rbp)
--  5a:   f2 0f 11 85 78 56 34 12 movsd  %xmm0,0x12345678(%rbp)
--  62:   ff b5 78 56 34 12       pushq  0x12345678(%rbp)
--  68:   f3 44 0f 11 04 24       movss  %xmm8,(%rsp)
--  6e:   f2 44 0f 11 04 24       movsd  %xmm8,(%rsp)
--  74:   48 81 ec 78 56 34 12    sub    $0x12345678,%rsp
--  7b:   48 81 c4 78 56 34 12    add    $0x12345678,%rsp
--  82:   41 ff d2                callq  *%r10
--  85:   c3                      retq   

     movq_rdi_rbp         = [0x48,0x89,0xfd]
     movq_rbpoff_rdi  off = [0x48, 0x8b, 0xbd] ++ lit32 off
     movq_rbpoff_rsi  off = [0x48, 0x8b, 0xb5] ++ lit32 off
     movq_rbpoff_rdx  off = [0x48, 0x8b, 0x95] ++ lit32 off
     movq_rbpoff_rcx  off = [0x48, 0x8b, 0x8d] ++ lit32 off 
     movq_rbpoff_r8   off = [0x4c, 0x8b, 0x85] ++ lit32 off
     movq_rbpoff_r9   off = [0x4c, 0x8b, 0x8d] ++ lit32 off
     movq_rbpoff_r10  off = [0x4c, 0x8b, 0x95] ++ lit32 off
     movq_lit_rax     lit = [0x48, 0xc7, 0xc0] ++ lit32 lit
     movq_rax_rbpoff  off = [0x48, 0x89, 0x85] ++ lit32 off
     mov_f32_rbpoff_xmm n off = [0xf3, 0x0f, 0x10, 0x85 + n`shiftL`3] ++ lit32 off
     mov_f64_rbpoff_xmm n off = [0xf2, 0x0f, 0x10, 0x85 + n`shiftL`3] ++ lit32 off
     mov_f32_xmm0_rbpoff  off = [0xf3, 0x0f, 0x11, 0x85] ++ lit32 off
     mov_f64_xmm0_rbpoff  off = [0xf2, 0x0f, 0x11, 0x85] ++ lit32 off
     pushq_rbpoff     off = [0xff, 0xb5] ++ lit32 off
     push_f32_rbpoff  off = 
	mov_f32_rbpoff_xmm 8 off ++		 -- movss off(%rbp), %xmm8
	[0xf3, 0x44, 0x0f, 0x11, 0x04, 0x24] ++	 -- movss %xmm8, (%rsp)
	subq_lit_rsp 8				 -- subq $8, %rsp
     push_f64_rbpoff  off =
	mov_f64_rbpoff_xmm 8 off ++		 -- movsd off(%rbp), %xmm8
	[0xf2, 0x44, 0x0f, 0x11, 0x04, 0x24] ++  -- movsd %xmm8, (%rsp)
	subq_lit_rsp 8				 -- subq $8, %rsp
     subq_lit_rsp     lit = [0x48, 0x81, 0xec] ++ lit32 lit
     addq_lit_rsp     lit = [0x48, 0x81, 0xc4] ++ lit32 lit
     call_star_r10 = [0x41,0xff,0xd2]
     ret = [0xc3]
     pushq_rbp = [0x55]
     popq_rbp = [0x5d]

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

         offsets_to_pushW
            = concat
              [  [a_offW .. a_offW + cgRepSizeW a_rep - 1]

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

     {- For each arg in args_offs_n_reps, examine the associated
        CgRep to determine how many words there are.  This gives a
        bunch of offsets on the H stack.  Move the first 6 words into
        %o0 .. %o5 and the rest on the stack, starting at [%sp+92].
        Use %g1 as a temp. 
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
     ++ [mkLD i0 (bytes_per_word * addr_offW) g1,
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
     ++ let i32 = [mkST o0 i0 0]
            i64 = [mkST o0 i0 0, mkST o1 i0 4]
            f32 = [mkSTF f0 i0 0]
            f64 = [mkSTF f0 i0 0, mkSTF f1 i0 4]
        in
            case r_rep of
               NonPtrArg -> i32
               DoubleArg -> f64
               FloatArg  -> f32
               VoidArg   -> []
               other     -> moan64 "ByteCodeFFI.mkMarshalCode_wrk(sparc)" 
                                   (ppr r_rep)

     ++ [mkRET,
         mkRESTORE_TRIVIAL]  -- this is in the delay slot of the RET
     )
#elif powerpc_TARGET_ARCH && darwin_TARGET_OS

   = let
         bytes_per_word = 4

         -- speaks for itself
         w32_to_w8s_bigEndian :: Word32 -> [Word8]
         w32_to_w8s_bigEndian w
            =  [fromIntegral (0xFF .&. (w `shiftR` 24)),
                fromIntegral (0xFF .&. (w `shiftR` 16)),
                fromIntegral (0xFF .&. (w `shiftR` 8)),
                fromIntegral (0xFF .&. w)]

         -- addr and result bits offsetsW
         a_off = addr_offW * bytes_per_word
         result_off  = r_offW * bytes_per_word

         linkageArea = 24
         parameterArea = sum [ cgRepSizeW a_rep * bytes_per_word
                        | (_, a_rep) <- arg_offs_n_reps ]
         savedRegisterArea = 4
         frameSize = padTo16 (linkageArea + max parameterArea 32 + savedRegisterArea)
         padTo16 x = case x `mod` 16 of
            0 -> x
            y -> x - y + 16
             
         pass_parameters [] _ _ = []
         pass_parameters ((a_offW, a_rep):args) nextFPR offsetW =
            let
               haskellArgOffset = a_offW * bytes_per_word
               offsetW' = offsetW + cgRepSizeW a_rep
               
               pass_word w 
                   | offsetW + w < 8 =
                      [0x801f0000    -- lwz rX, src(r31)
                        .|. (fromIntegral src .&. 0xFFFF)
                        .|. (fromIntegral (offsetW+w+3) `shiftL` 21)]
                   | otherwise =
                      [0x801f0000    -- lwz r0, src(r31)
                        .|. (fromIntegral src .&. 0xFFFF),
                       0x90010000    -- stw r0, dst(r1)
                        .|. (fromIntegral dst .&. 0xFFFF)]
                  where
                     src = haskellArgOffset + w*bytes_per_word
                     dst = linkageArea + (offsetW+w) * bytes_per_word
            in
               case a_rep of
                  FloatArg | nextFPR < 14 ->
                      (0xc01f0000    -- lfs fX, haskellArgOffset(r31)
                        .|. (fromIntegral haskellArgOffset .&. 0xFFFF)
                        .|. (fromIntegral nextFPR `shiftL` 21))
                      : pass_parameters args (nextFPR+1) offsetW'
                  DoubleArg | nextFPR < 14 ->
                      (0xc81f0000    -- lfd fX, haskellArgOffset(r31)
                        .|. (fromIntegral haskellArgOffset .&. 0xFFFF)
                        .|. (fromIntegral nextFPR `shiftL` 21))
                      : pass_parameters args (nextFPR+1) offsetW'
                  _ ->
                      concatMap pass_word [0 .. cgRepSizeW a_rep - 1]
                      ++ pass_parameters args nextFPR offsetW'              
               
         gather_result = case r_rep of
            VoidArg -> []
            FloatArg -> 
               [0xd03f0000 .|. (fromIntegral result_off .&. 0xFFFF)]
               -- stfs f1, result_off(r31)
            DoubleArg -> 
               [0xd83f0000 .|. (fromIntegral result_off .&. 0xFFFF)]
               -- stfd f1, result_off(r31)
            _ | cgRepSizeW r_rep == 2 ->
               [0x907f0000 .|. (fromIntegral result_off .&. 0xFFFF),
                0x909f0000 .|. (fromIntegral (result_off+4) .&. 0xFFFF)]
               -- stw r3, result_off(r31)
               -- stw r4, result_off+4(r31)
            _ | cgRepSizeW r_rep == 1 ->
               [0x907f0000 .|. (fromIntegral result_off .&. 0xFFFF)]
               -- stw r3, result_off(r31)
     in
         concatMap w32_to_w8s_bigEndian $ [
            0x7c0802a6,         -- mflr r0
            0x93e1fffc,         -- stw r31,-4(r1)
            0x90010008,         -- stw r0,8(r1)
            0x94210000 .|. (fromIntegral (-frameSize) .&. 0xFFFF),
                                -- stwu r1, -frameSize(r1)
            0x7c7f1b78          -- mr r31, r3
         ] ++ pass_parameters arg_offs_n_reps 1 0 ++ [
            0x819f0000 .|. (fromIntegral a_off .&. 0xFFFF),
                                -- lwz r12, a_off(r31)
            0x7d8903a6,         -- mtctr r12
            0x4e800421          -- bctrl
         ] ++ gather_result ++ [
            0x80210000,         -- lwz r1, 0(r1)
            0x83e1fffc,         -- lwz r31, -4(r1)
            0x80010008,         -- lwz r0, 8(r1)
            0x7c0803a6,         -- mtlr r0
            0x4e800020          -- blr
         ]

#elif powerpc_TARGET_ARCH && linux_TARGET_OS

   -- All offsets here are measured in Words (not bytes).  This includes
   -- arguments to the load/store machine code generators, alignment numbers
   -- and the final 'framesize' among others.

   = concatMap w32_to_w8s_bigEndian $ [
            0x7c0802a6,                         -- mflr r0
            0x93e1fffc,                         -- stw r31,-4(r1)
            0x90010008,                         -- stw r0,8(r1)
            0x94210000 .|. offset (-framesize), -- stwu r1, -frameSize(r1)
            0x7c7f1b78                          -- mr r31, r3
            ] ++ pass_parameters ++             -- pass the parameters
            loadWord 12 addr_offW ++ [          -- lwz r12, a_off(r31)
            0x7d8903a6,                         -- mtctr r12
            0x4e800421                          -- bctrl
            ] ++ gather_result ++ [             -- save the return value
            0x80210000,                         -- lwz r1, 0(r1)
            0x83e1fffc,                         -- lwz r31, -4(r1)
            0x80010008,                         -- lwz r0, 8(r1)
            0x7c0803a6,                         -- mtlr r0
            0x4e800020                          -- blr
         ]

   where
     gather_result :: [Word32]
     gather_result = case r_rep of
       VoidArg   -> []
       FloatArg  -> storeFloat  1 r_offW
       DoubleArg -> storeDouble 1 r_offW
       LongArg   -> storeLong   3 r_offW
       _         -> storeWord   3 r_offW

     pass_parameters :: [Word32]
     pass_parameters = concat params

     -- vector aligned (4 word = 16 bytes) with 8 extra words of buffer space
     framesize = alignedTo 4 (argsize + 8)

     ((_,_,argsize), params) = mapAccumL loadparam (3,1,2) arg_offs_n_reps

     -- handle one argument, returning machine code and the updated state
     loadparam :: (Int, Int, Int) -> (Int, CgRep) ->
                  ((Int, Int, Int), [Word32])

     loadparam (gpr, fpr, stack) (ofs, rep) = case rep of
       FloatArg | fpr <= 8  -> ( (gpr, fpr + 1, stack),  loadFloat fpr ofs )
       FloatArg             -> ( (gpr, fpr, stack + 1),  stackWord stack ofs )

       DoubleArg | fpr <= 8 -> ( (gpr, fpr + 1, stack),  loadDouble fpr ofs )
       DoubleArg            -> ( (gpr, fpr, astack + 2), stackLong astack ofs )

       LongArg | even gpr   -> loadparam (gpr + 1, fpr, stack) (ofs, rep)
       LongArg | gpr <= 9   -> ( (gpr + 2, fpr, stack),  loadLong gpr ofs )
       LongArg              -> ( (gpr, fpr, astack + 2), stackLong astack ofs )

       _ | gpr <= 10        -> ( (gpr + 1, fpr, stack),  loadWord gpr ofs )
       _                    -> ( (gpr, fpr, stack + 1),  stackWord stack ofs )
      where astack = alignedTo 2 stack

     alignedTo :: Int -> Int -> Int
     alignedTo alignment x = case x `mod` alignment of
                               0 -> x
                               y -> x - y + alignment

     -- convenience macros to do multiple-instruction data moves
     stackWord dst src = loadWord 0 src ++ storeWordC 0 dst
     stackLong dst src = stackWord dst src ++ stackWord (dst + 1) (src + 1)
     loadLong  dst src = loadWord dst src ++ loadWord (dst + 1) (src + 1)
     storeLong dst src = storeWord dst src ++ storeWord (dst + 1) (src + 1)

     -- load data from the Haskell stack (relative to r31)
     loadFloat   = loadstoreInstr 0xc01f0000 -- lfs fpr, ofs(r31)
     loadDouble  = loadstoreInstr 0xc81f0000 -- lfd fpr, ofs(r31)
     loadWord    = loadstoreInstr 0x801f0000 -- lwz gpr, ofs(r31)

     -- store data to the Haskell stack (relative to r31)
     storeFloat  = loadstoreInstr 0xd01f0000 -- stfs fpr, ofs(r31)
     storeDouble = loadstoreInstr 0xd81f0000 -- stfd fpr, ofs(r31)
     storeWord   = loadstoreInstr 0x901f0000 -- stw gpr, ofs(r31)

     -- store data to the C stack (relative to r1)
     storeWordC  = loadstoreInstr 0x90010000 -- stw gpr, ofs(r1)

     -- machine code building blocks
     loadstoreInstr :: Word32 -> Int -> Int -> [Word32]
     loadstoreInstr code reg ofs = [ code .|. register reg .|. offset ofs ]

     register :: Int -> Word32
     register reg = fromIntegral reg `shiftL` 21

     offset :: Int -> Word32
     offset ofs   = fromIntegral (ofs * 4) .&. 0xFFFF

     -- speaks for itself
     w32_to_w8s_bigEndian :: Word32 -> [Word8]
     w32_to_w8s_bigEndian w =  [fromIntegral (0xFF .&. (w `shiftR` 24)),
                                fromIntegral (0xFF .&. (w `shiftR` 16)),
                                fromIntegral (0xFF .&. (w `shiftR` 8)),
                                fromIntegral (0xFF .&. w)]

#else 

   = throwDyn (InstallationError "foreign import is not implemented for GHCi on this platform.")

#endif

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
lit32 :: Int -> [Word8]
lit32 i = let w32 = (fromIntegral i) :: Word32
          in  map (fromIntegral . ( .&. 0xFF))
                  [w32, w32 `shiftR` 8, 
                   w32 `shiftR` 16,  w32 `shiftR` 24]
#endif
\end{code}

