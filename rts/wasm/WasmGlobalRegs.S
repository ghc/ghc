/* Note [WASM GlobalRegs Linking]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * WASM has unique requirements for STG GlobalRegs:
 *
 * 1. Executables (both static and dynamic):
 *    - Need GlobalRegs injected at final link time
 *    - This file (WasmGlobalRegs.S) is compiled on-demand by GHC during linking
 *    - GlobalRegs become part of the executable .wasm binary
 *    - See mkWasmGlobalRegsObj in compiler/GHC/Linker/Executable.hs
 *
 * 2. Libraries and GHCi (runtime dynamic linking):
 *    - GlobalRegs are supplied by dyld.mjs at runtime
 *    - This file is NOT compiled or linked
 *    - dyld.mjs provides them per WASM dynamic linking convention
 *    - Including GlobalRegs in libraries causes "GOT.mem" errors
 *
 * This on-demand compilation approach is cleaner than including GlobalRegs
 * in the RTS because:
 * - GlobalRegs are never in the RTS libraries (avoids conflicts with dyld.mjs)
 * - Only executables get GlobalRegs (exactly when needed)
 * - Clear separation: libraries use dyld.mjs, executables use this file
 * - No conditional compilation or build-time flags needed
 *
 * Build strategy:
 * - RTS is built WITHOUT this file (see rts/rts.cabal)
 * - When linking an executable, GHC:
 *   1. Reads this source file
 *   2. Compiles it to WasmGlobalRegs.o in a temp directory
 *   3. Adds WasmGlobalRegs.o to the linker command
 * - Libraries and GHCi never see this file
 *
 * Historical context:
 * - Originally, Wasm.S had #if !defined(__PIC__) guards around GlobalRegs
 * - Commit 98a32ec551dd (Oct 2024, Cheng Shao): Added guards for PIC mode
 * - PR 142 removed guards (incorrectly)
 * - This implementation uses on-demand compilation for better control
 *
 * See also:
 * - Issue #134: https://github.com/stable-haskell/ghc/issues/134
 * - compiler/GHC/Linker/Executable.hs: mkWasmGlobalRegsObj function
 * - compiler/GHC/Driver/Session.hs:3730-3742: WASM WayDyn handling
 * - compiler/GHC/Runtime/Interpreter/Types.hs:161: ExtWasm dynamic requirement
 * - compiler/GHC/Linker/Dynamic.hs:263: --experimental-pic flag
 * - rts/wasm/Wasm.S: Common WASM definitions
 */

/* STG Global Registers
 * --------------------
 * These WASM globals represent the STG machine registers.
 * Only linked into static executables, not dynamic RTS.
 *
 * Word-sized registers (Rn, Sp, SpLim, Hp, HpLim) use WASM type i32
 * because this file targets wasm32 where the platform word is 32 bits.
 * A hypothetical wasm64 target would use i64 instead.
 *
 * Note: we avoid CPP macros here (no #include, no #define W_) so that
 * this file can be assembled directly as a .s file without preprocessing.
 * See mkWasmGlobalRegsObj in compiler/GHC/Linker/Executable.hs.
 */

	.hidden __R1
	.globl __R1
	.section .data.__R1,"",@
	.globaltype __R1, i32
__R1:

	.hidden __R2
	.globl __R2
	.section .data.__R2,"",@
	.globaltype __R2, i32
__R2:

	.hidden __R3
	.globl __R3
	.section .data.__R3,"",@
	.globaltype __R3, i32
__R3:

	.hidden __R4
	.globl __R4
	.section .data.__R4,"",@
	.globaltype __R4, i32
__R4:

	.hidden __R5
	.globl __R5
	.section .data.__R5,"",@
	.globaltype __R5, i32
__R5:

	.hidden __R6
	.globl __R6
	.section .data.__R6,"",@
	.globaltype __R6, i32
__R6:

	.hidden __R7
	.globl __R7
	.section .data.__R7,"",@
	.globaltype __R7, i32
__R7:

	.hidden __R8
	.globl __R8
	.section .data.__R8,"",@
	.globaltype __R8, i32
__R8:

	.hidden __R9
	.globl __R9
	.section .data.__R9,"",@
	.globaltype __R9, i32
__R9:

	.hidden __R10
	.globl __R10
	.section .data.__R10,"",@
	.globaltype __R10, i32
__R10:

	.hidden __F1
	.globl __F1
	.section .data.__F1,"",@
	.globaltype __F1, f32
__F1:

	.hidden __F2
	.globl __F2
	.section .data.__F2,"",@
	.globaltype __F2, f32
__F2:

	.hidden __F3
	.globl __F3
	.section .data.__F3,"",@
	.globaltype __F3, f32
__F3:

	.hidden __F4
	.globl __F4
	.section .data.__F4,"",@
	.globaltype __F4, f32
__F4:

	.hidden __F5
	.globl __F5
	.section .data.__F5,"",@
	.globaltype __F5, f32
__F5:

	.hidden __F6
	.globl __F6
	.section .data.__F6,"",@
	.globaltype __F6, f32
__F6:

	.hidden __D1
	.globl __D1
	.section .data.__D1,"",@
	.globaltype __D1, f64
__D1:

	.hidden __D2
	.globl __D2
	.section .data.__D2,"",@
	.globaltype __D2, f64
__D2:

	.hidden __D3
	.globl __D3
	.section .data.__D3,"",@
	.globaltype __D3, f64
__D3:

	.hidden __D4
	.globl __D4
	.section .data.__D4,"",@
	.globaltype __D4, f64
__D4:

	.hidden __D5
	.globl __D5
	.section .data.__D5,"",@
	.globaltype __D5, f64
__D5:

	.hidden __D6
	.globl __D6
	.section .data.__D6,"",@
	.globaltype __D6, f64
__D6:

	.hidden __L1
	.globl __L1
	.section .data.__L1,"",@
	.globaltype __L1, i64
__L1:

	.hidden __Sp
	.globl __Sp
	.section .data.__Sp,"",@
	.globaltype __Sp, i32
__Sp:

	.hidden __SpLim
	.globl __SpLim
	.section .data.__SpLim,"",@
	.globaltype __SpLim, i32
__SpLim:

	.hidden __Hp
	.globl __Hp
	.section .data.__Hp,"",@
	.globaltype __Hp, i32
__Hp:

	.hidden __HpLim
	.globl __HpLim
	.section .data.__HpLim,"",@
	.globaltype __HpLim, i32
__HpLim:
