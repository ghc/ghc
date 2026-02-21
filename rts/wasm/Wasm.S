/* Note [WASM Assembly Organization]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * WASM assembly is split into multiple files:
 *
 * - Wasm.S (this file): Common WASM definitions and utilities
 *   - Always linked into the RTS
 *   - Contains shared definitions like W_ type macro
 *   - Contains other WASM-specific code (if needed in future)
 *
 * - WasmGlobalRegs.S: STG GlobalRegs definitions
 *   - NOT part of the RTS (not in rts.cabal)
 *   - Compiled on-demand when linking executables
 *   - See Note [WASM GlobalRegs Linking] in WasmGlobalRegs.S
 *   - See mkWasmGlobalRegsObj in compiler/GHC/Linker/Executable.hs
 *
 * Why the split?
 * - RTS must NOT contain GlobalRegs (conflicts with dyld.mjs for GHCi)
 * - Executables need GlobalRegs injected at link time
 * - GHCi/libraries get GlobalRegs from dyld.mjs at runtime
 *
 * Historical context:
 * - Previously, GlobalRegs were in this file with #if !defined(__PIC__) guards
 * - Commit 98a32ec551dd (Oct 2024, Cheng Shao): Added guards for PIC mode
 * - PR 142 removed the guards (incorrectly)
 * - This implementation uses on-demand compilation for correct behavior
 *
 * See also:
 * - Issue #134: https://github.com/stable-haskell/ghc/issues/134
 * - rts/wasm/WasmGlobalRegs.S: GlobalRegs definitions and detailed documentation
 */

#include "ghcconfig.h"
#include "rts/Constants.h"
#include "DerivedConstants.h"

/* Type definition for word-sized WASM types
 * W_ is i32 on 32-bit platforms, i64 on 64-bit platforms
 */
#if SIZEOF_VOID_P == 4
#define W_ i32
#else
#define W_ i64
#endif

/* Future WASM-specific assembly code would go here
 * Currently, only the W_ definition is needed in this file.
 * All GlobalRegs have been moved to WasmGlobalRegs.S.
 */
