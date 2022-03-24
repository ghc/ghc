-- | Utilities for dealing with constructors/destructors.
module GHC.Cmm.InitFini
    ( InitOrFini(..)
    , isInitOrFiniArray
    ) where

import GHC.Prelude

import GHC.Cmm.CLabel
import GHC.Cmm
import GHC.Utils.Panic
import GHC.Utils.Outputable

{-
Note [Initializers and finalizers in Cmm]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most platforms support some mechanism for marking a procedure to be run when a
program is loaded (in which case the procedure is known as an "initializer",
"constructor", or "ctor") or unloaded (a "finalizer", "deconstructor", or
"dtor").

For instance, on ELF platforms pointers to initializer and finalizer functions
are listed in .init_array and .fini_array sections, which are traversed by libc
during program startup and shutdown.

In GHC-generated code, initializers are used for a few things:

 * registration of cost-centres and cost-centre stacks for profiling
 * registration of info-table provenance entries
 * registration of ticky tickers
 * registration of HPC ticks

All of these initializers are implemented as C functions, emitted by the
compiler as ForeignStubs. Consequently the GHC.Types.ForeignStubs.CStub type
carries with it lists of functions which should be marked as initializers or
finalizers.

These initializer and finalizer lists are then turned into CmmData declarations
which are fed to the backend. These declarations are distinguished by their
Section (e.g. InitArray or FiniArray) and consist of an array of words, where each
word is a pointer to an initializer/finalizer function. Since this is the same
form that most platforms expect initializer or finalizer lists to appear in
assembler, the NCG backends naturally emit the appropriate assembler.

However, for non-NCG backends (e.g. the C and LLVM backends) these
initializer/finalizer list declarations need to be detected and dealt with
appropriately. We provide isInitOrFiniArray to distinguish such declarations
and turn them back into a list of CLabels.

On Windows initializers/finalizers are a bit tricky due to the inability to
merge objects (due to the lld linker's lack of `-r` support on Windows; see
Note [Object merging] in GHC.Driver.Pipeline.Execute) since we instead must
package foreign stubs into static archives.  However, the linker is free to not
include any constituent objects of a static library in the final object code if
nothing depends upon them. Consequently, we must ensure that the initializer
list for a module is defined in the module's object code, not its foreign
stubs. This happens naturally with the plan laid out above.

Note that we maintain the invariant that at most one initializer and one
finalizer CmmDecl will be emitted per module.
-}

data InitOrFini = IsInitArray | IsFiniArray

isInitOrFiniArray :: RawCmmDecl -> Maybe (InitOrFini, [CLabel])
isInitOrFiniArray (CmmData sect (CmmStaticsRaw _ lits))
  | Just initOrFini <- isInitOrFiniSection sect
  = Just (initOrFini, map get_label lits)
  where
    get_label :: CmmStatic -> CLabel
    get_label (CmmStaticLit (CmmLabel lbl)) = lbl
    get_label static = pprPanic "isInitOrFiniArray: invalid entry" (ppr static)
isInitOrFiniArray _ = Nothing

isInitOrFiniSection :: Section -> Maybe InitOrFini
isInitOrFiniSection (Section InitArray _) = Just IsInitArray
isInitOrFiniSection (Section FiniArray _) = Just IsFiniArray
isInitOrFiniSection _                     = Nothing
