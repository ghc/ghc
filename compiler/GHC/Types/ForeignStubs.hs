-- | Foreign export stubs
{-# LANGUAGE DerivingVia #-}
module GHC.Types.ForeignStubs
   ( ForeignStubs (..)
   , CHeader(..)
   , CStub(..)
   , initializerCStub
   , finalizerCStub
   , appendStubC
   )
where

import {-# SOURCE #-} GHC.Cmm.CLabel

import GHC.Platform
import GHC.Utils.Outputable
import Data.List ((++))
import Data.Monoid
import Data.Semigroup
import Data.Coerce

data CStub = CStub { getCStub :: SDoc
                   , getInitializers :: [CLabel]
                     -- ^ Initializers to be run at startup
                     -- See Note [Initializers and finalizers in Cmm] in
                     -- "GHC.Cmm.InitFini".
                   , getFinalizers :: [CLabel]
                     -- ^ Finalizers to be run at shutdown
                   }

emptyCStub :: CStub
emptyCStub = CStub empty [] []

instance Monoid CStub where
  mempty = emptyCStub

instance Semigroup CStub where
  CStub a0 b0 c0 <> CStub a1 b1 c1 =
      CStub (a0 $$ a1) (b0 ++ b1) (c0 ++ c1)

functionCStub :: Platform -> CLabel -> SDoc -> SDoc -> CStub
functionCStub platform clbl declarations body =
    CStub body' [] []
  where
    body' = vcat
        [ declarations
        , hsep [text "void", pprCLabel platform clbl, text "(void)"]
        , braces body
        ]

-- | @initializerCStub fn_nm decls body@ is a 'CStub' containing C initializer
-- function (e.g. an entry of the @.init_array@ section) named
-- @fn_nm@ with the given body and the given set of declarations.
initializerCStub :: Platform -> CLabel -> SDoc -> SDoc -> CStub
initializerCStub platform clbl declarations body =
    functionCStub platform clbl declarations body
    `mappend` CStub empty [clbl] []

-- | @finalizerCStub fn_nm decls body@ is a 'CStub' containing C finalizer
-- function (e.g. an entry of the @.fini_array@ section) named
-- @fn_nm@ with the given body and the given set of declarations.
--
-- See Note [Finalizers via __cxa_atexit]
finalizerCStub :: Platform -> CLabel -> SDoc -> SDoc -> CStub
finalizerCStub platform clbl declarations body
  | ArchWasm32 <- platformArch platform
  = -- See Note [Finalizers via __cxa_atexit]
    cxaAtexitFinalizerCStub platform clbl declarations body
finalizerCStub platform clbl declarations body
  | OSDarwin <- platformOS platform
  = -- See Note [Finalizers via __cxa_atexit]
    cxaAtexitFinalizerCStub platform clbl declarations body
finalizerCStub platform clbl declarations body
  = functionCStub platform clbl declarations body
    `mappend` CStub empty [] [clbl]

-- | Generate a @__cxa_atexit@-based finalizer.
-- See Note [Finalizers via __cxa_atexit]
cxaAtexitFinalizerCStub :: Platform -> CLabel -> SDoc -> SDoc -> CStub
cxaAtexitFinalizerCStub platform clbl declarations body =
    let clbl_pretty = pprCLabel platform clbl
        fini_name    = hcat [clbl_pretty, text "$fini"]
        wrapper_name = hcat [clbl_pretty, text "$fini_atexit"]
        c_code = vcat
          [ declarations
          , text "int __cxa_atexit(void (*)(void *), void *, void *);"
          , hcat [text "static void ", fini_name, text "(void)"]
          , braces body
          , hcat [text "static void ", wrapper_name, text "(void *arg __attribute__((unused)))"]
          , braces (hcat [fini_name, text "();"])
          , hsep [text "void", clbl_pretty, text "(void)"]
          , braces (hcat [text "__cxa_atexit(", wrapper_name, text ", 0, 0);"])
          ]
    in CStub c_code [clbl] []

{-
Note [Finalizers via __cxa_atexit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On some platforms, placing a function pointer in the .fini_array /
__mod_term_func section is not sufficient to have it called on exit.
On these platforms we instead lower finalizers as initializers that register
the actual finalizer function via __cxa_atexit.

Affected platforms:

  Wasm32: does not support .fini_array sections.

  Darwin: modern macOS dyld no longer processes __DATA,__mod_term_func entries.
  Clang now lowers __attribute__((destructor)) as an initializer that calls
  __cxa_atexit, placing the initializer in __DATA,__mod_init_func (which the
  linker converts to __TEXT,__init_offsets). GHC must follow the same pattern.

For a finalizer with label `clbl` and body `body`, on these platforms we
generate:

    static void clbl$fini(void) {
        <body>
    }
    static void clbl$fini_atexit(void *arg) {
        clbl$fini();
    }
    void clbl(void) {
        __cxa_atexit(clbl$fini_atexit, 0, 0);
    }

The function `clbl` is placed in the initializers list (getInitializers)
instead of the finalizers list (getFinalizers). During code output,
emitInitializerDecls places it in .init_array / __mod_init_func, so the
registration runs at startup.

The actual finalizer body is in the static helper `clbl$fini`. A separate
wrapper `clbl$fini_atexit` with the void(*)(void*) signature expected by
__cxa_atexit is needed because some platforms (e.g. wasm32) enforce exact
function signature matching at call sites — a simple cast would trap at
runtime.

This matches what clang does when lowering __attribute__((destructor)) on
these platforms.
-}

newtype CHeader = CHeader { getCHeader :: SDoc }

instance Monoid CHeader where
  mempty = CHeader empty
  mconcat = coerce (vcat @SDoc)

instance Semigroup CHeader where
    (<>) = coerce (($$) @SDoc)

-- | Foreign export stubs
data ForeignStubs
  = NoStubs
      -- ^ We don't have any stubs
  | ForeignStubs CHeader CStub
      -- ^ There are some stubs. Parameters:
      --
      --  1) Header file prototypes for
      --     "foreign exported" functions
      --
      --  2) C stubs to use when calling
      --     "foreign exported" functions

appendStubC :: ForeignStubs -> CStub -> ForeignStubs
appendStubC NoStubs         c_code = ForeignStubs mempty c_code
appendStubC (ForeignStubs h c) c_code = ForeignStubs h (c `mappend` c_code)
