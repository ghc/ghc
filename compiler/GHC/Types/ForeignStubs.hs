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
        , hsep [text "void", pprCLabel platform CStyle clbl, text "(void)"]
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
finalizerCStub :: Platform -> CLabel -> SDoc -> SDoc -> CStub
finalizerCStub platform clbl declarations body =
    functionCStub platform clbl declarations body
    `mappend` CStub empty [] [clbl]

newtype CHeader = CHeader { getCHeader :: SDoc }

instance Monoid CHeader where
  mempty = CHeader empty
  mconcat = coerce vcat

instance Semigroup CHeader where
    (<>) = coerce ($$)

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
