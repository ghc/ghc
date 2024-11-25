-- | Info about modules in the "home" unit.
-- Stored in a 'HomePackageTable'.
module GHC.Unit.Home.ModInfo
   (
     HomeModInfo (..)
   , HomeModLinkable(..)
   , homeModInfoObject
   , homeModInfoByteCode
   , emptyHomeModInfoLinkable
   , justBytecode
   , justObjects
   , bytecodeAndObjects
   )
where

import GHC.Prelude

import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails

import GHC.Linker.Types ( Linkable(..), linkableIsNativeCodeOnly )

import GHC.Utils.Outputable
import GHC.Utils.Panic


-- | Information about modules in the package being compiled
data HomeModInfo = HomeModInfo
   { hm_iface    :: !ModIface
        -- ^ The basic loaded interface file: every loaded module has one of
        -- these, even if it is imported from another package

   , hm_details  :: ModDetails
        -- ^ Extra information that has been created from the 'ModIface' for
        -- the module, typically during typechecking

        -- This field is LAZY because a ModDetails is constructed by knot tying.

   , hm_linkable :: !HomeModLinkable
        -- ^ The actual artifact we would like to link to access things in
        -- this module. See Note [Home module build products]
        --
        -- 'hm_linkable' might be empty:
        --
        --   1. If this is an .hs-boot module
        --
        --   2. Temporarily during compilation if we pruned away
        --      the old linkable because it was out of date.
        --
        -- When re-linking a module ('GHC.Driver.Main.HscNoRecomp'), we construct the
        -- 'HomeModInfo' by building a new 'ModDetails' from the old
        -- 'ModIface' (only).
   }

homeModInfoByteCode :: HomeModInfo -> Maybe Linkable
homeModInfoByteCode = homeMod_bytecode . hm_linkable

homeModInfoObject :: HomeModInfo -> Maybe Linkable
homeModInfoObject = homeMod_object . hm_linkable

emptyHomeModInfoLinkable :: HomeModLinkable
emptyHomeModInfoLinkable = HomeModLinkable Nothing Nothing

-- See Note [Home module build products]
data HomeModLinkable = HomeModLinkable { homeMod_bytecode :: !(Maybe Linkable)
                                       , homeMod_object   :: !(Maybe Linkable) }

instance Outputable HomeModLinkable where
  ppr (HomeModLinkable l1 l2) = ppr l1 $$ ppr l2

justBytecode :: Linkable -> HomeModLinkable
justBytecode lm =
  assertPpr (not (linkableIsNativeCodeOnly lm)) (ppr lm)
   $ emptyHomeModInfoLinkable { homeMod_bytecode = Just lm }

justObjects :: Linkable -> HomeModLinkable
justObjects lm =
  assertPpr (linkableIsNativeCodeOnly lm) (ppr lm)
   $ emptyHomeModInfoLinkable { homeMod_object = Just lm }

bytecodeAndObjects :: Linkable -> Linkable -> HomeModLinkable
bytecodeAndObjects bc o =
  assertPpr (not (linkableIsNativeCodeOnly bc) && linkableIsNativeCodeOnly o) (ppr bc $$ ppr o)
    (HomeModLinkable (Just bc) (Just o))


{-
Note [Home module build products]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When compiling a home module we can produce some combination of the following
build products.

1. A byte code linkable, for use with the byte code interpreter.
2. An object file linkable, for linking a final executable or the byte code interpreter

What we have produced is recorded in the `HomeModLinkable` type. In the case
that these linkables are produced they are stored in the relevant field so that
subsequent modules can retrieve and use them as necessary.

* `-fbyte-code` will *only* produce a byte code linkable. This is the default in GHCi.
* `-fobject-code` will *only* produce an object file linkable. This is the default in -c and --make mode.
* `-fbyte-code-and-object-code` produces both a byte-code and object file linkable. So both fields are populated.

Why would you want to produce both an object file and byte code linkable? If you
also want to use `-fprefer-byte-code` then you should probably also use this
flag to make sure that byte code is generated for your modules.

-}
