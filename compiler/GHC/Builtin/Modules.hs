module GHC.Builtin.Modules
   ( module GHC.Builtin.Modules )
   where

import GHC.Prelude

import GHC.Unit.Types

import GHC.Data.FastString

import Language.Haskell.Syntax.Module.Name

{-
************************************************************************
*                                                                      *
\subsection{Module names}
*                                                                      *
************************************************************************


--MetaHaskell Extension Add a new module here
-}

gHC_PRIM, gHC_PRIM_PANIC, gHC_TYPES, gHC_MAGIC,
  gHC_MAGIC_DICT, gHC_CLASSES, gHC_PRIMOPWRAPPERS :: Module
gHC_PRIM           = mkGhcInternalModule (fsLit "GHC.Internal.Prim")   -- Primitive types and values
gHC_PRIM_PANIC     = mkGhcInternalModule (fsLit "GHC.Internal.Prim.Panic")
gHC_TYPES          = mkGhcInternalModule (fsLit "GHC.Internal.Types")
gHC_MAGIC          = mkGhcInternalModule (fsLit "GHC.Internal.Magic")
gHC_MAGIC_DICT     = mkGhcInternalModule (fsLit "GHC.Internal.Magic.Dict")
gHC_CLASSES        = mkGhcInternalModule (fsLit "GHC.Internal.Classes")
gHC_PRIMOPWRAPPERS = mkGhcInternalModule (fsLit "GHC.Internal.PrimopWrappers")
gHC_INTERNAL_TUPLE = mkGhcInternalModule (fsLit "GHC.Internal.Tuple")

gHC_INTERNAL_NUM_INTEGER, gHC_INTERNAL_NUM_NATURAL, gHC_INTERNAL_NUM_BIGNAT :: Module
gHC_INTERNAL_NUM_INTEGER            = mkGhcInternalModule (fsLit "GHC.Internal.Bignum.Integer")
gHC_INTERNAL_NUM_NATURAL            = mkGhcInternalModule (fsLit "GHC.Internal.Bignum.Natural")
gHC_INTERNAL_NUM_BIGNAT             = mkGhcInternalModule (fsLit "GHC.Internal.Bignum.BigNat")

gHC_INTERNAL_BASE, gHC_INTERNAL_GHCI_HELPERS, gHC_INTERNAL_MAYBE,
  gHC_INTERNAL_TUPLE, gHC_INTERNAL_DATA_TRAVERSABLE, gHC_INTERNAL_ERR,
  gHC_INTERNAL_WORD, gHC_INTERNAL_MONAD_FAIL,
  gHC_INTERNAL_CONTROL_EXCEPTION_BASE, gHC_INTERNAL_TYPEERROR,
  gHC_INTERNAL_TYPELITS, gHC_INTERNAL_TYPELITS_INTERNAL, gHC_INTERNAL_TYPENATS,
  gHC_INTERNAL_TYPENATS_INTERNAL, gHC_INTERNAL_UNSAFE_COERCE :: Module
gHC_INTERNAL_BASE                   = mkGhcInternalModule (fsLit "GHC.Internal.Base")
gHC_INTERNAL_GHCI_HELPERS           = mkGhcInternalModule (fsLit "GHC.Internal.GHCi.Helpers")
gHC_INTERNAL_MAYBE                  = mkGhcInternalModule (fsLit "GHC.Internal.Maybe")
gHC_INTERNAL_DATA_TRAVERSABLE       = mkGhcInternalModule (fsLit "GHC.Internal.Data.Traversable")
gHC_INTERNAL_ERR                    = mkGhcInternalModule (fsLit "GHC.Internal.Err")
gHC_INTERNAL_WORD                   = mkGhcInternalModule (fsLit "GHC.Internal.Word")
gHC_INTERNAL_MONAD_FAIL             = mkGhcInternalModule (fsLit "GHC.Internal.Control.Monad.Fail")
gHC_INTERNAL_CONTROL_EXCEPTION_BASE = mkGhcInternalModule (fsLit "GHC.Internal.Control.Exception.Base")
gHC_INTERNAL_TYPEERROR              = mkGhcInternalModule (fsLit "GHC.Internal.TypeError")
gHC_INTERNAL_TYPELITS               = mkGhcInternalModule (fsLit "GHC.Internal.TypeLits")
gHC_INTERNAL_TYPELITS_INTERNAL      = mkGhcInternalModule (fsLit "GHC.Internal.TypeLits.Internal")
gHC_INTERNAL_TYPENATS               = mkGhcInternalModule (fsLit "GHC.Internal.TypeNats")
gHC_INTERNAL_TYPENATS_INTERNAL      = mkGhcInternalModule (fsLit "GHC.Internal.TypeNats.Internal")
gHC_INTERNAL_UNSAFE_COERCE          = mkGhcInternalModule (fsLit "GHC.Internal.Unsafe.Coerce")

rOOT_MAIN :: Module
rOOT_MAIN       = mkMainModule (fsLit ":Main") -- Root module for initialisation

mkInteractiveModule :: String -> Module
-- (mkInteractiveMoudule "9") makes module 'interactive:Ghci9'
mkInteractiveModule n = mkModule interactiveUnit (mkModuleName ("Ghci" ++ n))

pRELUDE_NAME, mAIN_NAME, eSSENTIALS_NAME :: ModuleName
pRELUDE_NAME    = mkModuleNameFS (fsLit "Prelude")
mAIN_NAME       = mkModuleNameFS (fsLit "Main")
eSSENTIALS_NAME = mkModuleNameFS (fsLit "GHC.Essentials")

rEBINDABLE_MOD_NAME :: ModuleName
rEBINDABLE_MOD_NAME = mkModuleName "Rebindable"

mkGhcInternalModule :: FastString -> Module
mkGhcInternalModule m = mkGhcInternalModule_ (mkModuleNameFS m)

mkGhcInternalModule_ :: ModuleName -> Module
mkGhcInternalModule_ m = mkModule ghcInternalUnit m

mkThisGhcModule :: FastString -> Module
mkThisGhcModule m = mkThisGhcModule_ (mkModuleNameFS m)

mkThisGhcModule_ :: ModuleName -> Module
mkThisGhcModule_ m = mkModule thisGhcUnit m

mkMainModule :: FastString -> Module
mkMainModule m = mkModule mainUnit (mkModuleNameFS m)

mkMainModule_ :: ModuleName -> Module
mkMainModule_ m = mkModule mainUnit m

