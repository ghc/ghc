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

gHC_PRIM, gHC_PRIM_PANIC,
    gHC_TYPES, gHC_INTERNAL_DATA_DATA, gHC_MAGIC, gHC_MAGIC_DICT,
    gHC_CLASSES, gHC_CLASSES_IP, gHC_PRIMOPWRAPPERS :: Module
gHC_PRIM           = mkGhcInternalModule (fsLit "GHC.Internal.Prim")   -- Primitive types and values
gHC_PRIM_PANIC     = mkGhcInternalModule (fsLit "GHC.Internal.Prim.Panic")
gHC_TYPES          = mkGhcInternalModule (fsLit "GHC.Internal.Types")
gHC_MAGIC          = mkGhcInternalModule (fsLit "GHC.Internal.Magic")
gHC_MAGIC_DICT     = mkGhcInternalModule (fsLit "GHC.Internal.Magic.Dict")
gHC_CSTRING        = mkGhcInternalModule (fsLit "GHC.Internal.CString")
gHC_CLASSES        = mkGhcInternalModule (fsLit "GHC.Internal.Classes")
gHC_CLASSES_IP     = mkGhcInternalModule (fsLit "GHC.Internal.Classes.IP")
gHC_PRIMOPWRAPPERS = mkGhcInternalModule (fsLit "GHC.Internal.PrimopWrappers")
gHC_INTERNAL_TUPLE = mkGhcInternalModule (fsLit "GHC.Internal.Tuple")

gHC_INTERNAL_CONTROL_MONAD_ZIP :: Module
gHC_INTERNAL_CONTROL_MONAD_ZIP  = mkGhcInternalModule (fsLit "GHC.Internal.Control.Monad.Zip")

gHC_INTERNAL_NUM_INTEGER, gHC_INTERNAL_NUM_NATURAL, gHC_INTERNAL_NUM_BIGNAT :: Module
gHC_INTERNAL_NUM_INTEGER            = mkGhcInternalModule (fsLit "GHC.Internal.Bignum.Integer")
gHC_INTERNAL_NUM_NATURAL            = mkGhcInternalModule (fsLit "GHC.Internal.Bignum.Natural")
gHC_INTERNAL_NUM_BIGNAT             = mkGhcInternalModule (fsLit "GHC.Internal.Bignum.BigNat")

gHC_INTERNAL_BASE, gHC_INTERNAL_ENUM,
    gHC_INTERNAL_GHCI, gHC_INTERNAL_GHCI_HELPERS, gHC_CSTRING, gHC_INTERNAL_DATA_STRING,
    gHC_INTERNAL_SHOW, gHC_INTERNAL_READ, gHC_INTERNAL_NUM, gHC_INTERNAL_MAYBE,
    gHC_INTERNAL_LIST, gHC_INTERNAL_TUPLE, gHC_INTERNAL_DATA_EITHER,
    gHC_INTERNAL_DATA_FOLDABLE, gHC_INTERNAL_DATA_TRAVERSABLE,
    gHC_INTERNAL_EXCEPTION_CONTEXT,
    gHC_INTERNAL_CONC, gHC_INTERNAL_IO, gHC_INTERNAL_IO_Exception,
    gHC_INTERNAL_ST, gHC_INTERNAL_IX, gHC_INTERNAL_STABLE, gHC_INTERNAL_PTR, gHC_INTERNAL_ERR, gHC_INTERNAL_REAL,
    gHC_INTERNAL_FLOAT, gHC_INTERNAL_TOP_HANDLER, gHC_INTERNAL_SYSTEM_IO, gHC_INTERNAL_DYNAMIC,
    gHC_INTERNAL_TYPEABLE, gHC_INTERNAL_TYPEABLE_INTERNAL, gHC_INTERNAL_GENERICS,
    gHC_INTERNAL_READ_PREC, gHC_INTERNAL_LEX, gHC_INTERNAL_INT, gHC_INTERNAL_WORD, gHC_INTERNAL_MONAD, gHC_INTERNAL_MONAD_FIX,  gHC_INTERNAL_MONAD_FAIL,
    gHC_INTERNAL_ARROW, gHC_INTERNAL_DESUGAR, gHC_INTERNAL_RANDOM, gHC_INTERNAL_EXTS,
    gHC_INTERNAL_CONTROL_EXCEPTION_BASE, gHC_INTERNAL_TYPEERROR, gHC_INTERNAL_TYPELITS, gHC_INTERNAL_TYPELITS_INTERNAL,
    gHC_INTERNAL_TYPENATS, gHC_INTERNAL_TYPENATS_INTERNAL,
    gHC_INTERNAL_DATA_COERCE, gHC_INTERNAL_DEBUG_TRACE, gHC_INTERNAL_UNSAFE_COERCE, gHC_INTERNAL_FOREIGN_C_CONSTPTR,
    gHC_INTERNAL_JS_PRIM, gHC_INTERNAL_WASM_PRIM_TYPES :: Module
gHC_INTERNAL_BASE                   = mkGhcInternalModule (fsLit "GHC.Internal.Base")
gHC_INTERNAL_ENUM                   = mkGhcInternalModule (fsLit "GHC.Internal.Enum")
gHC_INTERNAL_GHCI                   = mkGhcInternalModule (fsLit "GHC.Internal.GHCi")
gHC_INTERNAL_GHCI_HELPERS           = mkGhcInternalModule (fsLit "GHC.Internal.GHCi.Helpers")
gHC_INTERNAL_SHOW                   = mkGhcInternalModule (fsLit "GHC.Internal.Show")
gHC_INTERNAL_READ                   = mkGhcInternalModule (fsLit "GHC.Internal.Read")
gHC_INTERNAL_NUM                    = mkGhcInternalModule (fsLit "GHC.Internal.Num")
gHC_INTERNAL_MAYBE                  = mkGhcInternalModule (fsLit "GHC.Internal.Maybe")
gHC_INTERNAL_LIST                   = mkGhcInternalModule (fsLit "GHC.Internal.List")
gHC_INTERNAL_DATA_EITHER            = mkGhcInternalModule (fsLit "GHC.Internal.Data.Either")
gHC_INTERNAL_DATA_STRING            = mkGhcInternalModule (fsLit "GHC.Internal.Data.String")
gHC_INTERNAL_DATA_FOLDABLE          = mkGhcInternalModule (fsLit "GHC.Internal.Data.Foldable")
gHC_INTERNAL_DATA_TRAVERSABLE       = mkGhcInternalModule (fsLit "GHC.Internal.Data.Traversable")
gHC_INTERNAL_CONC                   = mkGhcInternalModule (fsLit "GHC.Internal.GHC.Conc")
gHC_INTERNAL_IO                     = mkGhcInternalModule (fsLit "GHC.Internal.IO")
gHC_INTERNAL_IO_Exception           = mkGhcInternalModule (fsLit "GHC.Internal.IO.Exception")
gHC_INTERNAL_ST                     = mkGhcInternalModule (fsLit "GHC.Internal.ST")
gHC_INTERNAL_IX                     = mkGhcInternalModule (fsLit "GHC.Internal.Ix")
gHC_INTERNAL_STABLE                 = mkGhcInternalModule (fsLit "GHC.Internal.Stable")
gHC_INTERNAL_PTR                    = mkGhcInternalModule (fsLit "GHC.Internal.Ptr")
gHC_INTERNAL_ERR                    = mkGhcInternalModule (fsLit "GHC.Internal.Err")
gHC_INTERNAL_REAL                   = mkGhcInternalModule (fsLit "GHC.Internal.Real")
gHC_INTERNAL_FLOAT                  = mkGhcInternalModule (fsLit "GHC.Internal.Float")
gHC_INTERNAL_TOP_HANDLER            = mkGhcInternalModule (fsLit "GHC.Internal.TopHandler")
gHC_INTERNAL_SYSTEM_IO              = mkGhcInternalModule (fsLit "GHC.Internal.System.IO")
gHC_INTERNAL_DYNAMIC                = mkGhcInternalModule (fsLit "GHC.Internal.Data.Dynamic")
gHC_INTERNAL_TYPEABLE               = mkGhcInternalModule (fsLit "GHC.Internal.Data.Typeable")
gHC_INTERNAL_TYPEABLE_INTERNAL      = mkGhcInternalModule (fsLit "GHC.Internal.Data.Typeable.Internal")
gHC_INTERNAL_DATA_DATA              = mkGhcInternalModule (fsLit "GHC.Internal.Data.Data")
gHC_INTERNAL_READ_PREC              = mkGhcInternalModule (fsLit "GHC.Internal.Text.ParserCombinators.ReadPrec")
gHC_INTERNAL_LEX                    = mkGhcInternalModule (fsLit "GHC.Internal.Text.Read.Lex")
gHC_INTERNAL_INT                    = mkGhcInternalModule (fsLit "GHC.Internal.Int")
gHC_INTERNAL_WORD                   = mkGhcInternalModule (fsLit "GHC.Internal.Word")
gHC_INTERNAL_MONAD                  = mkGhcInternalModule (fsLit "GHC.Internal.Control.Monad")
gHC_INTERNAL_MONAD_FIX              = mkGhcInternalModule (fsLit "GHC.Internal.Control.Monad.Fix")
gHC_INTERNAL_MONAD_FAIL             = mkGhcInternalModule (fsLit "GHC.Internal.Control.Monad.Fail")
gHC_INTERNAL_ARROW                  = mkGhcInternalModule (fsLit "GHC.Internal.Control.Arrow")
gHC_INTERNAL_DESUGAR                = mkGhcInternalModule (fsLit "GHC.Internal.Desugar")
gHC_INTERNAL_RANDOM                 = mkGhcInternalModule (fsLit "GHC.Internal.System.Random")
gHC_INTERNAL_EXTS                   = mkGhcInternalModule (fsLit "GHC.Internal.Exts")
gHC_INTERNAL_CONTROL_EXCEPTION_BASE = mkGhcInternalModule (fsLit "GHC.Internal.Control.Exception.Base")
gHC_INTERNAL_EXCEPTION_CONTEXT      = mkGhcInternalModule (fsLit "GHC.Internal.Exception.Context")
gHC_INTERNAL_GENERICS               = mkGhcInternalModule (fsLit "GHC.Internal.Generics")
gHC_INTERNAL_TYPEERROR              = mkGhcInternalModule (fsLit "GHC.Internal.TypeError")
gHC_INTERNAL_TYPELITS               = mkGhcInternalModule (fsLit "GHC.Internal.TypeLits")
gHC_INTERNAL_TYPELITS_INTERNAL      = mkGhcInternalModule (fsLit "GHC.Internal.TypeLits.Internal")
gHC_INTERNAL_TYPENATS               = mkGhcInternalModule (fsLit "GHC.Internal.TypeNats")
gHC_INTERNAL_TYPENATS_INTERNAL      = mkGhcInternalModule (fsLit "GHC.Internal.TypeNats.Internal")
gHC_INTERNAL_DATA_COERCE            = mkGhcInternalModule (fsLit "GHC.Internal.Data.Coerce")
gHC_INTERNAL_DEBUG_TRACE            = mkGhcInternalModule (fsLit "GHC.Internal.Debug.Trace")
gHC_INTERNAL_UNSAFE_COERCE          = mkGhcInternalModule (fsLit "GHC.Internal.Unsafe.Coerce")
gHC_INTERNAL_FOREIGN_C_CONSTPTR     = mkGhcInternalModule (fsLit "GHC.Internal.Foreign.C.ConstPtr")
gHC_INTERNAL_JS_PRIM                = mkGhcInternalModule (fsLit "GHC.Internal.JS.Prim")
gHC_INTERNAL_WASM_PRIM_TYPES        = mkGhcInternalModule (fsLit "GHC.Internal.Wasm.Prim.Types")

gHC_INTERNAL_SRCLOC :: Module
gHC_INTERNAL_SRCLOC = mkGhcInternalModule (fsLit "GHC.Internal.SrcLoc")

gHC_INTERNAL_STACK, gHC_INTERNAL_STACK_TYPES :: Module
gHC_INTERNAL_STACK = mkGhcInternalModule (fsLit "GHC.Internal.Stack")
gHC_INTERNAL_STACK_TYPES = mkGhcInternalModule (fsLit "GHC.Internal.Stack.Types")

gHC_INTERNAL_STATICPTR :: Module
gHC_INTERNAL_STATICPTR = mkGhcInternalModule (fsLit "GHC.Internal.StaticPtr")

gHC_INTERNAL_STATICPTR_INTERNAL :: Module
gHC_INTERNAL_STATICPTR_INTERNAL = mkGhcInternalModule (fsLit "GHC.Internal.StaticPtr.Internal")

gHC_INTERNAL_FINGERPRINT_TYPE :: Module
gHC_INTERNAL_FINGERPRINT_TYPE = mkGhcInternalModule (fsLit "GHC.Internal.Fingerprint.Type")

gHC_INTERNAL_OVER_LABELS :: Module
gHC_INTERNAL_OVER_LABELS = mkGhcInternalModule (fsLit "GHC.Internal.OverloadedLabels")

gHC_INTERNAL_RECORDS :: Module
gHC_INTERNAL_RECORDS = mkGhcInternalModule (fsLit "GHC.Internal.Records")

rOOT_MAIN :: Module
rOOT_MAIN       = mkMainModule (fsLit ":Main") -- Root module for initialisation

mkInteractiveModule :: String -> Module
-- (mkInteractiveMoudule "9") makes module 'interactive:Ghci9'
mkInteractiveModule n = mkModule interactiveUnit (mkModuleName ("Ghci" ++ n))

pRELUDE_NAME, mAIN_NAME, kNOWN_KEY_NAMES :: ModuleName
pRELUDE_NAME    = mkModuleNameFS (fsLit "Prelude")
mAIN_NAME       = mkModuleNameFS (fsLit "Main")
kNOWN_KEY_NAMES = mkModuleNameFS (fsLit "GHC.KnownKeyNames")

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

