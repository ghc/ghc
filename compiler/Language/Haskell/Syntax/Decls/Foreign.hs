{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE ViewPatterns #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/@type@AQUA Project, Glasgow University, 1992-1998
-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*

-- |
-- Abstract syntax of foreign function interface declarations.
module Language.Haskell.Syntax.Decls.Foreign (
  -- * Foreign function interface declarations
  -- ** Data-type
  ForeignDecl(..),
  -- ** Record synonym
  LForeignDecl,

  -- * Foreign export types
  -- ** Data-type
  ForeignExport(..),
  -- ** Specification
  CExportSpec(..),

  -- * Foreign import types
  -- ** Data-type
  ForeignImport(..),
  -- ** Specification
  CImportSpec(..),
  -- ** Sub-types
  CCallTarget(..),
  ForeignKind(..),
  Safety(..),

  -- * Foreign binding type
  CType(..),

  -- * General sub-types
  CLabelString,
  CCallConv(..),
  Header(..),

  -- * Extension points
  -- ** ForeignDecl
  XForeignExport,
  XForeignImport,
  XXForeignDecl,
  -- ** ForeignExport
  XCExport,
  XXForeignExport,
  -- ** ForeignImport
  XCImport,
  XXForeignImport,
  -- ** CCallTarget
  XStaticTarget,
  XDynamicTarget,
  XXCCallTarget,
  -- ** CType
  XCType,
  XXCType,
  -- ** Header
  XHeader,
  XXHeader,
  ) where

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Type

import GHC.Data.FastString (FastString)

import Control.DeepSeq
import Data.Data        hiding (TyCon, Fixity, Infix)
import Data.Maybe
import Data.Eq
import Prelude (Enum, Show)

{-
************************************************************************
*                                                                      *
\subsection{Foreign function interface declaration}
*                                                                      *
************************************************************************
-}

-- foreign declarations are distinguished as to whether they define or use a
-- Haskell name
--
--  * the Boolean value indicates whether the pre-standard deprecated syntax
--   has been used

-- | Located Foreign Declaration
type LForeignDecl pass = XRec pass (ForeignDecl pass)

-- | Foreign Declaration
data ForeignDecl pass
  = ForeignImport
      { fd_i_ext  :: XForeignImport pass   -- Post typechecker, rep_ty ~ sig_ty
      , fd_name   :: LIdP pass             -- defines this name
      , fd_sig_ty :: LHsSigType pass       -- sig_ty
      , fd_fi     :: ForeignImport pass }

  | ForeignExport
      { fd_e_ext  :: XForeignExport pass   -- Post typechecker, rep_ty ~ sig_ty
      , fd_name   :: LIdP pass             -- uses this name
      , fd_sig_ty :: LHsSigType pass       -- sig_ty
      , fd_fe     :: ForeignExport pass }
  | XForeignDecl !(XXForeignDecl pass)

{-
    In both ForeignImport and ForeignExport:
        sig_ty is the type given in the Haskell code
        rep_ty is the representation for this type, i.e. with newtypes
               coerced away and type functions evaluated.
    Thus if the declaration is valid, then rep_ty will only use types
    such as Int and IO that we know how to make foreign calls with.
-}


-- |
-- Specification Of an imported external entity in dependence on the calling
-- convention
data ForeignImport pass
  = -- |
    -- Import of a C entity
    --
    --  * the two strings specifying a header file or library
    --   may be empty, which indicates the absence of a
    --   header or object specification (both are not used
    --   in the case of `CWrapper' and when `CFunction'
    --   has a dynamic target)
    --
    --  * the calling convention is irrelevant for code
    --   generation in the case of `CLabel', but is needed
    --   for pretty printing
    --
    --  * `Safety' is irrelevant for `CLabel' and `CWrapper'
    --
    CImport
      (XCImport pass)
      (XRec pass CCallConv) -- ccall
      (XRec pass Safety)    -- interruptible, safe or unsafe
      (Maybe (Header pass))        -- name of C header
      (CImportSpec pass)    -- details of the C entity
  | XForeignImport !(XXForeignImport pass)

-- |
-- Details of an external C entity
data CImportSpec pass
  = CLabel    CLabelString       -- import address of a C label
  | CFunction (CCallTarget pass) -- static or dynamic function
  | CWrapper                     -- wrapper to expose closures
                                 -- (former f.e.d.)

-- |
-- Specification of an externally exported entity in dependence on the calling
-- convention
data ForeignExport pass
  = CExport (XCExport pass) (XRec pass CExportSpec)
    -- ^ contains the calling convention
  | XForeignExport !(XXForeignExport pass)

{-
Stuff to do with calling convention:

ccall:          Caller allocates parameters, *and* deallocates them.

See: http://www.programmersheaven.com/2/Calling-conventions
-}

-- any changes here should be replicated in the Callconv type in template haskell
data CCallConv
  = CCallConv
  | CApiConv
  | StdCallConv
  | PrimCallConv
  | JavaScriptCallConv
  deriving (Enum, Eq, Data, Show)

instance NFData CCallConv where
  rnf = \case
    CCallConv -> ()
    StdCallConv -> ()
    PrimCallConv -> ()
    CApiConv -> ()
    JavaScriptCallConv -> ()

data ForeignKind
  = ForeignValue    -- ^ Binds to a value, a zero-arity function.
  | ForeignFunction -- ^ Binds to a function with arity /of at least one/.
  deriving stock (Enum, Eq, Data, Show)

instance NFData ForeignKind where
  rnf = \case
    ForeignValue    -> ()
    ForeignFunction -> ()

-- | How to call a particular function in C-land.
data CCallTarget pass
  -- An "unboxed" ccall# to named function in a particular package.
  = StaticTarget
        (XStaticTarget pass)
        CLabelString -- C-land name of label.
        ForeignKind  -- only allowed in CAPI imports

  -- | The first argument of the import is the name of a function pointer (an Addr#).
  -- Used when importing a label as "foreign import ccall "dynamic" ..."
  | DynamicTarget (XDynamicTarget pass)
  | XCCallTarget !(XXCCallTarget pass)

data CExportSpec
  -- | foreign export ccall foo :: ty
  = CExportStatic
        CLabelString -- C Name of exported function
        CCallConv
  deriving Data

type CLabelString = FastString  -- A C label, completely unencoded

-- | A C type, used in CAPI FFI calls
data CType pass
  = CType
      (XCType pass)
      (Maybe (Header pass)) -- header to include for this type
      FastString
  | XCType !(XXCType pass)

-- The filename for a C header file
-- See Note [Pragma source text] in "GHC.Types.SourceText"
data Header pass
  = Header
      (XHeader pass)
      FastString
  | XHeader !(XXHeader pass)

data Safety
  = PlaySafe          -- ^ Might invoke Haskell GC, or do a call back, or
                      --   switch threads, etc.  So make sure things are
                      --   tidy before the call. Additionally, in the threaded
                      --   RTS we arrange for the external call to be executed
                      --   by a separate OS thread, i.e., _concurrently_ to the
                      --   execution of other Haskell threads.

  | PlayInterruptible -- ^ Like PlaySafe, but additionally
                      --   the worker thread running this foreign call may
                      --   be unceremoniously killed, so it must be scheduled
                      --   on an unbound thread.

  | PlayRisky         -- ^ None of the above can happen; the call will return
                      --   without interacting with the runtime system at all.
                      --   Specifically:
                      --
                      --     * No GC
                      --     * No call backs
                      --     * No blocking
                      --     * No precise exceptions
                      --
  deriving ( Eq, Show, Data, Enum )
        -- Show used just for Show Lex.Token, I think

instance NFData Safety where
  rnf PlaySafe = ()
  rnf PlayInterruptible = ()
  rnf PlayRisky = ()
