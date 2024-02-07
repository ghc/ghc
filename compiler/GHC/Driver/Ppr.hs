-- | Printing related functions that depend on session state (DynFlags)
module GHC.Driver.Ppr
   ( showSDoc
   , showSDocUnsafe
   , showSDocForUser
   , showPpr
   , showPprUnsafe
   , printForUser
   , printForUserColoured
   )
where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Unit.State

import GHC.Utils.Outputable
import GHC.Utils.Ppr       ( Mode(..) )

import System.IO ( Handle )

-- | Show a SDoc as a String with the default user style
showSDoc :: DynFlags -> SDoc -> String
showSDoc dflags sdoc = renderWithContext (initSDocContext dflags defaultUserStyle) sdoc

showPpr :: Outputable a => DynFlags -> a -> String
showPpr dflags thing = showSDoc dflags (ppr thing)

-- | Allows caller to specify the NamePprCtx to use
showSDocForUser :: DynFlags -> UnitState -> NamePprCtx -> SDoc -> String
showSDocForUser dflags unit_state name_ppr_ctx doc = renderWithContext (initSDocContext dflags sty) doc'
   where
      sty  = mkUserStyle name_ppr_ctx AllTheWay
      doc' = pprWithUnitState unit_state doc

printForUser :: DynFlags -> Handle -> NamePprCtx -> Depth -> SDoc -> IO ()
printForUser = printForUser' False

printForUserColoured :: DynFlags -> Handle -> NamePprCtx -> Depth -> SDoc -> IO ()
printForUserColoured = printForUser' True

printForUser' :: Bool -> DynFlags -> Handle -> NamePprCtx -> Depth -> SDoc -> IO ()
printForUser' colour dflags handle name_ppr_ctx depth doc
  = printSDocLn ctx (PageMode False) handle doc
    where ctx = initSDocContext dflags (setStyleColoured colour $ mkUserStyle name_ppr_ctx depth)
