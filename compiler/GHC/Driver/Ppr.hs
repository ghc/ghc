-- | Printing related functions that depend on session state (DynFlags)
module GHC.Driver.Ppr
   ( showSDoc
   , showSDocUnsafe
   , showSDocForUser
   , showPpr
   , showPprUnsafe
   , printForUser
   )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Unit.State

import GHC.Utils.Outputable
import GHC.Utils.Ppr       ( Mode(..) )

import System.IO ( Handle )

-- | Show a SDoc as a String with the default user style
showSDoc :: DynFlags -> SDoc -> String
showSDoc dflags sdoc = renderWithContext (initSDocContext dflags defaultUserStyle) sdoc

showPpr :: Outputable a => DynFlags -> a -> String
showPpr dflags thing = showSDoc dflags (ppr thing)

-- | Allows caller to specify the PrintUnqualified to use
showSDocForUser :: DynFlags -> UnitState -> PrintUnqualified -> SDoc -> String
showSDocForUser dflags unit_state unqual doc = renderWithContext (initSDocContext dflags sty) doc'
   where
      sty  = mkUserStyle unqual AllTheWay
      doc' = pprWithUnitState unit_state doc

printForUser :: DynFlags -> Handle -> PrintUnqualified -> Depth -> SDoc -> IO ()
printForUser dflags handle unqual depth doc
  = printSDocLn ctx (PageMode False) handle doc
    where ctx = initSDocContext dflags (mkUserStyle unqual depth)
