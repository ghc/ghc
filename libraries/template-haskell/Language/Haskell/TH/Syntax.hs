module Language.Haskell.TH.Syntax
  ( module Language.Haskell.TH.Lib.Syntax
  )
where

import Language.Haskell.TH.Lib.Syntax

-- The only difference between this module and Language.Haskell.TH.Lib.Syntax
-- (which it reexports fully) is that this module depends on the Internal
-- module.
--
-- We did this to fix #22229: a module importing the Syntax module to use
-- DeriveLift (Lift is defined there) would lead GHC to load the
-- interface file for the Internal module (where wired-in TH things live),
-- but the Internal module might not be built yet at this point. Adding an
-- explicit dependency from Syntax to Internal fixes this. We do this with a
-- module reexport because Internal actually depends on Syntax.
--
-- See Note [Tracking dependencies on primitives] in GHC.Internal.Base, wrinkle W4.
import Language.Haskell.TH.Lib.Internal ()
