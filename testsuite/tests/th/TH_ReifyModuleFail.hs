{-# LANGUAGE TemplateHaskell #-}

-- A TH splice whose ReifyModule request fails on the GHC side.
--
-- `reifyModule` on a non-existing module/package should result into a clean
-- error message on both internal and external interpreter, and no deadlocks.
module TH_ReifyModuleFail where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Module(..), PkgName(..), ModName(..), reifyModule)

x :: Int
x = $(do _ <- reifyModule (Module (PkgName "no-such-package-0")
                                  (ModName "No.Such.Module"))
         litE (integerL 1))
