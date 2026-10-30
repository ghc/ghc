{-# LANGUAGE TemplateHaskell #-}

-- reifyModule forces a fresh interface load from inside the ReifyModule
-- THMessage handler, which is where the plugin hook panics. ghc-internal is
-- wired in, so its unit id carries no version.
module StaticPluginInterpPanicModule where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Module(..), PkgName(..), ModName(..), reifyModule)

x :: Int
x = $(do _ <- reifyModule (Module (PkgName "ghc-internal")
                                  (ModName "GHC.Internal.Real"))
         litE (integerL 42))
