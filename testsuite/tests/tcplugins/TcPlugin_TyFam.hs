{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin TyFamPlugin #-}

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables #-}

module TcPlugin_TyFam where

import Definitions
  ( MyTyFam )

foo :: proxy a -> proxy b -> MyTyFam a a -> ()
foo _ _ x = x

