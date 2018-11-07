{-# LANGUAGE TemplateHaskell, TypeFamilies, PolyKinds #-}

module T9692 where

import Data.Kind (Type)
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type)
import Language.Haskell.TH.Ppr
import System.IO

class C a where
        data F a (b :: k) :: Type

instance C Int where
        data F Int x = FInt x

$( do info <- qReify (mkName "F")
      runIO $ putStrLn $ pprint info
      runIO $ hFlush stdout
      return [])
