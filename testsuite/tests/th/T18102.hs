{-# LANGUAGE TemplateHaskell, RebindableSyntax #-}

module Bug where

import Prelude ( Monad(..), Bool(..), print, ($) )
import Language.Haskell.TH.Syntax

$( do _stuff <- [| if True then 10 else 15 |]
      return [] )

$$( do _stuff <- [|| if True then 10 else 15 ||]
       return [] )
