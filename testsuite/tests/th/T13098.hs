{-# LANGUAGE TemplateHaskell #-}

module T13098 where

import Language.Haskell.TH

$( sequence [ dataD (cxt []) (mkName "T") [PlainTV (mkName "a")]
                     Nothing [normalC (mkName "T") []] []
          , pragCompleteD [mkName "T"] Nothing ] )
