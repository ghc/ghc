{-# LANGUAGE TemplateHaskell #-}

module TH_Roles1 where

import Language.Haskell.TH

$( return [ DataD [] (mkName "T") [PlainTV (mkName "a")] [] []
          , RoleAnnotD (mkName "T") [RepresentationalR] ] )

