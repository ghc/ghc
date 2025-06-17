{-# LANGUAGE TemplateHaskell, GHC2021 #-}

module TH_Roles1 where

import Language.Haskell.TH

$( return [ DataD [] (mkName "T") [PlainTV (mkName "a") BndrReq] Nothing [] []
          , RoleAnnotD (mkName "T") [RepresentationalR] ] )

