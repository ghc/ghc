{-# LANGUAGE TemplateHaskell, RoleAnnotations, PolyKinds #-}

module TH_Roles2 where

import Language.Haskell.TH

$( return [ DataD [] (mkName "T") [KindedTV (mkName "a") (VarT (mkName "k"))]
            Nothing [] []
          , RoleAnnotD (mkName "T") [RepresentationalR] ] )

