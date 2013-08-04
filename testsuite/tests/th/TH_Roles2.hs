{-# LANGUAGE TemplateHaskell, RoleAnnotations, PolyKinds #-}

module TH_Roles2 where

import Language.Haskell.TH

$( return [DataD [] (mkName "T") [KindedRoledTV (mkName "a") (VarT (mkName "k")) Representational] [] []] )

