{-# LANGUAGE TemplateHaskell #-}

module TH_cvt_IllegalDeclaration where

import Language.Haskell.TH

$(return [InstanceD Nothing [] (ConT (mkName "Foo"))
    [DataD [] (mkName "Bar") [] Nothing [] []]])
