{-# LANGUAGE TemplateHaskell #-}

module TH_cvt_DefaultDataInstDecl where

import Language.Haskell.TH

$(return [ClassD [] (mkName "Foo") [] []
    [DataInstD [] Nothing (ConT (mkName "Bar")) Nothing [] []]])
