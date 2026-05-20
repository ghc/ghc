{-# LANGUAGE TemplateHaskell #-}

module TH_cvt_InvalidTypeInstanceHeader where

import Language.Haskell.TH

$(return [DataInstD [] Nothing WildCardT Nothing [] []])
