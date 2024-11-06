{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

-- For init in spliceMe
{-# OPTIONS_GHC -Wno-x-partial #-}

module TH where

import Language.Haskell.TH

spliceMe = [| (\xs -> tail xs ++ init xs) |]
