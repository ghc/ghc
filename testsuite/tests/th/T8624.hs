{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dth-dec-file #-}
module T8624 (THDec(..)) where

import Language.Haskell.TH

$(return [DataD [] (mkName "THDec") [] Nothing
          [NormalC (mkName "THDec") []] []])
