{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
module T22757 where

import Language.Haskell.TH

reportWarning "should be suppressed" >> pure []
