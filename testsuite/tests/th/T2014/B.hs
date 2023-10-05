{-# LANGUAGE TemplateHaskell #-}
module B where

import {-# SOURCE #-} A ()
--import A ()
import Language.Haskell.TH

expQ :: ExpQ
expQ = [| () |]
