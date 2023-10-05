{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module T23240_aux where

import Language.Haskell.TH ( CodeQ )

data D = MkD { myFld :: () }
mkD :: CodeQ D
mkD = [|| MkD { myFld = () } ||]
