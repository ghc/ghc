{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module T19519 where

import qualified Language.Haskell.TH.Syntax as TH

data VendorEnv = A

instance TH.Lift VendorEnv where
  liftTyped _ = [|| A ||]

