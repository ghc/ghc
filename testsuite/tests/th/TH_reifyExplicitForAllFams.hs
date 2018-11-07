-- test reification of explicit foralls in type families

{-# LANGUAGE TypeFamilies, ExplicitForAll #-}
module TH_reifyExplicitForAllFams where

import System.IO
import Language.Haskell.TH
import Text.PrettyPrint.HughesPJ

import Data.Proxy
import Data.Kind

$([d| data family F a
      data instance forall a. F (Maybe a) = MkF a |])

$([d| class C a where
        type G a b
      instance forall a. C [a] where
        type forall b. G [a] b = Proxy b |])

$([d| type family H a b where
        forall x y. H [x] (Proxy y) = Either x y
        forall z.   H z   z         = Maybe z |])

$(return [])

test :: ()
test = $(let
      display :: Name -> Q ()
      display q = do { i <- reify q; runIO $ hPutStrLn stderr (pprint i) }
    in do { display ''F
          ; display ''C
          ; display ''G
          ; display ''H
          ; [| () |] })
