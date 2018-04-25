{-# LANGUAGE TemplateHaskell, GADTs, ExplicitForAll, KindSignatures #-}

module T10828b where

import Language.Haskell.TH
import System.IO

-- attempting to mix GADT and normal constructors
$( return
   [ DataD [] (mkName "T")
           [ PlainTV (mkName "a") ]
           (Just StarT)
           [ NormalC
               (mkName "MkT")
               [ ( Bang NoSourceUnpackedness NoSourceStrictness
                 , VarT (mkName "a")
                 )
               , ( Bang NoSourceUnpackedness NoSourceStrictness
                 , VarT (mkName "a")
                 )
               ]
           , ForallC [PlainTV (mkName "a")]
                     [AppT (AppT EqualityT (VarT $ mkName "a"  ) )
                                           (ConT $ mkName "Int") ] $
             RecGadtC
                 [ (mkName "MkC")]
                 [ ( mkName "foo"
                   , Bang NoSourceUnpackedness NoSourceStrictness
                   , VarT (mkName "a")
                   )
                 , ( mkName "bar"
                   , Bang NoSourceUnpackedness NoSourceStrictness
                   , VarT (mkName "b")
                   )
                 ]
                 (AppT (ConT (mkName "T"))
                       (ConT (mkName "Int")))
           ]
           [] ])
