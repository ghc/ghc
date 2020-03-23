{-# LANGUAGE TemplateHaskell, GADTs, ExplicitForAll, KindSignatures #-}

module T10828a where

import Language.Haskell.TH
import System.IO

-- attempting to place a kind signature on a H98 data type
$( return
   [ DataD [] (mkName "T")
           [ PlainTV (mkName "a") () ]
           (Just StarT)
           [ NormalC (mkName "MkT")
                   [ ( Bang NoSourceUnpackedness NoSourceStrictness
                     , VarT (mkName "a")
                     )
                   , ( Bang NoSourceUnpackedness NoSourceStrictness
                     , VarT (mkName "a")
                     )
                   ]
           ]
           [] ])
