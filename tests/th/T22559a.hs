{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module T22559a where

import Language.Haskell.TH

$(pure [NewtypeD
         [] (mkName "D") [] (Just StarT)
         (NormalC (mkName "MkD")
                  [( Bang NoSourceUnpackedness NoSourceStrictness
                   , ConT ''Int
                   )])
         []])
