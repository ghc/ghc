{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T22559b where

import Language.Haskell.TH

data family D

$(pure [DataInstD
         [] Nothing
         (ConT (mkName "D")) (Just StarT)
         [NormalC (mkName "MkD")
                  [( Bang NoSourceUnpackedness NoSourceStrictness
                   , ConT ''Int
                   )]]
         []])
