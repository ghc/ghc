{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T22559c where

import Language.Haskell.TH

data family D

$(pure [NewtypeInstD
         [] Nothing
         (ConT (mkName "D")) (Just StarT)
         (NormalC (mkName "MkD")
                  [( Bang NoSourceUnpackedness NoSourceStrictness
                   , ConT ''Int
                   )])
         []])
