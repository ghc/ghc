{-# LANGUAGE TemplateHaskell, PartialTypeSignatures, NamedWildCards #-}

module SplicesUsed where

import Splices

maybeBool :: $(metaType1)
maybeBool = $(metaExp2) $(metaExp1)

charA :: a -> $(metaType2)
charA x = ('x', x)

filter' :: $(metaType3)
filter' = filter

$(metaDec1)

$(metaDec2)
