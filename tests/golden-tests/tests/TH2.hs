{-# LANGUAGE TemplateHaskell #-}

module TH2 where

import TH

-- we can't add a type sig here, so we get no doc
$( decl )
