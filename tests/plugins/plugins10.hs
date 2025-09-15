{-# OPTIONS_GHC -fplugin-opt Simple.SourcePlugin:a #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module B where

import QuasiQuotation

$(return [])

x = [stringify|x|]
