{-# LANGUAGE QuasiQuotes #-}
module B where

import A

foo:: [aquoter|Int|] -> [aquoter|String|]
foo = show
