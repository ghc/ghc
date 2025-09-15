{-# LANGUAGE PatternSynonyms #-}

module T19843e where

import Prelude (undefined, map, Maybe(Just))

pattern Mup k = Just k

foo (Map k) = undefined
