{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module RepPolyPatSynRes where

import GHC.Exts

pattern Pat :: forall rep (a :: TYPE rep). ( () ~ () ) => a
pattern Pat <- ( undefined -> True )
