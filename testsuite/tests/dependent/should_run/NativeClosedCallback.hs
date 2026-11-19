{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedCallbackLib as W
import qualified Prelude as P

prelude :: P.map P.not '[ 'P.True, 'P.False] :~: '[ 'P.False, 'P.True]
prelude = Refl

complemented :: W.each (W.flipBool) '[ 'P.True, 'P.False] :~: '[ 'P.False, 'P.True]
complemented = Refl

constant :: W.each W.setTrue '[ 'P.True, 'P.False] :~: '[ 'P.True, 'P.True]
constant = Refl

polymorphic :: W.each P.id '[ 'P.True, 'P.False] :~: '[ 'P.True, 'P.False]
polymorphic = Refl

main :: P.IO ()
main = P.print (prelude, complemented, constant, polymorphic)
