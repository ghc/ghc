{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module T20969 where

import Data.Sequence.Internal
import qualified Language.Haskell.TH.Syntax as TH

import T20969A

glumber :: forall a. Num a => a -> Seq a
glumber x = $$(sequenceCode (fromList [TH.liftTyped _ :: TH.Code TH.Q a, [||x||]]))

