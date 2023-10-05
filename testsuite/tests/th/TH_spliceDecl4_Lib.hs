
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}

module TH_spliceDecl4_Lib(
  instanceIncrSelfTuple,
  IncrSelf(..)
)
where
import Control.Monad
import Data.Maybe
import Language.Haskell.TH

class IncrSelf a where
    incrSelf :: a -> a



instanceIncrSelfTuple :: Int -> Q [Dec]
instanceIncrSelfTuple n = [d| incrSelf value = True |]


