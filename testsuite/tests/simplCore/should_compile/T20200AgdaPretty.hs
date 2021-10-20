module T20200AgdaPretty where

import Control.Monad

import T20200AgdaInternalToAbstract
import T20200AgdaBase

data Doc

render :: a
render = undefined

prettyA :: a -> m Doc
prettyA x = undefined

class PrettyTCM a where
  prettyTCM :: HasConstInfo m => a -> m Doc

instance PrettyTCM QName where
  prettyTCM = prettyA <=< reify
