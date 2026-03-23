\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Base
        (
        module GHC.Classes,
        module GHC.Prim,
        Num(..)
  ) where

import GHC.Classes
import GHC.Prim
import qualified GHC.Internal.Base as Rebindable

class Num a  where
    signum    :: a -> a
\end{code}

