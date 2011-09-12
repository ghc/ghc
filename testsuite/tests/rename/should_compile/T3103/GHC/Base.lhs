
\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Base
        (
        module GHC.Base,
        module GHC.Classes,
        module GHC.Types,
        module GHC.Prim,
  )
        where

import GHC.Types
import GHC.Classes
import GHC.Prim
import GHC.Tuple ()
import GHC.Integer ()

default ()

type String = [Char]
\end{code}

