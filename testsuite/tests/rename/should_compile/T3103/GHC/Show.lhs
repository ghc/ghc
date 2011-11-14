\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Show (Show(..)) where

import GHC.Types

class  Show a  where
    show      :: a   -> [Char]
\end{code}
