\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Show (Show(..)) where

import GHC.Types

type ShowS = [Char] -> [Char]

class  Show a  where
    showsPrec :: Int -> a -> ShowS
    show      :: a   -> [Char]
    showList  :: [a] -> ShowS

    showsPrec = showsPrec
    show      = show
    showList  = showList
\end{code}
