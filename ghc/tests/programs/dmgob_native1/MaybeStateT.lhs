        This module implements the ``State Transformer''
monad coupled with the ``Maybe'' monad, where the ``Maybe'' type is
wrapped around the pair containing the return value and the state.
        \begin{haskell}{MaybeStateT}

> module MaybeStateT(
>#ifndef __GLASGOW_HASKELL__
>       Maybe..,
>#endif
>       MST(..),
>       returnMST,
>       bindMST
>       ) where

>#ifndef __GLASGOW_HASKELL__
> import Maybe
>#endif

> type MST s a  =  s -> Maybe (a, s)

> returnMST     :: a -> MST s a
> returnMST x   = \s -> Just (x, s)

> bindMST       :: MST s a -> (a -> MST s b) -> MST s b
> bindMST m k s =  m s >>= \(x, s') -> k x s'

\end{haskell}
