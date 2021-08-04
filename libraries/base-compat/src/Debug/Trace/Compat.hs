{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Debug.Trace.Compat (
  module Base
, traceId
, traceShowId
, traceM
, traceShowM
) where

#if !(MIN_VERSION_base(4,7,0)) || MIN_VERSION_base(4,9,0)
import Debug.Trace as Base
#else
import Debug.Trace as Base hiding (
    traceM
  , traceShowM
  )
#endif

#if !(MIN_VERSION_base(4,9,0))
import Prelude.Compat
#endif

#if !(MIN_VERSION_base(4,7,0))
{-|
Like 'trace' but returns the message instead of a third value.

/Since: 4.7.0.0/
-}
traceId :: String -> String
traceId a = trace a a

{-|
Like 'traceShow' but returns the shown value instead of a third value.

/Since: 4.7.0.0/
-}
traceShowId :: (Show a) => a -> a
traceShowId a = trace (show a) a
#endif

#if !(MIN_VERSION_base(4,9,0))
{-|
Like 'trace' but returning unit in an arbitrary 'Applicative' context. Allows
for convenient use in do-notation.

Note that the application of 'traceM' is not an action in the 'Applicative'
context, as 'traceIO' is in the 'IO' type. While the fresh bindings in the
following example will force the 'traceM' expressions to be reduced every time
the @do@-block is executed, @traceM "not crashed"@ would only be reduced once,
and the message would only be printed once.  If your monad is in 'MonadIO',
@liftIO . traceIO@ may be a better option.

> ... = do
>   x <- ...
>   traceM $ "x: " ++ show x
>   y <- ...
>   traceM $ "y: " ++ show y

/Since: 4.7.0.0/
-}
traceM :: (Applicative f) => String -> f ()
traceM string = trace string $ pure ()

{-|
Like 'traceM', but uses 'show' on the argument to convert it to a 'String'.

> ... = do
>   x <- ...
>   traceShowM $ x
>   y <- ...
>   traceShowM $ x + y

/Since: 4.7.0.0/
-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = traceM . show
#endif
