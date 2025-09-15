
-- Test for trac #1588: unrequested generalized newtype deriving?

newtype MaybeT  m a = MaybeT  { runMaybeT  :: m (Maybe a) } deriving Eq

data    MaybeT' m a = MaybeT' { runMaybeT' :: m (Maybe a) } deriving Eq
