#if __GLASGOW_HASKELL__ >= 710
#define INCOHERENT_ {-# INCOHERENT #-}
#else
-- This causes some type class instances to break:
-- {-# LANGUAGE IncoherentInstances #-}
#define INCOHERENT_
#endif
