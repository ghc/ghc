#ifdef PROFILE_ENABLED
#define _INL_(x)
#else
#define _INL_(x) {-# INLINE x #-}
#endif
