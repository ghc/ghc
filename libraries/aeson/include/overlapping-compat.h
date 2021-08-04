#if __GLASGOW_HASKELL__ >= 710
#define OVERLAPPABLE_ {-# OVERLAPPABLE #-}
#define OVERLAPPING_  {-# OVERLAPPING #-}
#else
{-# LANGUAGE OverlappingInstances #-}
#define OVERLAPPABLE_
#define OVERLAPPING_
#endif
