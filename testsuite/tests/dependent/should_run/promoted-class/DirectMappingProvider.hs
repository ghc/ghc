{-# LANGUAGE CPP #-}
module DirectMappingProvider (Alternate (..)) where

class Alternate t where
    spare :: t a -> ()
    alternate :: (a -> b) -> t a -> t b

instance Alternate Maybe where
    spare _ = ()
#ifdef HIDE_WORKER
    {-# OPAQUE alternate #-}
#endif
    alternate _ _ = Nothing
