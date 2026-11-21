{-# LANGUAGE CPP #-}
module DirectMappingProvider (Alternate (..)) where

class Alternate t where
    spare :: t a -> ()
    alternate :: (a -> b) -> t a -> t b
    strictMap :: (a -> b) -> t a -> t b

instance Alternate Maybe where
    spare _ = ()
#ifdef HIDE_WORKER
    {-# OPAQUE alternate #-}
#endif
    alternate _ _ = Nothing
    strictMap _ Nothing = Nothing
    strictMap f (Just x) = let y = f x in y `seq` Just y
