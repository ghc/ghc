{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE ImpredicativeTypes #-}

module DeepSubsumption09 where

genericQuery :: forall a4. a4 -> ( forall a5. a5 -> () )
genericQuery = id ( genericQuery . const )
