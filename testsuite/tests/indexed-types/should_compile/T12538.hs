{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T12538 where

data Tagged t a = Tagged a

type family Tag a where
  Tag (Tagged t a) = Tagged t a
  Tag a = Tagged Int a

class (r ~ Tag a) => TagImpl a r | a -> r where
  tag :: a -> r

instance {-# OVERLAPPING #-} (r ~ Tag (Tagged t a)) => TagImpl (Tagged t a) r where
  tag = id

#if defined(WRONG)
instance {-# OVERLAPPING #-} (r ~ Tagged t a, r ~ Tag a) => TagImpl a r where
#else
instance {-# OVERLAPPING #-} (r ~ Tagged Int a, r ~ Tag a) => TagImpl a r where
#endif
  tag = Tagged @Int

data family   DF x
data instance DF (Tagged t a) = DF (Tagged t a)

class ToDF a b | a -> b where
  df :: a -> b

instance (TagImpl a a', b ~ DF a') => ToDF a b where
  df = DF . tag

main :: IO ()
main = pure ()
