  -- This tests an example included in the GHC user's guide (see hasfield.rst).
  -- Please update the user's guide if it needs to be changed!

  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE GADTs #-}
  {-# LANGUAGE PolyKinds #-}
  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE TypeApplications #-}
  {-# LANGUAGE TypeOperators #-}
  {-# LANGUAGE UndecidableInstances #-}

  import Data.Kind (Type)
  import Data.Proxy (Proxy(..))
  import GHC.Records

  data Record (xs :: [(k, Type)]) where
    Nil  :: Record '[]
    Cons :: Proxy x -> a -> Record xs -> Record ('(x, a) ': xs)

  instance {-# OVERLAPS #-} HasField x (Record ('(x, a) ': xs)) a where
    hasField (Cons p v r) = (\v' -> Cons p v' r, v)
  instance HasField x (Record xs) a => HasField x (Record ('(y, b) ': xs)) a where
    hasField (Cons p v r) = (\v' -> Cons p v (set v'), a)
      where
        (set,a) = hasField @x r

  r :: Record '[ '("name", String) ]
  r = Cons Proxy "R" Nil

  x = getField @"name" (setField @"name" r "S")

  main = print x
