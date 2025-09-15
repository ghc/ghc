{-# LANGUAGE ScopedTypeVariables #-}

import Prelude

{-# LANGUAGE RecursiveDo #-}

main = pure ()

foo :: forall a. a -> a
foo x = mdo x
