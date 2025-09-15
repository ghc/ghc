{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

main = pure ()

foo :: forall a. a -> a
foo x = mdo x
