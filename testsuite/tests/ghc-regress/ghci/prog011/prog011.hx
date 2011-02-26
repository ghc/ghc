:set +m
:m +Control.Monad.State
flip evalStateT 10 $ do
  i <- get
  lift $ print i

a <- return "Hello"
flip evalStateT 11 $ do
  i <- get
  lift $ print i
  lift $ print a

:unset +m
flip evalStateT 12 $ do

print a
