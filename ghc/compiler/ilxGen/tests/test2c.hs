import PrelIOBase


bindIO2 :: IO () -> IO () -> IO ()
bindIO2 m (IO k) = IO ( \ s -> k s )

foreign import "ilxHello" unsafe ilxHello :: IO ()

data N = S N | Z

f Z = bindIO2 
f (S x) = f x

main = f(S Z) ilxHello ilxHello
