import Prop
import Control.Monad.X.WriterT

listen_return x   = listen (return x) === return (x,mempty)
listen_bind m1 m2 = listen (m1 >>= m2) === (do (x,w1) <- listen m1
                                               (y,w2) <- listen (m2 x)
                                               return (y, w1 `mappend` w2)) 


