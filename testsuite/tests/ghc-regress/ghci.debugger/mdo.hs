import Control.Monad.Fix
import Data.IORef

data N a = N (IORef Bool, N a, a, N a)

newNode :: N a -> a -> N a -> IO (N a)
newNode b c f = do v  <- newIORef False
                   return (N (v, b, c, f))

ll = mdo n0 <- newNode n3 0 n1
         n1 <- newNode n0 1 n2
         n2 <- newNode n1 2 n3
         n3 <- newNode n2 3 n0
         return n0

data Dir = F | B deriving Eq

traverse :: Dir -> N a -> IO [a]
traverse d (N (v, b, i, f)) =
    do visited <- readIORef v
       if visited
          then return []
          else do writeIORef v True
                  let next = if d == F then f else b
                  is <- traverse d next
                  return (i:is)

l2dll :: [a] -> IO (N a)
l2dll (x:xs) = mdo c      <- newNode l x f
                   (f, l) <- l2dll' c xs
                   return c

l2dll' :: N a -> [a] -> IO (N a, N a)
l2dll' p []     = return (p, p)
l2dll' p (x:xs) = mdo c      <- newNode p x f
                      (f, l) <- l2dll' c xs
                      return (c, l)