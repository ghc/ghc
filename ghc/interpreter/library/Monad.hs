module Monad (
    join, mapAndUnzipM, zipWithM, zipWithM_, foldM, when, unless, ap,
    liftM, liftM2, liftM3, liftM4, liftM5
    ) where

join             :: (Monad m) => m (m a) -> m a
join x           =  x >>= id

mapAndUnzipM     :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = accumulate (map f xs) >>= return . unzip

zipWithM         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys =  accumulate (zipWith f xs ys)

zipWithM_         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys =  sequence (zipWith f xs ys)

foldM            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f a []     =  return a
foldM f a (x:xs) =  f a x >>= \ y -> foldM f y xs

when             :: (Monad m) => Bool -> m () -> m ()
when p s         =  if p then s else return ()

unless           :: (Monad m) => Bool -> m () -> m ()
unless p s       =  when (not p) s

ap               :: (Monad m) => m (a -> b) -> m a -> m b
ap               =  liftM2 ($)

#if STD_PRELUDE
liftM            :: (Monad m) => (a -> b) -> (m a -> m b)
liftM f          =  \a -> [f a' | a' <- a]

liftM2           :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
liftM2 f         =  \a b -> [f a' b' | a' <- a, b' <- b]  

liftM3           :: (Monad m) => (a -> b -> c -> d) ->
                                 (m a -> m b -> m c -> m d)
liftM3 f         =  \a b c -> [f a' b' c' | a' <- a, b' <- b, c' <- c]  

liftM4           :: (Monad m) => (a -> b -> c -> d -> e) ->
                                 (m a -> m b -> m c -> m d -> m e)
liftM4 f         =  \a b c d -> [f a' b' c' d' |
                                 a' <- a, b' <- b, c' <- c, d' <- d]  

liftM5           :: (Monad m) => (a -> b -> c -> d -> e -> f) ->
                                 (m a -> m b -> m c -> m d -> m e -> m f)
liftM5 f         =  \a b c d e -> [f a' b' c' d' e' |
                                   a' <- a, b' <- b,
                                   c' <- c, d' <- d, e' <- e]
#else
liftM            :: (Monad m) => (a -> b) -> (m a -> m b)
liftM f          =  \a -> do { a' <- a; return (f a') }

liftM2           :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
liftM2 f         =  \a b -> do { a' <- a; b' <- b; return (f a' b') }

liftM3           :: (Monad m) => (a -> b -> c -> d) ->
                                 (m a -> m b -> m c -> m d)
liftM3 f         =  \a b c -> do { a' <- a; b' <- b; c' <- c
                                 ; return (f a' b' c') 
                                 }

liftM4           :: (Monad m) => (a -> b -> c -> d -> e) ->
                                 (m a -> m b -> m c -> m d -> m e)
liftM4 f         =  \a b c d -> do { a' <- a; b' <- b; c' <- c; d' <- d
                                   ; return (f a' b' c' d')
                                   }
                                

liftM5           :: (Monad m) => (a -> b -> c -> d -> e -> f) ->
                                 (m a -> m b -> m c -> m d -> m e -> m f)
liftM5 f         =  \a b c d e -> do { a' <- a; b' <- b
                                     ; c' <- c; d' <- d; e' <- e
                                     ; return (f a' b' c' d' e')
                                     }
                                  
#endif