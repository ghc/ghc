module Monad (
    join, apply, (@@), mapAndUnzipL, mapAndUnzipR, accumulateL,
    accumulateR, zipWithL, zipWithR, sequenceL, sequenceR,
    mapL, mapR, map_, foldL, foldR, concatM, done, unless, when
    ) where

join             :: (Monad m) => m (m a) -> m a
join x           = x >>= id

apply            :: (Monad m) => (a -> m b) -> (m a -> m b)
apply f x        = x >>= f

(@@)             :: (Monad m) => (a -> m b) -> (c -> m a) -> (c -> m b)
f @@ g           = \ x -> g x >>= f

mapAndUnzipL     :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipL f xs = accumulateL (map f xs) >>= return . unzip

mapAndUnzipR     :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipR f xs = accumulateR (map f xs) >>= return . unzip

accumulateL      :: (Monad m) => [m a] -> m [a]
accumulateL      = accumulate

accumulateR      :: (Monad m) => [m a] -> m [a]
accumulateR      = foldr mcons (return [])
	where mcons p q = q >>= \ xs -> p >>= \ x -> return (x:xs)

zipWithL         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithL f xs ys = accumulateL (zipWith f xs ys)

zipWithR         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithR f xs ys = accumulateR (zipWith f xs ys)

sequenceL        :: (Monad m) => [m a] -> m ()
sequenceL []     = return ()
sequenceL (x:xs) = x >> sequenceL xs

sequenceR        :: (Monad m) => [m a] -> m ()
sequenceR []     = return ()
sequenceR (x:xs) = sequenceR xs >> x >> return ()

mapL             :: (Monad m) => (a -> m b) -> ([a] -> m [b])
mapL f []        = return []
mapL f (x:xs)    = f x >>= \ y -> mapL f xs >>= \ ys -> return (y:ys)

mapR             :: (Monad m) => (a -> m b) -> ([a] -> m [b])
mapR f []        = return []
mapR f (x:xs)    = mapR f xs >>= \ ys -> f x >>= \ y -> return (y:ys)

map_             :: (Monad m) => (a -> m b) -> ([a] -> m ())
map_ f []        = return ()
map_ f (x:xs)    = f x >> map_ f xs

foldL            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldL f a []     = return a
foldL f a (x:xs) = f a x >>= \fax -> foldL f fax xs

foldR            :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldR f a []     = return a
foldR f a (x:xs) = foldR f a xs >>= \y -> f x y

concatM          :: MonadPlus m => [m a] -> m a
concatM          =  foldr (++) zero

done 		 :: (Monad m) => m ()
done 		 =  return ()

unless 		 :: (Monad m) => Bool -> m () -> m ()
unless p s 	 =  if p then return () else s

when 		 :: (Monad m) => Bool -> m () -> m ()
when p s	 =  if p then s else return ()
