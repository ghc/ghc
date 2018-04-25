module StateMonad where

-- General purpose state monad -----------------------------------------------

type SM s a       = s -> (s, a)

-- Primitive monad operators -------------------------------------------------

retURN           :: a -> SM s a
retURN x          = \s -> (s, x)

bind             :: SM s a -> (a -> SM s b) -> SM s b
m `bind` f        = \s -> let (s',a) = m s in f a s'

join             :: SM s (SM s a) -> SM s a
join m            = \s -> let (s',ma) = m s in ma s'

mmap             :: (a -> b) -> (SM s a -> SM s b)
mmap f m          = \s -> let (s',a)  = m s in (s', f a)

-- General monad operators ---------------------------------------------------

mmapl            :: (a -> SM s b) -> ([a] -> SM s [b])
mmapl f []        = retURN []
mmapl f (a:as)    = f a             `bind` \b ->
                    mmapl f as      `bind` \bs ->
                    retURN (b:bs)

mmapr            :: (a -> SM s b) -> ([a] -> SM s [b])
mmapr f []        = retURN []
mmapr f (x:xs)    = mmapr f xs      `bind` \ys ->
                    f x             `bind` \y  ->
                    retURN (y:ys)

mfoldl           :: (a -> b -> SM s a) -> a -> [b] -> SM s a
mfoldl f a []     = retURN a
mfoldl f a (x:xs) = f a x           `bind` \fax ->
                    mfoldl f fax xs

mfoldr           :: (a -> b -> SM s b) -> b -> [a] -> SM s b
mfoldr f a []     = retURN a
mfoldr f a (x:xs) = mfoldr f a xs   `bind` \y ->
                    f x y

mif              :: SM s Bool -> SM s a -> SM s a -> SM s a
mif c t f         = c               `bind` \cond ->
                    if cond then t
                            else f

-- Specific utilities for state monads ---------------------------------------

startingWith      :: SM s a -> s -> a
m `startingWith` v = answer where (final,answer) = m v

fetch             :: SM s s
fetch              = \s -> (s,s)

fetchWith         :: (s -> a) -> SM s a
fetchWith f        = \s -> (s, f s)

update            :: (s -> s) -> SM s s
update f           = \s -> (f s, s)

set               :: s -> SM s s
set s'             = \s -> (s',s)

-- Common use of state monad: counter ----------------------------------------

incr              :: SM Int Int
incr               = update (1+)
