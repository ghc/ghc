%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1993-1998
%

This is useful, general stuff for the Native Code Generator.

Provide trees (of instructions), so that lists of instructions
can be appended in linear time.

\begin{code}
module OrdList (
        OrdList,
        nilOL, isNilOL, unitOL, appOL, consOL, snocOL, concatOL,
        mapOL, fromOL, toOL, foldrOL, foldlOL
) where

infixl 5  `appOL`
infixl 5  `snocOL`
infixr 5  `consOL`

data OrdList a
  = None
  | One a
  | Many [a]          -- Invariant: non-empty
  | Cons a (OrdList a)
  | Snoc (OrdList a) a
  | Two (OrdList a) -- Invariant: non-empty
        (OrdList a) -- Invariant: non-empty


nilOL    :: OrdList a
isNilOL  :: OrdList a -> Bool

unitOL   :: a           -> OrdList a
snocOL   :: OrdList a   -> a         -> OrdList a
consOL   :: a           -> OrdList a -> OrdList a
appOL    :: OrdList a   -> OrdList a -> OrdList a
concatOL :: [OrdList a] -> OrdList a

nilOL        = None
unitOL as    = One as
snocOL as   b    = Snoc as b
consOL a    bs   = Cons a bs
concatOL aas = foldr appOL None aas

isNilOL None = True
isNilOL _    = False

None  `appOL` b     = b
a     `appOL` None  = a
One a `appOL` b     = Cons a b
a     `appOL` One b = Snoc a b
a     `appOL` b     = Two a b

fromOL :: OrdList a -> [a]
fromOL a = go a []
  where go None       acc = acc
        go (One a)    acc = a : acc
        go (Cons a b) acc = a : go b acc
        go (Snoc a b) acc = go a (b:acc)
        go (Two a b)  acc = go a (go b acc)
        go (Many xs)  acc = xs ++ acc

mapOL :: (a -> b) -> OrdList a -> OrdList b
mapOL _ None = None
mapOL f (One x) = One (f x)
mapOL f (Cons x xs) = Cons (f x) (mapOL f xs)
mapOL f (Snoc xs x) = Snoc (mapOL f xs) (f x)
mapOL f (Two x y) = Two (mapOL f x) (mapOL f y)
mapOL f (Many xs) = Many (map f xs)

instance Functor OrdList where
  fmap = mapOL

foldrOL :: (a->b->b) -> b -> OrdList a -> b
foldrOL _ z None        = z
foldrOL k z (One x)     = k x z
foldrOL k z (Cons x xs) = k x (foldrOL k z xs)
foldrOL k z (Snoc xs x) = foldrOL k (k x z) xs
foldrOL k z (Two b1 b2) = foldrOL k (foldrOL k z b2) b1
foldrOL k z (Many xs)   = foldr k z xs

foldlOL :: (b->a->b) -> b -> OrdList a -> b
foldlOL _ z None        = z
foldlOL k z (One x)     = k z x
foldlOL k z (Cons x xs) = foldlOL k (k z x) xs
foldlOL k z (Snoc xs x) = k (foldlOL k z xs) x
foldlOL k z (Two b1 b2) = foldlOL k (foldlOL k z b1) b2
foldlOL k z (Many xs)   = foldl k z xs

toOL :: [a] -> OrdList a
toOL [] = None
toOL xs = Many xs
\end{code}
