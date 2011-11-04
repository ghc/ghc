%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1993-1998
%

This is useful, general stuff for the Native Code Generator.

Provide trees (of instructions), so that lists of instructions
can be appended in linear time.

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module OrdList (
	OrdList, 
        nilOL, isNilOL, unitOL, appOL, consOL, snocOL, concatOL,
        mapOL, fromOL, toOL, foldrOL, foldlOL
) where

infixl 5  `appOL`
infixl 5  `snocOL`
infixr 5  `consOL`

data OrdList a
  = Many [a]        -- Invariant: non-empty
  | Two (OrdList a) -- Invariant: non-empty
        (OrdList a) -- Invariant: non-empty
  | One  a
  | None

nilOL    :: OrdList a
isNilOL  :: OrdList a -> Bool

unitOL   :: a           -> OrdList a
snocOL   :: OrdList a   -> a         -> OrdList a
consOL   :: a           -> OrdList a -> OrdList a
appOL    :: OrdList a   -> OrdList a -> OrdList a
concatOL :: [OrdList a] -> OrdList a

nilOL        = None
unitOL as    = One as
snocOL None b    = One b
snocOL as   b    = Two as (One b)
consOL a    None = One a
consOL a    bs   = Two (One a) bs
concatOL aas = foldr appOL None aas

isNilOL None = True
isNilOL _    = False

appOL None bs   = bs
appOL as   None = as
appOL as   bs   = Two as bs

mapOL :: (a -> b) -> OrdList a -> OrdList b
mapOL _ None = None
mapOL f (One x) = One (f x)
mapOL f (Two x y) = Two (mapOL f x) (mapOL f y)
mapOL f (Many xs) = Many (map f xs)

instance Functor OrdList where
  fmap = mapOL

foldrOL :: (a->b->b) -> b -> OrdList a -> b
foldrOL _ z None        = z
foldrOL k z (One x)     = k x z
foldrOL k z (Two b1 b2) = foldrOL k (foldrOL k z b2) b1
foldrOL k z (Many xs)   = foldr k z xs

foldlOL :: (b->a->b) -> b -> OrdList a -> b
foldlOL _ z None        = z
foldlOL k z (One x)     = k z x
foldlOL k z (Two b1 b2) = foldlOL k (foldlOL k z b1) b2
foldlOL k z (Many xs)   = foldl k z xs

fromOL :: OrdList a -> [a]
fromOL ol 
   = flat ol []
     where
        flat None      rest = rest
        flat (One x)   rest = x:rest
        flat (Two a b) rest = flat a (flat b rest)
        flat (Many xs) rest = xs ++ rest

toOL :: [a] -> OrdList a
toOL [] = None
toOL xs = Many xs
\end{code}
