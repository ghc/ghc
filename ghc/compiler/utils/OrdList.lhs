%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

This is useful, general stuff for the Native Code Generator.

Provide trees (of instructions), so that lists of instructions
can be appended in linear time.

\begin{code}
module OrdList (
	OrdList, 
        nilOL, isNilOL, unitOL, appOL, consOL, snocOL, concatOL,
        fromOL, toOL, foldOL
) where

infixl 5  `appOL`
infixl 5  `snocOL`
infixr 5  `consOL`

data OrdList a
  = Many [a]
  | Two (OrdList a) (OrdList a)
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
snocOL as b  = Two as (One b)
consOL a  bs = Two (One a) bs
concatOL aas = foldr Two None aas

isNilOL None        = True
isNilOL (One _)     = False
isNilOL (Two as bs) = isNilOL as && isNilOL bs
isNilOL (Many xs)   = null xs

appOL None bs   = bs
appOL as   None = as
appOL as   bs   = Two as bs

foldOL :: (a->b->b) -> b -> OrdList a -> b
foldOL k z None        = z
foldOL k z (One x)     = k x z
foldOL k z (Two b1 b2) = foldOL k (foldOL k z b2) b1
foldOL k z (Many xs)   = foldr k z xs

fromOL :: OrdList a -> [a]
fromOL ol 
   = flat ol []
     where
        flat None      rest = rest
        flat (One x)   rest = x:rest
        flat (Two a b) rest = flat a (flat b rest)
	flat (Many xs) rest = xs ++ rest

toOL :: [a] -> OrdList a
toOL xs = Many xs
\end{code}
