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
        fromOL, toOL
) where

infixl 5  `appOL`
infixl 5  `snocOL`
infixr 5  `consOL`

data OrdList a
  = Many (OrdList a) (OrdList a)
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
snocOL as b  = Many as (One b)
consOL a  bs = Many (One a) bs
concatOL aas = foldr Many None aas

isNilOL None         = True
isNilOL (One _)      = False
isNilOL (Many as bs) = isNilOL as && isNilOL bs

appOL None bs   = bs
appOL as   None = as
appOL as   bs   = Many as bs

fromOL :: OrdList a -> [a]
fromOL ol 
   = flat ol []
     where
        flat None       rest = rest
        flat (One x)    rest = x:rest
        flat (Many a b) rest = flat a (flat b rest)

toOL :: [a] -> OrdList a
toOL []     = None
toOL (x:xs) = Many (One x) (toOL xs)

\end{code}
