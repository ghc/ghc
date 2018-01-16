module Numbers(Number) where
data Number = Tolerant Float
-- if you are not using the Chalmers Haskell B compiler then remove the @
{- Number is the same as Float except that Number uses a comparison
   tolerance eps.
-}
instance Eq Number where
	Tolerant a == Tolerant b	= abs(a-b) < eps
	Tolerant a /= Tolerant b	= abs(a-b) > eps
instance Ord Number where
	Tolerant a <= Tolerant b	= a-eps < b
	Tolerant a < Tolerant b		= a < b-eps
instance Num Number where
	Tolerant a + Tolerant b		= Tolerant (a+b)
	Tolerant a - Tolerant b		= Tolerant (a-b)
	Tolerant a * Tolerant b		= Tolerant (a*b)
	negate (Tolerant a)		= Tolerant (-a)
	fromInteger n			= Tolerant (fromInteger n)
instance Fractional Number where
	Tolerant a / Tolerant b		= Tolerant (a/b)
instance Floating Number where
	sqrt (Tolerant a)		= Tolerant (sqrt a)
{- Allow both integral and floating denotations for numbers -}
instance Read Number where
	readsPrec p s = [(Tolerant n,t) | (n,t) <- readsPrec p s] -- ++
			--[(Tolerant (fromInteger n),t) | (n,t) <- readsPrec p s]
instance Show Number where
	showsPrec p (Tolerant x) = showsPrec p x
eps	= 0.0001 :: Float
