module Number(Number, isInteger) where
data Number = I Integer | F Double

toF (I i) = fromInteger i
toF (F f) = f

toI (I i) = i
toI (F f) = round f

-- slow!!
toN x | fromInteger i == x = I i where i = truncate x
toN x = F x

isInteger (I i) = True
isInteger (F x) = fromInteger (truncate x) == x

instance Eq Number where
    I x == I y = x == y
    x   == y   = toF x == toF y

instance Ord Number where
    I x <= I y = x <= y
    x   <= y   = toF x <= toF y

instance Text Number where
    showsPrec p (I i) = showsPrec p i
--    showsPrec p (F f) | fromInteger i == f = showsPrec p i where i = truncate f
    showsPrec p (F f) = 
        let s = reverse (show f)
	    s' = if 'e' `notElem` s then dropWhile (=='0') (tail s) else s
	    s'' = if head s' == '.' then tail s' else s'
	in  showString (reverse s'')
    readsPrec p s = [(I i, s) | (i, s)<-readsPrec p s] ++
                    [(F i, s) | (i, s)<-readsPrec p s]

#if defined(__HBC__)
    showsType _ = showString "Number"
#endif

instance Num Number where
    I x + I y  = I (x+y)
    x   + y    = toN (toF x + toF y)
    I x - I y  = I (x-y)
    x   - y    = toN (toF x - toF y)
    I x * I y  = I (x*y)
    x   * y    = toN (toF x * toF y)
    negate (I x) = I (-x)
    negate (F x) = F (-x)
    abs x = if x <= 0 then -x else x
    signum x = if x <= 0 then if x==0 then 0 else -1 else 1
    fromInteger i = I i

instance Ix Number where
    range (x, y) = [I i | i<-[toI x .. toI y]]
    index (x, y) i = fromInteger (toI i - toI x)
    inRange (x, y) i = toI x <= toI i && toI i <= toI y

instance Integral Number where
    quotRem (I x) (I y) = case quotRem x y of (q,r) -> (I q, I r)
    quotRem x y = let q = truncate (x' / y')
                      x' = toF x
		      y' = toF y
                  in  (I q, toN (x' - fromInteger q * y'))
    toInteger (I i) = i
    toInteger (F f) = round f

instance Enum Number where
    enumFrom (I i) = [I x | x<-[i..]]
    enumFrom (F i) = [F x | x<-[i..]]
    enumFromThen (I i) (I j) = [I x | x<-[i,j..]]
    enumFromThen i j = [F x | x<-[toF i,toF j..]]

instance Real Number where
    toRational (I i) = i % 1
    toRational (F f) = toRational f

instance Fractional Number where
    I x / I y | r == 0 = I q where (q,r) = quotRem x y
    x / y = toN (toF x / toF y)
    fromRational r | denominator r == 0 = I (numerator r)
    fromRational r = toN (fromRational r)

instance RealFrac Number where
    properFraction (I i) = (fromInteger i, I 0)
    properFraction (F f) = let (i,x) = properFraction f in (i, toN x)
    truncate (I i) = fromInteger i
    truncate (F f) = truncate f
    round (I i) = fromInteger i
    round (F f) = round f
    ceiling (I i) = fromInteger i
    ceiling (F f) = ceiling f
    floor (I i) = fromInteger i
    floor (F f) = floor f

instance RealFloat Number where
    floatRadix x = floatRadix (toF x)
    floatDigits x = floatDigits (toF x)
    floatRange x = floatRange (toF x)
    decodeFloat x = decodeFloat (toF x)
    encodeFloat m e = toN (encodeFloat m e)
    exponent x = exponent (toF x)
    significand x = toN (significand (toF x))
    scaleFloat n x = toN (scaleFloat n (toF x))

instance Floating Number where
    pi = F pi
    exp = toN . exp . toF
    log = toN . log . toF
    sqrt = toN . sqrt . toF
    x ** y = toN (toF x ** toF y)
    logBase x y = toN (logBase (toF x) (toF y))
    sin = toN . sin . toF
    cos = toN . cos . toF
    tan = toN . tan . toF
    asin = toN . asin . toF
    acos = toN . acos . toF
    atan = toN . atan . toF
    sinh = toN . sinh . toF
    cosh = toN . cosh . toF
    tanh = toN . tanh . toF
    asinh = toN . asinh . toF
    acosh = toN . acosh . toF
    atanh = toN . atanh . toF

