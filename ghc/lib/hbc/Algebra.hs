module Algebra where
infixl 6 +. , -.
infixl 7 *. , /.

--
-- (x::A)->B is dependant functions
-- (x = y) A is equality in type A
--

-- For simplicity we may require decidable equality on the elements.
class {-(Eq a) =>-} SemiGroup a where
    (+.) :: a->a->a
--  assocAdd :: (x::a)->(y::a)->(z::a)->
--              ((a+.b)+.c = a+.(b+.c)) a

class (SemiGroup a) => Monoid a where
    zero :: a
--  leftZero :: (x::a) -> (zero +. x = x) a

class (Monoid a) => Group a where
    neg :: a->a
    (-.) :: a->a->a
    x -. y = x +. neg y
--  leftNeg :: (x::a) -> (neg x +. x = zero) a

class (Group a) => AbelianGroup a
--  commAdd :: (x::a)->(y::a)-> (x+.y = y+.x) a

class (AbelianGroup a) => Ring a where
    (*.) :: a->a->a
--  assocMul  :: (x::a)->(y::a)->(z::a)->
--               ((a*.b)*.c = a*.(b*.c)) a
--  distrRingL :: (x::a)->(y::a)->(z::a)->
--                (x*.(y+.z) = x*.y +. x*.z)
--  distrRingR :: (x::a)->(y::a)->(z::a)->
--                ((y+.z)*.x = y*.x +. z*.x)

class (Ring a) => UnityRing a where
    one :: a
--  leftOne :: (x::a)->(one *. x = x) a
--  rightOne :: (x::a)->(x *. one = x) a

class (Ring a) => CommutativeRing a
--  commMul :: (x::a)->(y::a)-> (x*.y = y*.x) a

class (CommutativeRing a, UnityRing a) => IntegralDomain a
--  noZeroDiv :: (x::a)->(y::a)-> (  (x*.y = zero) a  ->  Either ((x=zero) a) ((y=zero) a)  )

class (UnityRing a) => DivisionRing a where
    inv :: a->a
    (/.) :: a->a->a
    x /. y = x *. inv y
--  leftinv :: (x::a) -> (inv x *. x = one) a

class (DivisionRing a, CommutativeRing a) => Field a

-- Every finite integral domain is a field.

-- Unique Factorization Domain
class (IntegralDomain a) => UFD a
--  every non-zero element has a unique factorization

-- Principal Ideal Domain
class (IntegralDomain a) => PID a
--  every ideal is a principal ideal

---------------------------------------------------

-- [a] --
instance SemiGroup [a] where
    (+.) = (++)
instance Monoid [a] where
    zero = []

-- Bool --
instance SemiGroup Bool where
    (+.) = (||)
instance Monoid Bool where
    zero = False
instance Group Bool where
    neg = not
instance AbelianGroup Bool
instance Ring Bool where
    (*.) = (&&)
instance CommutativeRing Bool 
instance UnityRing Bool where
    one = True
instance DivisionRing Bool where
    inv x = x

-- Int --
instance SemiGroup Int where
    (+.) = (+)
instance Monoid Int where
    zero = 0
instance Group Int where
    neg = negate
instance AbelianGroup Int
instance Ring Int where
    (*.) = (*)
instance CommutativeRing Int
instance UnityRing Int where
    one = 1

-- Integer --
instance SemiGroup Integer where
    (+.) = (+)
instance Monoid Integer where
    zero = 0
instance Group Integer where
    neg = negate
instance AbelianGroup Integer
instance Ring Integer where
    (*.) = (*)
instance CommutativeRing Integer
instance UnityRing Integer where
    one = 1
instance IntegralDomain Integer

-- Q --
-- A new data tupe is needed to do the instance declarations
data Q = Q Rational {-#STRICT#-} deriving (Eq, Ord)
instance Text Q where
#if defined(__HBC__)
    -- not standard
    showsType _ = showString "Q"
#endif
    showsPrec n (Q p) = showsPrec n p
instance SemiGroup Q where
    Q a +. Q b = Q (a+b)
instance Monoid Q where
    zero = Q 0
instance Group Q where
    neg (Q a) = Q (-a)
instance AbelianGroup Q
instance Ring Q where
    Q a *. Q b = Q (a*b)
instance CommutativeRing Q
instance UnityRing Q where
    one = Q 1
instance IntegralDomain Q
instance DivisionRing Q where
    inv (Q x) = Q (recip x)
instance Field Q

