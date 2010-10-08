{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, ExistentialQuantification,
             ScopedTypeVariables #-}

-- TypeVal.hs: dependent types, here we come!
module TypeVal where

-- The functional dependency is necessary to get ghc to accept
-- some of the instances below.  Is this a bug?
class TypeVal a t | t -> a where
    -- typeToVal should ignore its argument.
    typeToVal :: t -> a

data Wrapper a = forall t . (TypeVal a t) => Wrapper t
instance Show a => Show (Wrapper a) where
   show x = "valToType " ++ show (getValue x)

class ValToType a where
    valToType :: a -> Wrapper a

getValue :: Wrapper a -> a
getValue (Wrapper p) = case p of {(_::t) -> typeToVal (undefined::t)}

-- Instances of ValToType for standard types.  This would also be a
-- straightforward compiler extension for the general case.
---- ValToType Bool
data TRUE = Dummy1
instance TypeVal Bool TRUE where typeToVal = const True
data FALSE = Dummy2
instance TypeVal Bool FALSE where typeToVal = const False

instance ValToType Bool where
    valToType True = Wrapper (undefined :: TRUE)
    valToType False = Wrapper (undefined :: FALSE)

---- ValToType Int, ValToType Integer
data Zero = Dummy3
instance TypeVal Integer Zero where typeToVal = const 0
data Succ n = Dummy4
instance (TypeVal Integer n) => TypeVal Integer (Succ n)
  where typeToVal = const (typeToVal (undefined :: n) + 1)
data Pred n = Dummy5
instance (TypeVal Integer n) => TypeVal Integer (Pred n)
  where typeToVal = const (typeToVal (undefined :: n) - 1)
data Dbl n = Dummy6
instance (TypeVal Integer n) => TypeVal Integer (Dbl n)
  where typeToVal = const (typeToVal (undefined :: n) * 2)

instance ValToType Integer where
    valToType n | n == 0 = Wrapper (undefined :: Zero)
	        | even n    =
		   case valToType (div n 2) of {Wrapper x ->
	           case x of {(_ :: t) ->
		   Wrapper (undefined :: Dbl t)}}
	        | n > 0  =
		   case valToType (n-1) of {Wrapper x ->
	           case x of {(_ :: t) ->
		   Wrapper (undefined :: Succ t)}}
	        | n < 1  =
		   case valToType (n+1) of {Wrapper x ->
	           case x of {(_ :: t) ->
		   Wrapper (undefined :: Pred t)}}

--- ValToType (a,b)
--- Doesn't work.  Perhaps a bug in ghc?
--- Don't need it yet, anyway.
--instance (TypeVal a1 t1, TypeVal a2 t2) => TypeVal (a1,a2) (t1,t2)
--    where typeToVal (x,y) = (typeToVal x, typeToVal y)

--instance (ValToType a, ValToType b) => ValToType (a,b) where
--    valToType (a,b) = case valToType a of {x ->
--		    case valToType b of {y ->
--		    Wrapper (x,y)}}

data NIL a = Dummy20
instance TypeVal [a] (NIL a)
  where typeToVal = const []
data CONS t r = Dummy21
instance (TypeVal [a] r, TypeVal a t) => TypeVal [a] (CONS t r)
  where typeToVal = const (typeToVal (undefined::t):typeToVal (undefined::r))

instance (ValToType a) => ValToType [a] where
  valToType [] = Wrapper (undefined::NIL a)
  valToType (x:xs) = case valToType x of {Wrapper x' ->
		     case x' of {(_::xt) ->
		     case valToType xs of {Wrapper xs' ->
		     case xs' of {(_::xst) ->
		     Wrapper (undefined::CONS xt xst)}}}}

