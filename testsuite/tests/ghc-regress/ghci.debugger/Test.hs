module Test.Test2 where
import Data.Typeable
       
data Show1 = S1 Char Char Char
 deriving Typeable

data Strict = S2 Char !Char
	
data Opaque = forall a. O a
data List1 a = Nil | a :^ (List1 a)
  deriving Show

newtype MyInt = My Int
  deriving (Eq,Show,Num, Enum)

newtype MkT a = MkT a
  deriving (Show)

data MkT2 a = MkT2 (MkT a)
  deriving Show

data Param2 s r = P2 (FakeSTRef r (s(Param2 s r)))
		| P2Nil
data FakeSTRef r s = Ref s

testParam2 = O (P2 (Ref P2Nil))

infixr 5 :^ 
--test T{t=t1} = undefined

instance Show Show1 where
 show (S1 a b c) = show (a)

type Just1 = Maybe


data Unary = Unary

poly :: a -> ()
poly x = seq x ()