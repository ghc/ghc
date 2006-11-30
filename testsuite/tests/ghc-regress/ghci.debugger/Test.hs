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

infixr 5 :^ 
--test T{t=t1} = undefined

instance Show Show1 where
 show (S1 a b c) = show (a)

type Just1 = Maybe

