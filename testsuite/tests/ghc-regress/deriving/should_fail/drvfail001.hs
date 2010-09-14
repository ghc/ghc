{-  From: Ian Bayley 
    Sent: Tuesday, June 29, 1999 3:39 PM
    To: hugs-bugs@haskell.org
    Subject: Show for higher-order nested datatypes
    
    
    Is "deriving Show" meant to work for higher-order nested datatypes ?
    Hugs hangs when loading in the following file:
-}

module Foo where

type SqMat a = SM Nil a

data SM f a = ZeroS (f (f a)) | SuccS (SM (Cons f) a) 
		       deriving Show

-- Show (f (f a)), Show (SM (Cons f) a) => Show (SM f a)

data Nil a = MkNil deriving Show

data Cons f a = MkCons a (f a)
		       deriving Show



