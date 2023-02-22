{-# OPTIONS_GHC -fspecialize-aggressively -fexpose-all-unfoldings -Wno-missing-methods #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances,
             DataKinds, MultiParamTypeClasses, RankNTypes, MonoLocalBinds #-}
module T23024a where

import System.IO.Unsafe
import Control.Monad.ST ( ST, runST )
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.ForeignPtr ( unsafeWithForeignPtr )

class MyNum a where
  fi :: a

class (MyNum a, Eq a) => MyReal a

class (MyReal a) => MyRealFrac a  where
  fun :: a -> ()

class (MyRealFrac a, MyNum a) => MyRealFloat a

instance MyNum Double
instance MyReal Double
instance MyRealFloat Double
instance MyRealFrac Double

newtype Vector a = Vector (ForeignPtr a)

class GVector v a where
instance Storable a => GVector Vector a

vunstream :: () -> ST s (v a)
vunstream () = vunstream ()

empty :: GVector v a => v a
empty = runST (vunstream ())
{-# NOINLINE empty #-}

instance (Storable a, Eq a) => Eq (Vector a) where
  xs == ys = idx xs == idx ys

{-# NOINLINE idx #-}
idx (Vector fp) = unsafePerformIO
                        $ unsafeWithForeignPtr fp $ \p ->
                          peekElemOff p 0

instance MyNum (Vector Double)
instance (MyNum (Vector a), Storable a, Eq a) => MyReal (Vector a)
instance (MyNum (Vector a), Storable a, Eq a) => MyRealFrac (Vector a)
instance (MyNum (Vector a), Storable a, MyRealFloat a) => MyRealFloat (Vector a)

newtype ORArray a = A a

instance (Eq a) => Eq (ORArray a) where
  A x == A y = x == y

instance (MyNum (Vector a)) => MyNum (ORArray a)
instance (MyNum (Vector a), Storable a, Eq a) => MyReal (ORArray a)
instance (MyRealFrac (Vector a), Storable a, Eq a) => MyRealFrac (ORArray a)
instance (MyRealFloat (Vector a), Storable a, Eq a) => MyRealFloat (ORArray a)

newtype Ast r = AstConst (ORArray r)

instance Eq (Ast a) where
  (==) = undefined

instance MyNum (ORArray a) => MyNum (Ast a) where
  fi = AstConst fi

instance MyNum (ORArray a) => MyReal (Ast a)
instance MyRealFrac (ORArray a) => MyRealFrac (Ast a) where
  {-# INLINE fun #-}
  fun x = ()
  
instance MyRealFloat (ORArray a) => MyRealFloat (Ast a)

class (MyRealFloat a) => Tensor a
instance (MyRealFloat a, MyNum (Vector a), Storable a) => Tensor (Ast a)

gradientFromDelta :: Storable a => Ast a -> Vector a
gradientFromDelta _ = empty
{-# NOINLINE gradientFromDelta #-}
