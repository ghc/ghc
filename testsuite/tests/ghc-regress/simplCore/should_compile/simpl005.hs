-- !!! CPR on newtype with polymorphic argument

{-# OPTIONS -O #-}

module ShouldCompile where

data StateM m s a = STM (s -> m (a,s)) 

instance Functor m => Functor (StateM m s) where
  fmap f (STM xs) =  STM (\s -> fmap (\ (x,s') -> (f x, s')) 
                    		     (xs s)                                
		         )
{-	With GHC 4.04 (first release) this program gave:

  panic! (the `impossible' happened):
          mk_cpr_let: not a product
      forall a{-ruq-} b{-rur-}.
      (a{-ruq-} -> b{-rur-})
      -> MonadLibrary.StateM{-r2o,x-} m{-a30Y-} s{-a30Z-} a{-ruq-}
      -> MonadLibrary.StateM{-r2o,x-} m{-a30Y-} s{-a30Z-} b{-rur-}

 The reason: 'Functor' is a newtype, whose element is a for-all type.

	newtype Functor f = Functor (forall a,b.  (a->b) -> f a -> f b)
-}
