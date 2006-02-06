{-# OPTIONS -fglasgow-exts #-}

{- This code, courtesy of Markus Lauer (markus.lauer-2006@lauerit.de)
   was rejected by the sophisticated wobbly-type impl in 6.4.1, and
   with a terrible error message:
     Sample.hs:22:40:
       Couldn't match `MayFail' against `MayFail'
         Expected type: Result s b
         Inferred type: Result MayFail a
       In the first argument of `Return', namely `Fail'
       In a case alternative: Fail -> return Fail

   Strangely it is accepted by the simplified impl in GHC 6.5. -}

module Sample where

data Safe
data MayFail

data Result s a where
   Ok   :: a   -> Result s       a
   Fail ::        Result MayFail a

newtype M s a = M { unM :: IO (Result s a) }

instance Monad (M s) where  

  return x = M (return (Ok x))
  
  {- this one gives a type error in 6.4.1 -}
  M m >>= k = M (do res <- m
                    case res of 
                         Ok x -> unM (k x)
                         Fail -> return Fail
                     )                    

  {- while this one works -}  
  -- M m >>= k = M (f m (unM . k))
  --   where 
  --     f :: IO (Result s a) -> (a -> IO (Result s b)) -> IO (Result s b)
  --     f m k = do res <- m
  --                case res of
  --                     Ok x -> k x
  --                     Fail -> return Fail
 
  

