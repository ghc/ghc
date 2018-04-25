
-- | Closure converted tuple data constructors used by the vectoriser.
module Data.Array.Parallel.Prelude.Tuple 
        (tup2, tup3, tup4, tup5)
where  
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray.PRepr
import qualified Data.Array.Parallel.PArray     as PA


tup2    :: (PA a, PA b)
        => a :-> b :-> (a, b)
tup2    = closure2' (,) PA.zip
{-# INLINE tup2 #-}


tup3    :: (PA a, PA b, PA c)
        => a :-> b :-> c :-> (a, b, c)
tup3    = closure3' (,,) PA.zip3
{-# INLINE tup3 #-}


tup4    :: (PA a, PA b, PA c, PA d)
        => a :-> b :-> c :-> d :-> (a, b, c, d)
tup4    = closure4' (,,,) PA.zip4
{-# INLINE tup4 #-}


tup5    :: (PA a, PA b, PA c, PA d, PA e)
        =>  a :-> b :-> c :-> d :-> e :-> (a, b, c, d, e)
tup5    = closure5' (,,,,) PA.zip5
{-# INLINE tup5 #-}
