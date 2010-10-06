{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- Example of improvement, due to George Russel

module Folders where

data Folder = Folder 

newtype SB x = SB x
newtype SS x = SS x 

data NodeArcsHidden = NodeArcsHidden

class HasSS hasS x | hasS -> x where
   toSS :: hasS -> SS x

instance HasSS (SB x) x where
   toSS (SB x) = (SS x)

class HMV option graph node where
   modd :: option -> graph -> node value -> IO ()

instance HMV NodeArcsHidden graph node 
      => HMV (Maybe NodeArcsHidden) graph node 
  where
   modd = error "burk"

gn :: HMV NodeArcsHidden graph node 
   => graph 
   -> SS (graph -> node Int -> IO ())
gn graph = fmapSS (\ arcsHidden -> (\ graph node -> modd arcsHidden graph node))
	          (toSS (error "C" :: SB (Maybe NodeArcsHidden)))

-- The call to modd gives rise to
--	HMV option graph node
-- The call to toSS gives rise to
--	HasSS (SB (Maybe NodeArcsHidden)) x  
-- where (toSS (error ...)) :: SS x
-- and hence arcsHidden :: x
--
-- Then improvement should give x = Maybe NodeArcsHidden
-- and hence option=Maybe NodeArcsHidden
   
fmapSS :: (a->b) -> SS a -> SS b
fmapSS = error "urk"
