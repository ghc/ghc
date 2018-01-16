
-- | Pretty printer classes
module Data.Array.Parallel.Pretty
        ( module Text.PrettyPrint
        , PprPhysical(..)
        , PprVirtual (..))
where
import Text.PrettyPrint
import qualified Data.Vector            as V
import Data.Vector                      (Vector)


-- | Pretty print the physical structure of data.
class PprPhysical a where
 pprp :: a -> Doc

instance PprPhysical Bool where
 pprp = text . show

instance PprPhysical Int where
 pprp = text . show 
 
instance PprPhysical Double where
 pprp = text . show 
 
 
-- | Pretty print virtual \/ logical structure of data.
class PprVirtual a where
 pprv :: a -> Doc

instance PprVirtual Bool where
 pprv = text . show

instance PprVirtual Int where
 pprv = text . show 
 
instance PprVirtual Double where
 pprv = text . show 


-- Instances ------------------------------------------------------------------ 
instance (PprPhysical a, PprPhysical b)
        => PprPhysical (a, b) where
 pprp (x, y)
  = vcat
        [ text "Tuple2"
        , nest 4 $ pprp x
        , nest 4 $ pprp y]

instance PprPhysical a
        => PprPhysical (Vector a) where
 pprp vec
        = brackets 
        $ hcat
        $ punctuate (text ", ") 
        $ V.toList $ V.map pprp vec

