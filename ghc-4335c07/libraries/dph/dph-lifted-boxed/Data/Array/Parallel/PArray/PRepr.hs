
module Data.Array.Parallel.PArray.PRepr
        ( PRepr
        , PA (..)
        , toVectorPA
        , fromVectorPA)
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.Types
import qualified Data.Vector            as V
import Data.Vector                      (Vector)
import Data.Word


-- Wrap -----------------------------------------------------------------------
newtype instance PData  (Wrap a)   = PWrap  (PData  a)
newtype instance PDatas (Wrap a)   = PWraps (PDatas a)

instance PA a => PR (Wrap a) where
  fromVectorPR vec
        = PWrap $ fromVectorPA $ V.map unWrap vec

  toVectorPR (PWrap pdata)
        = V.map Wrap $ toVectorPA pdata


-------------------------------------------------------------------------------
-- | Fake PRepr and PA classes.
--   The vectoriser wants to build PA instances involving these types, 
--   but we don't need that support for this library.
type family PRepr a

class PR (PRepr a) => PA a where
  toPRepr       :: a                -> PRepr a
  fromPRepr     :: PRepr a          -> a

  toArrPRepr    :: PData a          -> PData (PRepr a)
  fromArrPRepr  :: PData (PRepr a)  -> PData a

  toArrPReprs   :: PDatas a         -> PDatas (PRepr a)
  fromArrPReprs :: PDatas (PRepr a) -> PDatas a


-- PA Functions ---------------------------------------------------------------
fromVectorPA    :: PA a => Vector a -> PData a
fromVectorPA vec
        = fromArrPRepr
        $ fromVectorPR (V.map toPRepr vec)


toVectorPA      :: PA a => PData a -> Vector a
toVectorPA pdata
        = V.map fromPRepr
        $ toVectorPR (toArrPRepr pdata)


-- Void -----------------------------------------------------------------------
type instance PRepr Void
        = Void

instance PA Void where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id


-- () ------------------------------------------------------------------------
type instance PRepr ()
        = ()

instance PA () where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id


-- Int ------------------------------------------------------------------------
type instance PRepr Int
        = Int

instance PA Int where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id


-- Double ---------------------------------------------------------------------
type instance PRepr Double
        = Double

instance PA Double where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id


-- Word8 ----------------------------------------------------------------------
type instance PRepr Word8
        = Word8

instance PA Word8 where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id


-- Tuple2 ---------------------------------------------------------------------
type instance PRepr (a, b)      
        = (Wrap a, Wrap b)

instance (PA a, PA b) => PA (a, b) where
  toPRepr (a, b)
        = (Wrap a, Wrap b)

  fromPRepr (Wrap a, Wrap b)
        = (a, b)

  toArrPRepr (PTuple2 as bs)
        = PTuple2 (PWrap as) (PWrap bs)

  fromArrPRepr (PTuple2 (PWrap as) (PWrap bs))
        = PTuple2 as bs

  toArrPReprs (PTuple2s as bs)
        = PTuple2s (PWraps as) (PWraps bs)

  fromArrPReprs (PTuple2s (PWraps as) (PWraps bs))
        = PTuple2s as bs


-- PArray ---------------------------------------------------------------------
type instance PRepr (PArray a)
        = PArray (PRepr a)

instance PA a => PA (PArray a) where
  toPRepr (PArray n xs) 
        = PArray n $ toArrPRepr xs

  fromPRepr (PArray n xs)
        = PArray n $ fromArrPRepr xs

  toArrPRepr   (PNested xs)
        = PNested  $ V.map toPRepr xs

  fromArrPRepr (PNested xs)
        = PNested  $ V.map fromPRepr xs

  toArrPReprs (PNesteds vec)
        = PNesteds $ V.map toArrPRepr vec

  fromArrPReprs (PNesteds vec)
        = PNesteds $ V.map fromArrPRepr vec


-- Bool -----------------------------------------------------------------------
type instance PRepr Bool
        = Bool

instance PA Bool where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id



