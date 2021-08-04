module Basement.NormalForm
    ( NormalForm(..)
    , deepseq
    , force
    ) where

import Basement.Compat.Base
import Basement.Compat.C.Types
import Basement.Compat.Natural
import Basement.Types.OffsetSize
import Basement.Types.Char7
import Basement.Types.Word128 (Word128)
import Basement.Types.Word256 (Word256)
import Basement.Bounded
import Basement.Endianness

-- | Data that can be fully evaluated in Normal Form
--
class NormalForm a where
    toNormalForm :: a -> ()

deepseq :: NormalForm a => a -> b -> b
deepseq a b = toNormalForm a `seq` b

force :: NormalForm a => a -> a
force a = toNormalForm a `seq` a

-----
-- GHC / base types

instance NormalForm Int8    where toNormalForm !_ = ()
instance NormalForm Int16   where toNormalForm !_ = ()
instance NormalForm Int32   where toNormalForm !_ = ()
instance NormalForm Int64   where toNormalForm !_ = ()
instance NormalForm Int     where toNormalForm !_ = ()
instance NormalForm Integer where toNormalForm !_ = ()

instance NormalForm Word8   where toNormalForm !_ = ()
instance NormalForm Word16  where toNormalForm !_ = ()
instance NormalForm Word32  where toNormalForm !_ = ()
instance NormalForm Word64  where toNormalForm !_ = ()
instance NormalForm Word    where toNormalForm !_ = ()
instance NormalForm Natural where toNormalForm !_ = ()

instance NormalForm Float  where toNormalForm !_ = ()
instance NormalForm Double where toNormalForm !_ = ()

instance NormalForm Char where toNormalForm !_ = ()
instance NormalForm Bool where toNormalForm !_ = ()
instance NormalForm ()   where toNormalForm !_ = ()

-----
-- C Types
instance NormalForm CChar  where toNormalForm !_ = ()
instance NormalForm CUChar where toNormalForm !_ = ()
instance NormalForm CSChar where toNormalForm !_ = ()

instance NormalForm CShort  where toNormalForm !_ = ()
instance NormalForm CUShort where toNormalForm !_ = ()
instance NormalForm CInt    where toNormalForm !_ = ()
instance NormalForm CUInt   where toNormalForm !_ = ()
instance NormalForm CLong   where toNormalForm !_ = ()
instance NormalForm CULong  where toNormalForm !_ = ()
instance NormalForm CLLong  where toNormalForm !_ = ()
instance NormalForm CULLong where toNormalForm !_ = ()

instance NormalForm CFloat  where toNormalForm !_ = ()
instance NormalForm CDouble where toNormalForm !_ = ()

instance NormalForm (Ptr a) where toNormalForm !_ = ()

-----
-- Basic Foundation primitive types
instance NormalForm (Offset a) where toNormalForm !_ = ()
instance NormalForm (CountOf a) where toNormalForm !_ = ()

instance NormalForm Char7 where toNormalForm !_ = ()
instance NormalForm Word128 where toNormalForm !_ = ()
instance NormalForm Word256 where toNormalForm !_ = ()
instance NormalForm (Zn n) where toNormalForm = toNormalForm . unZn
instance NormalForm (Zn64 n) where toNormalForm = toNormalForm . unZn64

-----
-- composed type

instance NormalForm a => NormalForm (Maybe a) where
    toNormalForm Nothing  = ()
    toNormalForm (Just a) = toNormalForm a `seq` ()
instance (NormalForm l, NormalForm r) => NormalForm (Either l r) where
    toNormalForm (Left l)  = toNormalForm l `seq` ()
    toNormalForm (Right r) = toNormalForm r `seq` ()
instance NormalForm a => NormalForm (LE a) where
    toNormalForm (LE a) = toNormalForm a `seq` ()
instance NormalForm a => NormalForm (BE a) where
    toNormalForm (BE a) = toNormalForm a `seq` ()

instance NormalForm a => NormalForm [a] where
    toNormalForm []     = ()
    toNormalForm (x:xs) = toNormalForm x `seq` toNormalForm xs

instance (NormalForm a, NormalForm b) => NormalForm (a,b) where
    toNormalForm (a,b) = toNormalForm a `seq` toNormalForm b

instance (NormalForm a, NormalForm b, NormalForm c) => NormalForm (a,b,c) where
    toNormalForm (a,b,c) = toNormalForm a `seq` toNormalForm b `seq` toNormalForm c

instance (NormalForm a, NormalForm b, NormalForm c, NormalForm d) => NormalForm (a,b,c,d) where
    toNormalForm (a,b,c,d) = toNormalForm a `seq` toNormalForm b `seq` toNormalForm c `seq` toNormalForm d

instance (NormalForm a, NormalForm b, NormalForm c, NormalForm d, NormalForm e)
      => NormalForm (a,b,c,d,e) where
    toNormalForm (a,b,c,d,e) =
        toNormalForm a `seq` toNormalForm b `seq` toNormalForm c `seq` toNormalForm d `seq`
        toNormalForm e

instance (NormalForm a, NormalForm b, NormalForm c, NormalForm d, NormalForm e, NormalForm f)
      => NormalForm (a,b,c,d,e,f) where
    toNormalForm (a,b,c,d,e,f) =
        toNormalForm a `seq` toNormalForm b `seq` toNormalForm c `seq` toNormalForm d `seq`
        toNormalForm e `seq` toNormalForm f

instance (NormalForm a, NormalForm b, NormalForm c, NormalForm d, NormalForm e, NormalForm f, NormalForm g)
      => NormalForm (a,b,c,d,e,f,g) where
    toNormalForm (a,b,c,d,e,f,g) =
        toNormalForm a `seq` toNormalForm b `seq` toNormalForm c `seq` toNormalForm d `seq`
        toNormalForm e `seq` toNormalForm f `seq` toNormalForm g
instance (NormalForm a, NormalForm b, NormalForm c, NormalForm d, NormalForm e, NormalForm f, NormalForm g, NormalForm h)
      => NormalForm (a,b,c,d,e,f,g,h) where
    toNormalForm (a,b,c,d,e,f,g,h) =
        toNormalForm a `seq` toNormalForm b `seq` toNormalForm c `seq` toNormalForm d `seq`
        toNormalForm e `seq` toNormalForm f `seq` toNormalForm g `seq` toNormalForm h
