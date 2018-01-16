{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module T8002a where

import T8002b

class QA a where
   type QRep a
   type QRep a = QRep (Maybe a)

instance QA () where
   type QRep () = ()
