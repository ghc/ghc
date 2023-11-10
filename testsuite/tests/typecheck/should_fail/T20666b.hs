{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module T20666b where

type family F a

class Eq (F a) => D a
class Eq (F a) => C a

instance D [a] => C [a]

