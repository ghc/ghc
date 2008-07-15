{-# LANGUAGE TypeFamilies, GADTs #-}

data family HiThere a :: *

data instance HiThere () where
    HiThere :: HiThere ()
