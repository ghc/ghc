-- !!! export a derived thingy which mentions an internal type
--
{- from simonpj; who adds:

   It is NOT ENOUGH to put

	   data OpaqueType deriving(Text)

   in the interface
-}

module ExportOpaque( OpaqueType ) where

data OpaqueType a = Con (FunnyInternalType a) deriving(Show)

data FunnyInternalType a = Junk11 | Junk2

instance Ord a => Show (FunnyInternalType a)
