module M where

newtype GetDiscardingUnlift a = MkGetDiscardingUnlift
    { unGetDiscardingUnlift :: forall m. Either a m
    }

build :: forall a. a -> GetDiscardingUnlift a
build w =
    case build w of
        MkGetDiscardingUnlift getDiscardingUnlift' ->
         let getDiscardingUnlift'' :: forall m. Either a m
             getDiscardingUnlift'' = getDiscardingUnlift' @m
         in  MkGetDiscardingUnlift getDiscardingUnlift''
