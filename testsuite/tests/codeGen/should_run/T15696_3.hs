import qualified Data.Set as S

main = print $
  let {-# noinline f #-}
      f () = T2
  in  S.fromList [f (), f ()]

data T = T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
