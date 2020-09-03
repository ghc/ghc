{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
module PM where

data T a  where
  T :: T b -> T (a, b)

pattern P a <- (T a)

massive :: T recty -> ()
massive (P (P (P (P (P (P (P (P (P (P (P (P (P (P (P (P (P _))))))))))))))))) = ()
