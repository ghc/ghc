{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- !!! One method class from Sergey Mechveliani 
--     showed up problematic newtype dict rep.
module Main where
import Data.Ratio

class MBConvertible a b  where  cm :: a -> b -> Maybe b

c :: MBConvertible a b => a -> b -> b
c                         a    b =  case  cm a b  
                                    of 
                                      Just b' -> b'
                                      _       -> error "c a b  failed"


instance MBConvertible Int Int  where  cm a _ = Just a

instance (MBConvertible a b,Integral b) => MBConvertible a (Ratio b)
  where
  cm a f =  case  cm a (numerator f)  of  Just a' -> Just (a'%1)
                                          _       -> Nothing 
  
main =  let  f  = 1%1 :: Ratio Int
             n2 = 2::Int
             g  = (c n2 f) + f
        in
             putStr (shows g "\n")
