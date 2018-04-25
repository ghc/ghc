-- Time-stamp: <Sat Jun 05 2010 01:37:17 Stardate: Stardate: [-28]3175.12 hwloidl>
--
-- Modular Arithmetic over Z_p
-----------------------------------------------------------------------------

-- @node Modular Arithmetic, ADT Matrix, Top, Top
-- @chapter Modular Arithmetic

module ModArithm (modHom, modSum, modDif, modProd, modQuot, modInv  
                 {-, Hom(hom) -} )  where

{-# SPECIALISE 
    modHom  :: Int -> Int -> Int
  #-}
modHom :: (Integral a) => a -> a -> a
modHom m x = x `mod` m

mapMod :: (Integral a) => (a -> a -> a) -> a -> a -> a -> a
mapMod f m x y = modHom m (f x y)

{-# SPECIALISE 
    modSum  :: Int -> Int -> Int -> Int
  #-}
modSum :: (Integral a) => a -> a -> a -> a
modSum = mapMod (+)  

{-# SPECIALISE 
    modDif  :: Int -> Int -> Int -> Int
  #-}
modDif :: (Integral a) => a -> a -> a -> a
modDif = mapMod (-)  

{-# SPECIALISE 
    modProd :: Int -> Int -> Int -> Int
  #-}
modProd :: (Integral a) => a -> a -> a -> a
modProd = mapMod (*)

{-# SPECIALISE 
    modQuot :: Int -> Int -> Int -> Int
  #-}
modQuot :: (Integral a) => a -> a -> a -> a
modQuot m x y = modProd m x (modInv m y)

{-# SPECIALISE 
       modInv :: Int -> Int -> Int
  #-}
modInv :: (Integral a) => a -> a -> a
modInv m x = let 
               (g,foo,inv) = gcdCF m x
             in
               if (g /= 1) 
                 then modHom m inv -- error $ "modInv: Input values " ++ (show (m,x)) ++ " are not relative prime!" ++ ("** Wrong GCD res: gcd= " ++ (show g) ++ "; but x*a+y*b= " ++ (show (m*foo+x*inv)))
                 else modHom m inv

gcdCF_with_check x y =
  let 
    res@(g,a,b) = gcdCF x y
  in
  if check_gcdCF x y res
    then res
    else error ("** Wrong GCD res: gcd= " ++ (show g) ++ "; but x*a+y*b= " ++ (show (x*a+y*b)))

{-# SPECIALISE 
    gcdCF :: Int -> Int -> (Int,Int,Int)
  #-}
gcdCF :: (Integral a) => a -> a -> (a,a,a)
gcdCF x y = gcdCF' x y 1 0 0 1 
            where gcdCF' x 0 x1 x2 _ _   = (x,x1,x2)
                  gcdCF' x y x1 x2 y1 y2 | x<y       = gcdCF' y x y1 y2 x1 x2 
                                         | otherwise = let 
                                                         q  = x `div` y
                                                         z  = x  - q*y
                                                         z1 = x1 - q*y1
                                                         z2 = x2 - q*y2
                                                       in
                                                         gcdCF' y z y1 y2 z1 z2


check_gcdCF :: (Integral a) => a -> a -> (a,a,a) -> Bool
check_gcdCF x y (g,a,b) = if (x*a+y*b)==g
                            then True
                            else False

