{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

module Natural where

import Prelude hiding (Integer, abs, sum)

import StrictPrim
import Type


{-# NOINLINE timesNatural #-}
timesNatural :: Natural -> Natural -> Natural
timesNatural (Natural !n1 !arr1) (Natural !n2 !arr2) =
    runStrictPrim $ do
        maxOutLen <- return (1 + n1 + n2)
        marr <- newWordArray maxOutLen
        len <- preLoop marr
        narr <- unsafeFreezeWordArray marr
        return $! Natural len narr
  where
    preLoop marr = do
        x <- indexWordArrayM arr1 0
        y <- indexWordArrayM arr2 0
        let (# cry, prod #) = timesWord2 x y
        writeWordArray marr 0 prod
        outerLoop1 1 marr 0 cry

    outerLoop1 !nx !marr !carryhi !carrylo
        | nx < n2 = do
            (cryhi, crylo, sum) <- innerLoop1xi nx 0 0 carryhi carrylo
            writeWordArray marr nx sum
            outerLoop1 (nx + 1) marr cryhi crylo
        | otherwise = outerLoop1a nx marr carryhi carrylo

    outerLoop1a !nx !marr !carryhi !carrylo
        | nx < n1 - 1 = do
            (cryhi, crylo, sum) <- innerLoop1yi nx 0 0 carryhi carrylo
            writeWordArray marr nx sum
            outerLoop1a (nx + 1) marr cryhi crylo
        | otherwise = outerLoop2 nx marr carryhi carrylo

    innerLoop1xi !xi !yi !carryhi !carrylo !sum
        | xi >= 0 = do
            x <- indexWordArrayM arr1 xi
            y <- indexWordArrayM arr2 yi
            let (# !cry0, !prod #) = timesWord2 x y
                (# !cry1, !sum1 #) = plusWord2 prod sum
                (# !tcryhi, !crylo #) = plusWord2C carrylo cry0 cry1
                !cryhi = plusWord carryhi tcryhi
            innerLoop1xi (xi - 1) (yi + 1) cryhi crylo sum1
        | otherwise = return $! (carryhi, carrylo, sum)

    innerLoop1yi !xi !yi !carryhi !carrylo !sum
        | yi < n2 = do
            x <- indexWordArrayM arr1 xi
            y <- indexWordArrayM arr2 yi
            let (# !cry0, !prod #) = timesWord2 x y
                (# !cry1, !sum1 #) = plusWord2 prod sum
                (# !tcryhi, !crylo #) = plusWord2C carrylo cry0 cry1
                !cryhi = plusWord carryhi tcryhi
            innerLoop1yi (xi - 1) (yi + 1) cryhi crylo sum1
        | otherwise = return $! (carryhi, carrylo, sum)

    outerLoop2 !nx !marr !carryhi !carrylo
        | nx < n1 + n2 - 1 = do
            (cryhi, crylo, sum)
                    <- innerLoop2 (n1 - 1) (nx - n1 + 1) 0 carryhi carrylo
            writeWordArray marr nx sum
            outerLoop2 (nx + 1) marr cryhi crylo
        | carrylo /= 0 = do
            writeWordArray marr nx carrylo
            return $! nx + 1
        | otherwise = return $! nx

    innerLoop2 !xi !yi !carryhi !carrylo !sum
        | yi < n2 = do
            x <- indexWordArrayM arr1 xi
            y <- indexWordArrayM arr2 yi
            let (# !cry0, !prod #) = timesWord2 x y
                (# !cry1, !sum1 #) = plusWord2 prod sum
                (# !tcryhi, !crylo #) = plusWord2C carrylo cry0 cry1
                !cryhi = plusWord carryhi tcryhi
            innerLoop2 (xi - 1) (yi + 1) cryhi crylo sum1
        | otherwise = return $! (carryhi, carrylo, sum)
