{-# OPTIONS -cpp -fglasgow-exts -O2 -optc-O3 -funbox-strict-fields #-}
--
-- Reverse complement benchmark
--
-- Written by Don Stewart
--
import GHC.Base
import Data.Char
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe   as B
import qualified Data.ByteString.Internal as B

main = B.getContents >>= process B.empty []

process h b ps
    | h `seq` b `seq` ps `seq` False = undefined
    | B.null ps = write h b
    | x == '>'  = write h b >> process h' [] ps'
    | x == '\n' = process h b xs
    | otherwise = process h ((complement . toUpper $ x) : b) xs
    where (x,xs)   = (B.w2c (B.unsafeHead ps), B.unsafeTail ps)
          (h',ps') = B.break (=='\n') ps

write h s
    | B.null h  = return ()
    | otherwise = B.putStrLn h >> write_ (B.pack s)
    where
        write_ t
            | B.null t  = return ()
            | otherwise = let (a,b) = B.splitAt 60 t in B.putStrLn a >> write_ b

complement (C# i) = C# (indexCharOffAddr# arr (ord# i -# 65#))
    where arr = "TVGH  CD  M KN   YSAABW R"#
