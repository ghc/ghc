{-# LANGUAGE StrictData #-}
-- Regression test from #27857: We store a function that loops indefinitely
-- in a strict field.
module Main (main) where
import Control.Monad

data Body
    = BNone
    | BStream ((Int -> IO ()) -> IO () -> IO ())
    | BIface (Int -> IO ())
    | BBuilder String
    | BFile Int

data Obj = Obj [String] Body Int

newtype Resp = Resp Obj

stream :: Int -> [String] -> ((Int -> IO ()) -> IO () -> IO ()) -> Resp
stream st hdr b = Resp $ Obj (show st : hdr) (BStream b) 0

noBody :: Int -> [String] -> Resp
noBody st hdr = Resp $ Obj (show st : hdr) BNone 0

infinite :: Resp
infinite = stream 200 ["text/plain"] body
  where
    body :: (Int -> IO ()) -> IO () -> IO ()
    body write flush = do
        let go n = write n *> flush *> go (succ n)
        go (0 :: Int)

other :: Resp
other = noBody 404 []

name :: Body -> String
name b = case b of
    BNone -> "None"
    BStream _ -> "Stream"
    BIface _ -> "Iface"
    BBuilder _ -> "Builder"
    BFile _ -> "File"

main :: IO ()
main = forM_ [("infinite", infinite), ("other", other)] $ \(n, Resp (Obj _ b _)) ->
    putStrLn $ n ++ "=" ++ name b
