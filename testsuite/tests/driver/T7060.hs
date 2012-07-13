main :: IO ()
main = print (3 + 4 :: Int)

{-# RULES "rule"  forall xs . map id xs = xs #-}
