{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Main where

data JSONState = JSONState [()] () () deriving Show

weta_s6yD :: Either a (b, c) -> (# (Either a b, JSONState) #)
weta_s6yD ww_s6ys = case ww_s6ys of
    Left l -> (# (Left l, JSONState [] () ()) #)
    Right (x, _) -> (# (Right x, JSONState [] () ()) #)

eta_B1 :: (Either a (b, c), t) -> Either a1 (Either a b, JSONState)
eta_B1 (ww_s6ys, _) = case weta_s6yD ww_s6ys of
    (# ww_s6zb #) -> Right ww_s6zb

wks_s6yS :: Either a b -> (# (Either a b, JSONState) #)
wks_s6yS ww_s6yH =
    case case ww_s6yH of
        Left l_a4ay -> eta_B1 (Left l_a4ay, ())
        Right r_a4aB -> eta_B1 (Right (r_a4aB, ()), ())
    of
        Right ww_s6ze -> (# ww_s6ze #)

ks_a49u :: (Either a b, t) -> Either a1 (Either a b, JSONState)
ks_a49u (ww_s6yH, _) = case wks_s6yS ww_s6yH of
    (# ww_s6ze #) -> Right ww_s6ze

wks_s6z7 :: Either a b -> (# (Either a b, JSONState) #)
wks_s6z7 ww_s6yW = case (
    case ww_s6yW of
        Left _ -> ks_a49u (ww_s6yW, JSONState [()] () ())
        Right _ -> ks_a49u (ww_s6yW, JSONState [] () ())
    ) of
        Right ww_s6zh -> (# ww_s6zh #)

ks_X3Sb :: Either () Int -> Either String (Either () Int, JSONState)
ks_X3Sb ww_s6yW = case wks_s6z7 ww_s6yW of
    (# ww_s6zh #) -> Right ww_s6zh

main :: IO ()
main = print $ ks_X3Sb (Left ())
