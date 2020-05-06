module T3000S (sshow) where

sshow :: Int -> String
sshow n =
    let nest  :: Int -> String
        nest 0 = " nest: " ++ hidden 0
        nestn = " nest: " ++ show n
    in
        " show: " ++ nest n

hidden :: Int -> String
hidden 1 = " hidden: 1"
hidden n = " hidden: " ++ show n
