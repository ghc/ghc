module T22807_ghci where


foo b =
    let x = Just [1..1000]
    in if b
        then Left x
        else Right x
