{-# LANGUAGE ImplicitParams #-}
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

funcToReify :: (?z :: Int) => Int
funcToReify = ?z

$( [d|
        f :: (?x :: Int) => Int
        f = let ?y = 2 in ?x + ?y |] )

main = do
    putStrLn $(lift . pprint =<< reify 'funcToReify)
    print (let ?x = 3 in f)
    print $( [| let ?x = 1 in ?x |] )
    print $(letE [implicitParamBindD "y" (lift (2 :: Int))]
                 (implicitParamVarE "y") )
    putStrLn $( lift . pprint =<< [d|
        f :: (?x :: Int) => Int
        f = let ?y = 2 in ?x + ?y |] )
