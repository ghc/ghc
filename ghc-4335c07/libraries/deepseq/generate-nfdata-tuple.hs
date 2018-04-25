import Data.List

genNFData :: Int -> [String]
genNFData n = 
    [ ""
    , "instance (" ++ ctx0 ++ ") =>"
    , "         NFData (" ++ intercalate ", " as0  ++ ") where rnf = rnf2"
    , "instance (" ++ ctx1 ++ ") =>"
    , "         NFData1 ((" ++ replicate (n - 1) ',' ++ ") " ++ intercalate " " as1  ++ ") where liftRnf = liftRnf2 rnf"
    , "instance (" ++ ctx2 ++ ") =>"
    , "         NFData2 ((" ++ replicate (n - 1) ',' ++ ") " ++ intercalate " " as2  ++ ") where"
    , "  liftRnf2 r r' (" ++ intercalate "," xs ++ ") = " ++ implemantation
    ]
  where
    as0 = take (n - 0) $ ('a' :) . show <$> [1..]
    as1 = take (n - 1) $ ('a' :) . show <$> [1..]
    as2 = take (n - 2) $ ('a' :) . show <$> [1..]

    xs = take n $ ('x' : ) . show <$> [1..]

    ctx0 = intercalate ", " $ ("NFData " ++) <$> as0
    ctx1 = intercalate ", " $ ("NFData " ++) <$> as1
    ctx2 = intercalate ", " $ ("NFData " ++) <$> as2

    implemantation
        = intercalate " `seq` "
        $ zipWith (\l r -> l ++ " " ++ r)
            (replicate (n-2) "rnf" ++ ["r", "r'"])
            xs
    

main :: IO ()
main = putStr $ unlines $ concatMap (\x -> genNFData x) [3..9]
