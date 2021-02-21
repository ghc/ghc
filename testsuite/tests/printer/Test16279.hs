{-# LANGUAGE AlternativeLayoutRule #-}
{-# LANGUAGE LambdaCase            #-}
-- duplicate of T13087.hs

isOne :: Int -> Bool
isOne = \case 1 -> True
              _ -> False

main = return ()

