{-# OPTIONS_GHC -fno-full-laziness -dppr-debug #-}

module T27261 (foo) where

import T27261_aux (myError)

foo :: [String] -> (() -> Int) -> Int
foo cs =
  \ k -> ( case bar of
             Just str -> let cs2 = case cs of { [] -> cs; _ -> "stack entry" : cs }
                         in myError cs2 str
             Nothing -> \ c -> c () )
         ( \ _ -> k () )

bar :: Maybe String
bar = Nothing
{-# NOINLINE bar #-}
