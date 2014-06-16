module ShouldCompile where

(<>)          :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
(m1 <> m2) a1	=  case m1 a1 of
                      Nothing -> Nothing
                      Just a2 -> m2 a2
