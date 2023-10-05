{-# LANGUAGE PatternSynonyms #-}
module Bug where

{-# COMPLETE Id #-}
pattern Id :: ()
pattern Id = ()

bug :: ()
bug | Id <- id () = ()

