{-# LANGUAGE RecursiveDo    #-}
{-# LANGUAGE ImplicitParams #-}

module ShouldFail where

x = mdo
  let ?x = 5
  ()
