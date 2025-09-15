
{-# LANGUAGE RecursiveDo, DoRec #-}
{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}

module M where

-- do, mdo and rec should all open layouts

f :: IO ()
f = do print 'a'
       print 'b'

g :: IO ()
g = mdo print 'a'
        print 'b'

h :: IO ()
h = do print 'a'
       rec print 'b'
           print 'c'
       print 'd'

