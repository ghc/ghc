-- Test that 'hiding' hides the qualified name too

module Main where
import Prelude hiding (head)

main  = do print (Prelude.head [0..])
           head

head = print "head"
