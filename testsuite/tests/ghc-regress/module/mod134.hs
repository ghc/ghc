-- Test that 'hiding' only hides the unqualified name

module Main where
import Prelude hiding (head)

main  = do print (Prelude.head [0..])
           head

head = print "head"
