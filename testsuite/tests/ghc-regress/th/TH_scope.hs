
module TH_scope where

import TH_scope_helper

x :: ()
x = ()
    where hold = $( wibble [d| hold :: ()
                               hold = () |] )
