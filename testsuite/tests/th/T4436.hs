{-# OPTIONS -XTemplateHaskell #-} 
module TH( x ) where 
import Language.Haskell.TH

x = $(return (LitE (StringL "hello\ngoodbye\nand then")))
 



