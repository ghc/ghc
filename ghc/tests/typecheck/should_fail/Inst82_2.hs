module Inst82_2 where
import Data82

instance Read FooData where
   readsPrec _ _ = [(FooData,"")]
