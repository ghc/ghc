module Inst82_1 where
import Data82

instance Read FooData where
   readsPrec _ _ = [(FooData,"")]
