main = print val
val = 3 +! 4 *! 2
  where (+!) = (+)
        (*!) = (*)
        infixl 6 +!
        infixl 7 *!
