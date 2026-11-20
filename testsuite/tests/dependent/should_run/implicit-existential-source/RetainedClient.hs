module RetainedClient where

import RetainedDot (forget, observe, retained)

main :: IO ()
main = print (map (observe . forget . retained) [True, False])
