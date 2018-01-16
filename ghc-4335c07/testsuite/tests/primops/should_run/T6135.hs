-- Tests created by Jan Stolarek <jan.stolarek@p.lodz.pl>

{-# LANGUAGE BangPatterns, MagicHash #-}
module Main where

import GHC.Exts

main :: IO ()
main = do
  -- PrimOps for comparing Char#
  putStrLn "=== testing gtChar# ==="
  test "gtChar#" (a#    `gtChar#` a#   ) -- False
  test "gtChar#" (b#    `gtChar#` a#   ) -- True
  test "gtChar#" (a#    `gtChar#` b#   ) -- False
  test "gtChar#" (minC# `gtChar#` minC#) -- False
  test "gtChar#" (maxC# `gtChar#` maxC#) -- False
  test "gtChar#" (minC# `gtChar#` maxC#) -- False
  test "gtChar#" (maxC# `gtChar#` minC#) -- True

  putStrLn "=== testing geChar# ==="
  test "geChar#" (a#    `geChar#` a#   ) -- True
  test "geChar#" (b#    `geChar#` a#   ) -- True
  test "geChar#" (a#    `geChar#` b#   ) -- False
  test "geChar#" (minC# `geChar#` minC#) -- True
  test "geChar#" (maxC# `geChar#` maxC#) -- True
  test "geChar#" (minC# `geChar#` maxC#) -- False
  test "geChar#" (maxC# `geChar#` minC#) -- True

  putStrLn "=== testing ltChar# ==="
  test "ltChar#" (a#    `ltChar#` a#   ) -- False
  test "ltChar#" (b#    `ltChar#` a#   ) -- False
  test "ltChar#" (a#    `ltChar#` b#   ) -- True
  test "ltChar#" (minC# `ltChar#` minC#) -- False
  test "ltChar#" (maxC# `ltChar#` maxC#) -- False
  test "ltChar#" (minC# `ltChar#` maxC#) -- True
  test "ltChar#" (maxC# `ltChar#` minC#) -- False

  putStrLn "=== testing leChar# ==="
  test "leChar#" (a#    `leChar#` a#   ) -- True
  test "leChar#" (b#    `leChar#` a#   ) -- False
  test "leChar#" (a#    `leChar#` b#   ) -- True
  test "leChar#" (minC# `leChar#` minC#) -- True
  test "leChar#" (maxC# `leChar#` maxC#) -- True
  test "leChar#" (minC# `leChar#` maxC#) -- True
  test "leChar#" (maxC# `leChar#` minC#) -- False

  putStrLn "=== testing eqChar# ==="
  test "eqChar#" (a#    `eqChar#` a#   ) -- True
  test "eqChar#" (b#    `eqChar#` a#   ) -- False
  test "eqChar#" (a#    `eqChar#` b#   ) -- False
  test "eqChar#" (minC# `eqChar#` minC#) -- True
  test "eqChar#" (maxC# `eqChar#` maxC#) -- True
  test "eqChar#" (minC# `eqChar#` maxC#) -- False
  test "eqChar#" (maxC# `eqChar#` minC#) -- False

  putStrLn "=== testing neChar# ==="
  test "neChar#" (a#    `neChar#` a#   ) -- False
  test "neChar#" (b#    `neChar#` a#   ) -- True
  test "neChar#" (a#    `neChar#` b#   ) -- True
  test "neChar#" (minC# `neChar#` minC#) -- False
  test "neChar#" (maxC# `neChar#` maxC#) -- False
  test "neChar#" (minC# `neChar#` maxC#) -- True
  test "neChar#" (maxC# `neChar#` minC#) -- True

  -- PrimOps for comparing Int#
  putStrLn "=== testing ># ==="
  test ">#" (0#          ># 0#         ) -- False
  test ">#" (1#          ># 0#         ) -- True
  test ">#" (0#          ># 1#         ) -- False
  test ">#" (minI#       ># minI#      ) -- False
  test ">#" (maxI#       ># maxI#      ) -- False
  test ">#" (minI# +# 1# ># minI#      ) -- True
  test ">#" (minI#       ># minI# -# 1#) -- False (overflow)
  test ">#" (maxI# +# 1# ># maxI#      ) -- False (overflow)
  test ">#" (maxI#       ># maxI# -# 1#) -- True

  putStrLn "=== testing <# ==="
  test "<#" (0#          <# 0#         ) -- False
  test "<#" (1#          <# 0#         ) -- False
  test "<#" (0#          <# 1#         ) -- True
  test "<#" (minI#       <# minI#      ) -- False
  test "<#" (maxI#       <# maxI#      ) -- False
  test "<#" (minI#       <# minI# +# 1#) -- True
  test "<#" (minI# -# 1# <# minI#      ) -- False (overflow)
  test "<#" (maxI#       <# maxI# +# 1#) -- False (overflow)
  test "<#" (maxI# -# 1# <# maxI#      ) -- True

  putStrLn "=== testing >=# ==="
  test ">=#" (0#          >=# 0#         ) -- True
  test ">=#" (1#          >=# 0#         ) -- True
  test ">=#" (0#          >=# 1#         ) -- False
  test ">=#" (minI#       >=# minI#      ) -- True
  test ">=#" (maxI#       >=# maxI#      ) -- True
  test ">=#" (minI# +# 1# >=# minI#      ) -- True
  test ">=#" (minI#       >=# minI# -# 1#) -- False (overflow)
  test ">=#" (maxI# +# 1# >=# maxI#      ) -- False (overflow)
  test ">=#" (maxI#       >=# maxI# -# 1#) -- True

  putStrLn "=== testing <=# ==="
  test "<=#" (0#          <=# 0#         ) -- True
  test "<=#" (1#          <=# 0#         ) -- False
  test "<=#" (0#          <=# 1#         ) -- True
  test "<=#" (minI#       <=# minI#      ) -- True
  test "<=#" (maxI#       <=# maxI#      ) -- True
  test "<=#" (minI#       <=# minI# +# 1#) -- True
  test "<=#" (minI# -# 1# <=# minI#      ) -- False (overflow)
  test "<=#" (maxI#       <=# maxI# +# 1#) -- False (overflow)
  test "<=#" (maxI# -# 1# <=# maxI#      ) -- True

  putStrLn "=== testing ==# ==="
  test "==#" (0#          ==# 0#         ) -- True
  test "==#" (1#          ==# 0#         ) -- False
  test "==#" (0#          ==# 1#         ) -- False
  test "==#" (maxI#       ==# maxI#      ) -- True
  test "==#" (maxI# -# 1# ==# maxI#      ) -- False
  test "==#" (minI#       ==# minI#      ) -- True
  test "==#" (minI#       ==# minI# +# 1#) -- False
  test "==#" (minI#       ==# maxI# +# 1#) -- True (overflow)
  test "==#" (maxI# +# 1# ==# minI#      ) -- True (overflow)
  test "==#" (maxI#       ==# minI# -# 1#) -- True (overflow)
  test "==#" (minI# -# 1# ==# maxI#      ) -- True (overflow)

  putStrLn "=== testing /=# ==="
  test "/=#" (0#          /=# 0#         ) -- False
  test "/=#" (1#          /=# 0#         ) -- True
  test "/=#" (0#          /=# 1#         ) -- True
  test "/=#" (maxI#       /=# maxI#      ) -- False
  test "/=#" (maxI# -# 1# /=# maxI#      ) -- True
  test "/=#" (minI#       /=# minI#      ) -- False
  test "/=#" (minI#       /=# minI# +# 1#) -- True
  test "/=#" (minI#       /=# maxI# +# 1#) -- False (overflow)
  test "/=#" (maxI# +# 1# /=# minI#      ) -- False (overflow)
  test "/=#" (maxI#       /=# minI# -# 1#) -- False (overflow)
  test "/=#" (minI# -# 1# /=# maxI#      ) -- False (overflow)

  -- PrimOps for comparing Word#
  putStrLn "=== testing gtWord# ==="
  test "gtWord#" (zeroW#                    `gtWord#` zeroW#                    ) -- False
  test "gtWord#" (oneW#                     `gtWord#` zeroW#                    ) -- True
  test "gtWord#" (zeroW#                    `gtWord#` oneW#                     ) -- False
  test "gtWord#" (minW#                     `gtWord#` minW#                     ) -- False
  test "gtWord#" (maxW#                     `gtWord#` maxW#                     ) -- False
  test "gtWord#" ((minW# `plusWord#` oneW#) `gtWord#` minW#                     ) -- True
  test "gtWord#" (minW#                     `gtWord#` (minW# `minusWord#` oneW#)) -- False (overflow)
  test "gtWord#" ((maxW# `plusWord#` oneW#) `gtWord#` maxW#                     ) -- False (overflow)
  test "gtWord#" (maxW#                     `gtWord#` (maxW# `minusWord#` oneW#)) -- True

  putStrLn "=== testing ltWord# ==="
  test "ltWord#" (zeroW#                     `ltWord#` zeroW#                   ) -- False
  test "ltWord#" (oneW#                      `ltWord#` zeroW#                   ) -- False
  test "ltWord#" (zeroW#                     `ltWord#` oneW#                    ) -- True
  test "ltWord#" (minW#                      `ltWord#` minW#                    ) -- False
  test "ltWord#" (maxW#                      `ltWord#` maxW#                    ) -- False
  test "ltWord#" (minW#                      `ltWord#` (minW# `plusWord#` oneW#)) -- True
  test "ltWord#" ((minW# `minusWord#` oneW#) `ltWord#` minW#                    ) -- False (overflow)
  test "ltWord#" (maxW#                      `ltWord#` (maxW# `plusWord#` oneW#)) -- False (overflow)
  test "ltWord#" ((maxW# `minusWord#` oneW#) `ltWord#` maxW#                    ) -- True

  putStrLn "=== testing geWord# ==="
  test "geWord#" (zeroW#                    `geWord#` zeroW#                    ) -- True
  test "geWord#" (oneW#                     `geWord#` zeroW#                    ) -- True
  test "geWord#" (zeroW#                    `geWord#` oneW#                     ) -- False
  test "geWord#" (minW#                     `geWord#` minW#                     ) -- True
  test "geWord#" (maxW#                     `geWord#` maxW#                     ) -- True
  test "geWord#" ((minW# `plusWord#` oneW#) `geWord#` minW#                     ) -- True
  test "geWord#" (minW#                     `geWord#` (minW# `minusWord#` oneW#)) -- False (overflow)
  test "geWord#" ((maxW# `plusWord#` oneW#) `geWord#` maxW#                     ) -- False (overflow)
  test "geWord#" (maxW#                     `geWord#` (maxW# `minusWord#` oneW#)) -- True

  putStrLn "=== testing leWord# ==="
  test "leWord#" (zeroW#                     `leWord#` zeroW#                   ) -- True
  test "leWord#" (oneW#                      `leWord#` zeroW#                   ) -- False
  test "leWord#" (zeroW#                     `leWord#` oneW#                    ) -- True
  test "leWord#" (minW#                      `leWord#` minW#                    ) -- True
  test "leWord#" (maxW#                      `leWord#` maxW#                    ) -- True
  test "leWord#" (minW#                      `leWord#` (minW# `plusWord#` oneW#)) -- True
  test "leWord#" ((minW# `minusWord#` oneW#) `leWord#` minW#                    ) -- False (overflow)
  test "leWord#" (maxW#                      `leWord#` (maxW# `plusWord#` oneW#)) -- False (overflow)
  test "leWord#" ((maxW# `minusWord#` oneW#) `leWord#` maxW#                    ) -- True

  putStrLn "=== testing eqWord# ==="
  test "eqWord#" (zeroW#                     `eqWord#` zeroW#                    ) -- True
  test "eqWord#" (oneW#                      `eqWord#` zeroW#                    ) -- False
  test "eqWord#" (zeroW#                     `eqWord#` oneW#                     ) -- False
  test "eqWord#" (maxW#                      `eqWord#` maxW#                     ) -- True
  test "eqWord#" ((maxW# `minusWord#` oneW#) `eqWord#` maxW#                     ) -- False
  test "eqWord#" (minW#                      `eqWord#` minW#                     ) -- True
  test "eqWord#" (minW#                      `eqWord#` (minW# `plusWord#` oneW#) ) -- False
  test "eqWord#" (minW#                      `eqWord#` (maxW# `plusWord#` oneW#) ) -- True (overflow)
  test "eqWord#" ((maxW# `plusWord#` oneW#)  `eqWord#` minW#                     ) -- True (overflow)
  test "eqWord#" (maxW#                      `eqWord#` (minW# `minusWord#` oneW#)) -- True (overflow)
  test "eqWord#" ((minW# `minusWord#` oneW#) `eqWord#` maxW#                     ) -- True (overflow)

  putStrLn "=== testing neWord# ==="
  test "neWord#" (zeroW#                     `neWord#` zeroW#                    ) -- False
  test "neWord#" (oneW#                      `neWord#` zeroW#                    ) -- True
  test "neWord#" (zeroW#                     `neWord#` oneW#                     ) -- True
  test "neWord#" (maxW#                      `neWord#` maxW#                     ) -- False
  test "neWord#" ((maxW# `minusWord#` oneW#) `neWord#` maxW#                     ) -- True
  test "neWord#" (minW#                      `neWord#` minW#                     ) -- False
  test "neWord#" (minW#                      `neWord#` (minW# `plusWord#` oneW#) ) -- True
  test "neWord#" (minW#                      `neWord#` (maxW# `plusWord#` oneW#) ) -- False (overflow)
  test "neWord#" ((maxW# `plusWord#` oneW#)  `neWord#` minW#                     ) -- False (overflow)
  test "neWord#" (maxW#                      `neWord#` (minW# `minusWord#` oneW#)) -- False (overflow)
  test "neWord#" ((minW# `minusWord#` oneW#) `neWord#` maxW#                     ) -- False (overflow)

  -- PrimOps for comparing Double#
  putStrLn "=== testing >## ==="
  test ">##" (0.0##  >## 0.0## ) -- False
  test ">##" (1.0##  >## 0.0## ) -- True
  test ">##" (0.0##  >## 1.0## ) -- False
  test ">##" (0.0##  >## nan## ) -- False
  test ">##" (nan##  >## 0.0## ) -- False
  test ">##" (nan##  >## nan## ) -- False
  test ">##" (infp## >## infp##) -- False
  test ">##" (infn## >## infn##) -- False
  test ">##" (infp## >## infn##) -- True
  test ">##" (infn## >## infp##) -- False
  test ">##" (infp## >## nan## ) -- False
  test ">##" (infn## >## nan## ) -- False
  test ">##" (nan##  >## infp##) -- False
  test ">##" (nan##  >## infn##) -- False

  putStrLn "=== testing <## ==="
  test "<##" (0.0##  <## 0.0## ) -- False
  test "<##" (1.0##  <## 0.0## ) -- False
  test "<##" (0.0##  <## 1.0## ) -- True
  test "<##" (0.0##  <## nan## ) -- False
  test "<##" (nan##  <## 0.0## ) -- False
  test "<##" (nan##  <## nan## ) -- False
  test "<##" (infp## <## infp##) -- False
  test "<##" (infn## <## infn##) -- False
  test "<##" (infp## <## infn##) -- False
  test "<##" (infn## <## infp##) -- True
  test "<##" (infp## <## nan## ) -- False
  test "<##" (infn## <## nan## ) -- False
  test "<##" (nan##  <## infp##) -- False
  test "<##" (nan##  <## infn##) -- False

  putStrLn "=== testing >=## ==="
  test ">=##" (0.0##  >=## 0.0## ) -- True
  test ">=##" (1.0##  >=## 0.0## ) -- True
  test ">=##" (0.0##  >=## 1.0## ) -- False
  test ">=##" (0.0##  >=## nan## ) -- False
  test ">=##" (nan##  >=## 0.0## ) -- False
  test ">=##" (nan##  >=## nan## ) -- False
  test ">=##" (infp## >=## infp##) -- True
  test ">=##" (infn## >=## infn##) -- True
  test ">=##" (infp## >=## infn##) -- True
  test ">=##" (infn## >=## infp##) -- False
  test ">=##" (infp## >=## nan## ) -- False
  test ">=##" (infn## >=## nan## ) -- False
  test ">=##" (nan##  >=## infp##) -- False
  test ">=##" (nan##  >=## infn##) -- False

  putStrLn "=== testing <=## ==="
  test "<=##" (0.0##  <=## 0.0## ) -- True
  test "<=##" (1.0##  <=## 0.0## ) -- False
  test "<=##" (0.0##  <=## 1.0## ) -- True
  test "<=##" (0.0##  <=## nan## ) -- False
  test "<=##" (nan##  <=## 0.0## ) -- False
  test "<=##" (nan##  <=## nan## ) -- False
  test "<=##" (infp## <=## infp##) -- True
  test "<=##" (infn## <=## infn##) -- True
  test "<=##" (infp## <=## infn##) -- False
  test "<=##" (infn## <=## infp##) -- True
  test "<=##" (infp## <=## nan## ) -- False
  test "<=##" (infn## <=## nan## ) -- False
  test "<=##" (nan##  <=## infp##) -- False
  test "<=##" (nan##  <=## infn##) -- False

  putStrLn "=== testing ==## ==="
  test "==##" (0.0##  ==## 0.0## ) -- True
  test "==##" (1.0##  ==## 0.0## ) -- False
  test "==##" (0.0##  ==## 1.0## ) -- False
  test "==##" (0.0##  ==## nan## ) -- False
  test "==##" (nan##  ==## 0.0## ) -- False
  test "==##" (nan##  ==## nan## ) -- False
  test "==##" (infp## ==## infp##) -- True
  test "==##" (infn## ==## infn##) -- True
  test "==##" (infp## ==## infn##) -- False
  test "==##" (infn## ==## infp##) -- False
  test "==##" (infp## ==## nan## ) -- False
  test "==##" (infn## ==## nan## ) -- False
  test "==##" (nan##  ==## infp##) -- False
  test "==##" (nan##  ==## infn##) -- False

  putStrLn "=== testing /=## ==="
  test "/=##" (0.0##  /=## 0.0## ) -- False
  test "/=##" (1.0##  /=## 0.0## ) -- True
  test "/=##" (0.0##  /=## 1.0## ) -- True
  test "/=##" (0.0##  /=## nan## ) -- True
  test "/=##" (nan##  /=## 0.0## ) -- True
  test "/=##" (nan##  /=## nan## ) -- True
  test "/=##" (infp## /=## infp##) -- False
  test "/=##" (infn## /=## infn##) -- False
  test "/=##" (infp## /=## infn##) -- True
  test "/=##" (infn## /=## infp##) -- True
  test "/=##" (infp## /=## nan## ) -- True
  test "/=##" (infn## /=## nan## ) -- True
  test "/=##" (nan##  /=## infp##) -- True
  test "/=##" (nan##  /=## infn##) -- True

  -- PrimOps for comparing Float#
  putStrLn "=== testing gtFloat# ==="
  test "gtFloat#" (zeroF# `gtFloat#` zeroF#) -- False
  test "gtFloat#" (oneF#  `gtFloat#` zeroF#) -- True
  test "gtFloat#" (zeroF# `gtFloat#` oneF# ) -- False
  test "gtFloat#" (zeroF# `gtFloat#` nanF# ) -- False
  test "gtFloat#" (nanF#  `gtFloat#` zeroF#) -- False
  test "gtFloat#" (nanF#  `gtFloat#` nanF# ) -- False
  test "gtFloat#" (infpF# `gtFloat#` infpF#) -- False
  test "gtFloat#" (infnF# `gtFloat#` infnF#) -- False
  test "gtFloat#" (infpF# `gtFloat#` infnF#) -- True
  test "gtFloat#" (infnF# `gtFloat#` infpF#) -- False
  test "gtFloat#" (infpF# `gtFloat#` nanF# ) -- False
  test "gtFloat#" (infnF# `gtFloat#` nanF# ) -- False
  test "gtFloat#" (nanF#  `gtFloat#` infpF#) -- False
  test "gtFloat#" (nanF#  `gtFloat#` infnF#) -- False

  putStrLn "=== testing ltFloat# ==="
  test "ltFloat#" (zeroF# `ltFloat#` zeroF#) -- False
  test "ltFloat#" (oneF#  `ltFloat#` zeroF#) -- False
  test "ltFloat#" (zeroF# `ltFloat#` oneF# ) -- True
  test "ltFloat#" (zeroF# `ltFloat#` nanF# ) -- False
  test "ltFloat#" (nanF#  `ltFloat#` zeroF#) -- False
  test "ltFloat#" (nanF#  `ltFloat#` nanF# ) -- False
  test "ltFloat#" (infpF# `ltFloat#` infpF#) -- False
  test "ltFloat#" (infnF# `ltFloat#` infnF#) -- False
  test "ltFloat#" (infpF# `ltFloat#` infnF#) -- False
  test "ltFloat#" (infnF# `ltFloat#` infpF#) -- True
  test "ltFloat#" (infpF# `ltFloat#` nanF# ) -- False
  test "ltFloat#" (infnF# `ltFloat#` nanF# ) -- False
  test "ltFloat#" (nanF#  `ltFloat#` infpF#) -- False
  test "ltFloat#" (nanF#  `ltFloat#` infnF#) -- False

  putStrLn "=== testing geFloat# ==="
  test "geFloat#" (zeroF# `geFloat#` zeroF#) -- True
  test "geFloat#" (oneF#  `geFloat#` zeroF#) -- True
  test "geFloat#" (zeroF# `geFloat#` oneF# ) -- False
  test "geFloat#" (zeroF# `geFloat#` nanF# ) -- False
  test "geFloat#" (nanF#  `geFloat#` zeroF#) -- False
  test "geFloat#" (nanF#  `geFloat#` nanF# ) -- False
  test "geFloat#" (infpF# `geFloat#` infpF#) -- True
  test "geFloat#" (infnF# `geFloat#` infnF#) -- True
  test "geFloat#" (infpF# `geFloat#` infnF#) -- True
  test "geFloat#" (infnF# `geFloat#` infpF#) -- False
  test "geFloat#" (infpF# `geFloat#` nanF# ) -- False
  test "geFloat#" (infnF# `geFloat#` nanF# ) -- False
  test "geFloat#" (nanF#  `geFloat#` infpF#) -- False
  test "geFloat#" (nanF#  `geFloat#` infnF#) -- False

  putStrLn "=== testing leFloat# ==="
  test "leFloat#" (zeroF# `leFloat#` zeroF#) -- True
  test "leFloat#" (oneF#  `leFloat#` zeroF#) -- False
  test "leFloat#" (zeroF# `leFloat#` oneF# ) -- True
  test "leFloat#" (zeroF# `leFloat#` nanF# ) -- False
  test "leFloat#" (nanF#  `leFloat#` zeroF#) -- False
  test "leFloat#" (nanF#  `leFloat#` nanF# ) -- False
  test "leFloat#" (infpF# `leFloat#` infpF#) -- True
  test "leFloat#" (infnF# `leFloat#` infnF#) -- True
  test "leFloat#" (infpF# `leFloat#` infnF#) -- False
  test "leFloat#" (infnF# `leFloat#` infpF#) -- True
  test "leFloat#" (infpF# `leFloat#` nanF# ) -- False
  test "leFloat#" (infnF# `leFloat#` nanF# ) -- False
  test "leFloat#" (nanF#  `leFloat#` infpF#) -- False
  test "leFloat#" (nanF#  `leFloat#` infnF#) -- False

  putStrLn "=== testing eqFloat# ==="
  test "eqFloat#" (zeroF# `eqFloat#` zeroF#) -- True
  test "eqFloat#" (oneF#  `eqFloat#` zeroF#) -- False
  test "eqFloat#" (zeroF# `eqFloat#` oneF# ) -- False
  test "eqFloat#" (zeroF# `eqFloat#` nanF# ) -- False
  test "eqFloat#" (nanF#  `eqFloat#` zeroF#) -- False
  test "eqFloat#" (nanF#  `eqFloat#` nanF# ) -- False
  test "eqFloat#" (infpF# `eqFloat#` infpF#) -- True
  test "eqFloat#" (infnF# `eqFloat#` infnF#) -- True
  test "eqFloat#" (infpF# `eqFloat#` infnF#) -- False
  test "eqFloat#" (infnF# `eqFloat#` infpF#) -- False
  test "eqFloat#" (infpF# `eqFloat#` nanF# ) -- False
  test "eqFloat#" (infnF# `eqFloat#` nanF# ) -- False
  test "eqFloat#" (nanF#  `eqFloat#` infpF#) -- False
  test "eqFloat#" (nanF#  `eqFloat#` infnF#) -- False

  putStrLn "=== testing neFloat# ==="
  test "neFloat#" (zeroF# `neFloat#` zeroF#) -- False
  test "neFloat#" (oneF#  `neFloat#` zeroF#) -- True
  test "neFloat#" (zeroF# `neFloat#` oneF# ) -- True
  test "neFloat#" (zeroF# `neFloat#` nanF# ) -- True
  test "neFloat#" (nanF#  `neFloat#` zeroF#) -- True
  test "neFloat#" (nanF#  `neFloat#` nanF# ) -- True
  test "neFloat#" (infpF# `neFloat#` infpF#) -- False
  test "neFloat#" (infnF# `neFloat#` infnF#) -- False
  test "neFloat#" (infpF# `neFloat#` infnF#) -- True
  test "neFloat#" (infnF# `neFloat#` infpF#) -- True
  test "neFloat#" (infpF# `neFloat#` nanF# ) -- True
  test "neFloat#" (infnF# `neFloat#` nanF# ) -- True
  test "neFloat#" (nanF#  `neFloat#` infpF#) -- True
  test "neFloat#" (nanF#  `neFloat#` infnF#) -- True
    where
      -- every integer is compared to 1 representing True. This
      -- results in printing "True" when the primop should return True
      -- and printing "False" when it should return False
      test :: String -> Int# -> IO ()
      test str x = putStrLn $ str ++ " " ++ (show (I# x == 1))
      a            = 'a'
      b            = 'b'
      !(C# a#)     = a
      !(C# b#)     = b
      zeroW        = 0 :: Word
      oneW         = 1 :: Word
      !(W# zeroW#) =  zeroW
      !(W# oneW#)  =  oneW
      nan          =  0.0 / 0.0 :: Double
      infp         =  1.0 / 0.0 :: Double
      infn         = -1.0 / 0.0 :: Double
      !(D# nan##)  =  0.0 / 0.0
      !(D# infp##) =  1.0 / 0.0
      !(D# infn##) = -1.0 / 0.0
      zeroF        = 0.0 :: Float
      oneF         = 1.0 :: Float
      nanF         = 0.0 / 0.0 :: Float
      infpF        = 1.0 / 0.0 :: Float
      infnF        = -1.0 / 0.0 :: Float
      !(F# zeroF#) =  0.0
      !(F# oneF#)  =  1.0
      !(F# nanF#)  =  0.0 / 0.0
      !(F# infpF#) =  1.0 / 0.0
      !(F# infnF#) = -1.0 / 0.0
      minC         = minBound :: Char
      maxC         = maxBound :: Char
      !(C# minC#)  = minC
      !(C# maxC#)  = maxC
      minI         = minBound :: Int
      maxI         = maxBound :: Int
      !(I# minI#)  = minI
      !(I# maxI#)  = maxI
      minW         = minBound :: Word
      maxW         = maxBound :: Word
      !(W# minW#)  = minW
      !(W# maxW#)  = maxW
