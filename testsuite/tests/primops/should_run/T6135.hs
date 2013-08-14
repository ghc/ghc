-- Tests created by Jan Stolarek <jan.stolarek@p.lodz.pl>

{-# LANGUAGE BangPatterns, MagicHash #-}
module Main where

import GHC.Exts

main :: IO ()
main = do
  -- PrimOps for comparing Char#
  putStrLn "=== testing gtCharI# ==="
  test "gtCharI#" (a#    `gtCharI#` a#   ) -- False
  test "gtCharI#" (b#    `gtCharI#` a#   ) -- True
  test "gtCharI#" (a#    `gtCharI#` b#   ) -- False
  test "gtCharI#" (minC# `gtCharI#` minC#) -- False
  test "gtCharI#" (maxC# `gtCharI#` maxC#) -- False
  test "gtCharI#" (minC# `gtCharI#` maxC#) -- False
  test "gtCharI#" (maxC# `gtCharI#` minC#) -- True

  putStrLn "=== testing geCharI# ==="
  test "geCharI#" (a#    `geCharI#` a#   ) -- True
  test "geCharI#" (b#    `geCharI#` a#   ) -- True
  test "geCharI#" (a#    `geCharI#` b#   ) -- False
  test "geCharI#" (minC# `geCharI#` minC#) -- True
  test "geCharI#" (maxC# `geCharI#` maxC#) -- True
  test "geCharI#" (minC# `geCharI#` maxC#) -- False
  test "geCharI#" (maxC# `geCharI#` minC#) -- True

  putStrLn "=== testing ltCharI# ==="
  test "ltCharI#" (a#    `ltCharI#` a#   ) -- False
  test "ltCharI#" (b#    `ltCharI#` a#   ) -- False
  test "ltCharI#" (a#    `ltCharI#` b#   ) -- True
  test "ltCharI#" (minC# `ltCharI#` minC#) -- False
  test "ltCharI#" (maxC# `ltCharI#` maxC#) -- False
  test "ltCharI#" (minC# `ltCharI#` maxC#) -- True
  test "ltCharI#" (maxC# `ltCharI#` minC#) -- False

  putStrLn "=== testing leCharI# ==="
  test "leCharI#" (a#    `leCharI#` a#   ) -- True
  test "leCharI#" (b#    `leCharI#` a#   ) -- False
  test "leCharI#" (a#    `leCharI#` b#   ) -- True
  test "leCharI#" (minC# `leCharI#` minC#) -- True
  test "leCharI#" (maxC# `leCharI#` maxC#) -- True
  test "leCharI#" (minC# `leCharI#` maxC#) -- True
  test "leCharI#" (maxC# `leCharI#` minC#) -- False

  putStrLn "=== testing eqCharI# ==="
  test "eqCharI#" (a#    `eqCharI#` a#   ) -- True
  test "eqCharI#" (b#    `eqCharI#` a#   ) -- False
  test "eqCharI#" (a#    `eqCharI#` b#   ) -- False
  test "eqCharI#" (minC# `eqCharI#` minC#) -- True
  test "eqCharI#" (maxC# `eqCharI#` maxC#) -- True
  test "eqCharI#" (minC# `eqCharI#` maxC#) -- False
  test "eqCharI#" (maxC# `eqCharI#` minC#) -- False

  putStrLn "=== testing neCharI# ==="
  test "neCharI#" (a#    `neCharI#` a#   ) -- False
  test "neCharI#" (b#    `neCharI#` a#   ) -- True
  test "neCharI#" (a#    `neCharI#` b#   ) -- True
  test "neCharI#" (minC# `neCharI#` minC#) -- False
  test "neCharI#" (maxC# `neCharI#` maxC#) -- False
  test "neCharI#" (minC# `neCharI#` maxC#) -- True
  test "neCharI#" (maxC# `neCharI#` minC#) -- True

  -- PrimOps for comparing Int#
  putStrLn "=== testing >$# ==="
  test ">$#" (0#          >$# 0#         ) -- False
  test ">$#" (1#          >$# 0#         ) -- True
  test ">$#" (0#          >$# 1#         ) -- False
  test ">$#" (minI#       >$# minI#      ) -- False
  test ">$#" (maxI#       >$# maxI#      ) -- False
  test ">$#" (minI# +# 1# >$# minI#      ) -- True
  test ">$#" (minI#       >$# minI# -# 1#) -- False (overflow)
  test ">$#" (maxI# +# 1# >$# maxI#      ) -- False (overflow)
  test ">$#" (maxI#       >$# maxI# -# 1#) -- True

  putStrLn "=== testing <$# ==="
  test "<$#" (0#          <$# 0#         ) -- False
  test "<$#" (1#          <$# 0#         ) -- False
  test "<$#" (0#          <$# 1#         ) -- True
  test "<$#" (minI#       <$# minI#      ) -- False
  test "<$#" (maxI#       <$# maxI#      ) -- False
  test "<$#" (minI#       <$# minI# +# 1#) -- True
  test "<$#" (minI# -# 1# <$# minI#      ) -- False (overflow)
  test "<$#" (maxI#       <$# maxI# +# 1#) -- False (overflow)
  test "<$#" (maxI# -# 1# <$# maxI#      ) -- True

  putStrLn "=== testing >=$# ==="
  test ">=$#" (0#          >=$# 0#         ) -- True
  test ">=$#" (1#          >=$# 0#         ) -- True
  test ">=$#" (0#          >=$# 1#         ) -- False
  test ">=$#" (minI#       >=$# minI#      ) -- True
  test ">=$#" (maxI#       >=$# maxI#      ) -- True
  test ">=$#" (minI# +# 1# >=$# minI#      ) -- True
  test ">=$#" (minI#       >=$# minI# -# 1#) -- False (overflow)
  test ">=$#" (maxI# +# 1# >=$# maxI#      ) -- False (overflow)
  test ">=$#" (maxI#       >=$# maxI# -# 1#) -- True

  putStrLn "=== testing <=$# ==="
  test "<=$#" (0#          <=$# 0#         ) -- True
  test "<=$#" (1#          <=$# 0#         ) -- False
  test "<=$#" (0#          <=$# 1#         ) -- True
  test "<=$#" (minI#       <=$# minI#      ) -- True
  test "<=$#" (maxI#       <=$# maxI#      ) -- True
  test "<=$#" (minI#       <=$# minI# +# 1#) -- True
  test "<=$#" (minI# -# 1# <=$# minI#      ) -- False (overflow)
  test "<=$#" (maxI#       <=$# maxI# +# 1#) -- False (overflow)
  test "<=$#" (maxI# -# 1# <=$# maxI#      ) -- True

  putStrLn "=== testing ==$# ==="
  test "==$#" (0#          ==$# 0#         ) -- True
  test "==$#" (1#          ==$# 0#         ) -- False
  test "==$#" (0#          ==$# 1#         ) -- False
  test "==$#" (maxI#       ==$# maxI#      ) -- True
  test "==$#" (maxI# -# 1# ==$# maxI#      ) -- False
  test "==$#" (minI#       ==$# minI#      ) -- True
  test "==$#" (minI#       ==$# minI# +# 1#) -- False
  test "==$#" (minI#       ==$# maxI# +# 1#) -- True (overflow)
  test "==$#" (maxI# +# 1# ==$# minI#      ) -- True (overflow)
  test "==$#" (maxI#       ==$# minI# -# 1#) -- True (overflow)
  test "==$#" (minI# -# 1# ==$# maxI#      ) -- True (overflow)

  putStrLn "=== testing /=$# ==="
  test "/=$#" (0#          /=$# 0#         ) -- False
  test "/=$#" (1#          /=$# 0#         ) -- True
  test "/=$#" (0#          /=$# 1#         ) -- True
  test "/=$#" (maxI#       /=$# maxI#      ) -- False
  test "/=$#" (maxI# -# 1# /=$# maxI#      ) -- True
  test "/=$#" (minI#       /=$# minI#      ) -- False
  test "/=$#" (minI#       /=$# minI# +# 1#) -- True
  test "/=$#" (minI#       /=$# maxI# +# 1#) -- False (overflow)
  test "/=$#" (maxI# +# 1# /=$# minI#      ) -- False (overflow)
  test "/=$#" (maxI#       /=$# minI# -# 1#) -- False (overflow)
  test "/=$#" (minI# -# 1# /=$# maxI#      ) -- False (overflow)

  -- PrimOps for comparing Word#
  putStrLn "=== testing gtWordI# ==="
  test "gtWordI#" (zeroW#                    `gtWordI#` zeroW#                    ) -- False
  test "gtWordI#" (oneW#                     `gtWordI#` zeroW#                    ) -- True
  test "gtWordI#" (zeroW#                    `gtWordI#` oneW#                     ) -- False
  test "gtWordI#" (minW#                     `gtWordI#` minW#                     ) -- False
  test "gtWordI#" (maxW#                     `gtWordI#` maxW#                     ) -- False
  test "gtWordI#" ((minW# `plusWord#` oneW#) `gtWordI#` minW#                     ) -- True
  test "gtWordI#" (minW#                     `gtWordI#` (minW# `minusWord#` oneW#)) -- False (overflow)
  test "gtWordI#" ((maxW# `plusWord#` oneW#) `gtWordI#` maxW#                     ) -- False (overflow)
  test "gtWordI#" (maxW#                     `gtWordI#` (maxW# `minusWord#` oneW#)) -- True

  putStrLn "=== testing ltWordI# ==="
  test "ltWordI#" (zeroW#                     `ltWordI#` zeroW#                   ) -- False
  test "ltWordI#" (oneW#                      `ltWordI#` zeroW#                   ) -- False
  test "ltWordI#" (zeroW#                     `ltWordI#` oneW#                    ) -- True
  test "ltWordI#" (minW#                      `ltWordI#` minW#                    ) -- False
  test "ltWordI#" (maxW#                      `ltWordI#` maxW#                    ) -- False
  test "ltWordI#" (minW#                      `ltWordI#` (minW# `plusWord#` oneW#)) -- True
  test "ltWordI#" ((minW# `minusWord#` oneW#) `ltWordI#` minW#                    ) -- False (overflow)
  test "ltWordI#" (maxW#                      `ltWordI#` (maxW# `plusWord#` oneW#)) -- False (overflow)
  test "ltWordI#" ((maxW# `minusWord#` oneW#) `ltWordI#` maxW#                    ) -- True

  putStrLn "=== testing geWordI# ==="
  test "geWordI#" (zeroW#                    `geWordI#` zeroW#                    ) -- True
  test "geWordI#" (oneW#                     `geWordI#` zeroW#                    ) -- True
  test "geWordI#" (zeroW#                    `geWordI#` oneW#                     ) -- False
  test "geWordI#" (minW#                     `geWordI#` minW#                     ) -- True
  test "geWordI#" (maxW#                     `geWordI#` maxW#                     ) -- True
  test "geWordI#" ((minW# `plusWord#` oneW#) `geWordI#` minW#                     ) -- True
  test "geWordI#" (minW#                     `geWordI#` (minW# `minusWord#` oneW#)) -- False (overflow)
  test "geWordI#" ((maxW# `plusWord#` oneW#) `geWordI#` maxW#                     ) -- False (overflow)
  test "geWordI#" (maxW#                     `geWordI#` (maxW# `minusWord#` oneW#)) -- True

  putStrLn "=== testing leWordI# ==="
  test "leWordI#" (zeroW#                     `leWordI#` zeroW#                   ) -- True
  test "leWordI#" (oneW#                      `leWordI#` zeroW#                   ) -- False
  test "leWordI#" (zeroW#                     `leWordI#` oneW#                    ) -- True
  test "leWordI#" (minW#                      `leWordI#` minW#                    ) -- True
  test "leWordI#" (maxW#                      `leWordI#` maxW#                    ) -- True
  test "leWordI#" (minW#                      `leWordI#` (minW# `plusWord#` oneW#)) -- True
  test "leWordI#" ((minW# `minusWord#` oneW#) `leWordI#` minW#                    ) -- False (overflow)
  test "leWordI#" (maxW#                      `leWordI#` (maxW# `plusWord#` oneW#)) -- False (overflow)
  test "leWordI#" ((maxW# `minusWord#` oneW#) `leWordI#` maxW#                    ) -- True

  putStrLn "=== testing eqWordI# ==="
  test "eqWordI#" (zeroW#                     `eqWordI#` zeroW#                    ) -- True
  test "eqWordI#" (oneW#                      `eqWordI#` zeroW#                    ) -- False
  test "eqWordI#" (zeroW#                     `eqWordI#` oneW#                     ) -- False
  test "eqWordI#" (maxW#                      `eqWordI#` maxW#                     ) -- True
  test "eqWordI#" ((maxW# `minusWord#` oneW#) `eqWordI#` maxW#                     ) -- False
  test "eqWordI#" (minW#                      `eqWordI#` minW#                     ) -- True
  test "eqWordI#" (minW#                      `eqWordI#` (minW# `plusWord#` oneW#) ) -- False
  test "eqWordI#" (minW#                      `eqWordI#` (maxW# `plusWord#` oneW#) ) -- True (overflow)
  test "eqWordI#" ((maxW# `plusWord#` oneW#)  `eqWordI#` minW#                     ) -- True (overflow)
  test "eqWordI#" (maxW#                      `eqWordI#` (minW# `minusWord#` oneW#)) -- True (overflow)
  test "eqWordI#" ((minW# `minusWord#` oneW#) `eqWordI#` maxW#                     ) -- True (overflow)

  putStrLn "=== testing neWordI# ==="
  test "neWordI#" (zeroW#                     `neWordI#` zeroW#                    ) -- False
  test "neWordI#" (oneW#                      `neWordI#` zeroW#                    ) -- True
  test "neWordI#" (zeroW#                     `neWordI#` oneW#                     ) -- True
  test "neWordI#" (maxW#                      `neWordI#` maxW#                     ) -- False
  test "neWordI#" ((maxW# `minusWord#` oneW#) `neWordI#` maxW#                     ) -- True
  test "neWordI#" (minW#                      `neWordI#` minW#                     ) -- False
  test "neWordI#" (minW#                      `neWordI#` (minW# `plusWord#` oneW#) ) -- True
  test "neWordI#" (minW#                      `neWordI#` (maxW# `plusWord#` oneW#) ) -- False (overflow)
  test "neWordI#" ((maxW# `plusWord#` oneW#)  `neWordI#` minW#                     ) -- False (overflow)
  test "neWordI#" (maxW#                      `neWordI#` (minW# `minusWord#` oneW#)) -- False (overflow)
  test "neWordI#" ((minW# `minusWord#` oneW#) `neWordI#` maxW#                     ) -- False (overflow)

  -- PrimOps for comparing Double#
  putStrLn "=== testing >$## ==="
  test ">$##" (0.0##  >$## 0.0## ) -- False
  test ">$##" (1.0##  >$## 0.0## ) -- True
  test ">$##" (0.0##  >$## 1.0## ) -- False
  test ">$##" (0.0##  >$## nan## ) -- False
  test ">$##" (nan##  >$## 0.0## ) -- False
  test ">$##" (nan##  >$## nan## ) -- False
  test ">$##" (infp## >$## infp##) -- False
  test ">$##" (infn## >$## infn##) -- False
  test ">$##" (infp## >$## infn##) -- True
  test ">$##" (infn## >$## infp##) -- False
  test ">$##" (infp## >$## nan## ) -- False
  test ">$##" (infn## >$## nan## ) -- False
  test ">$##" (nan##  >$## infp##) -- False
  test ">$##" (nan##  >$## infn##) -- False

  putStrLn "=== testing <$## ==="
  test "<$##" (0.0##  <$## 0.0## ) -- False
  test "<$##" (1.0##  <$## 0.0## ) -- False
  test "<$##" (0.0##  <$## 1.0## ) -- True
  test "<$##" (0.0##  <$## nan## ) -- False
  test "<$##" (nan##  <$## 0.0## ) -- False
  test "<$##" (nan##  <$## nan## ) -- False
  test "<$##" (infp## <$## infp##) -- False
  test "<$##" (infn## <$## infn##) -- False
  test "<$##" (infp## <$## infn##) -- False
  test "<$##" (infn## <$## infp##) -- True
  test "<$##" (infp## <$## nan## ) -- False
  test "<$##" (infn## <$## nan## ) -- False
  test "<$##" (nan##  <$## infp##) -- False
  test "<$##" (nan##  <$## infn##) -- False

  putStrLn "=== testing >=$## ==="
  test ">=$##" (0.0##  >=$## 0.0## ) -- True
  test ">=$##" (1.0##  >=$## 0.0## ) -- True
  test ">=$##" (0.0##  >=$## 1.0## ) -- False
  test ">=$##" (0.0##  >=$## nan## ) -- False
  test ">=$##" (nan##  >=$## 0.0## ) -- False
  test ">=$##" (nan##  >=$## nan## ) -- False
  test ">=$##" (infp## >=$## infp##) -- True
  test ">=$##" (infn## >=$## infn##) -- True
  test ">=$##" (infp## >=$## infn##) -- True
  test ">=$##" (infn## >=$## infp##) -- False
  test ">=$##" (infp## >=$## nan## ) -- False
  test ">=$##" (infn## >=$## nan## ) -- False
  test ">=$##" (nan##  >=$## infp##) -- False
  test ">=$##" (nan##  >=$## infn##) -- False

  putStrLn "=== testing <=$## ==="
  test "<=$##" (0.0##  <=$## 0.0## ) -- True
  test "<=$##" (1.0##  <=$## 0.0## ) -- False
  test "<=$##" (0.0##  <=$## 1.0## ) -- True
  test "<=$##" (0.0##  <=$## nan## ) -- False
  test "<=$##" (nan##  <=$## 0.0## ) -- False
  test "<=$##" (nan##  <=$## nan## ) -- False
  test "<=$##" (infp## <=$## infp##) -- True
  test "<=$##" (infn## <=$## infn##) -- True
  test "<=$##" (infp## <=$## infn##) -- False
  test "<=$##" (infn## <=$## infp##) -- True
  test "<=$##" (infp## <=$## nan## ) -- False
  test "<=$##" (infn## <=$## nan## ) -- False
  test "<=$##" (nan##  <=$## infp##) -- False
  test "<=$##" (nan##  <=$## infn##) -- False

  putStrLn "=== testing ==$## ==="
  test "==$##" (0.0##  ==$## 0.0## ) -- True
  test "==$##" (1.0##  ==$## 0.0## ) -- False
  test "==$##" (0.0##  ==$## 1.0## ) -- False
  test "==$##" (0.0##  ==$## nan## ) -- False
  test "==$##" (nan##  ==$## 0.0## ) -- False
  test "==$##" (nan##  ==$## nan## ) -- False
  test "==$##" (infp## ==$## infp##) -- True
  test "==$##" (infn## ==$## infn##) -- True
  test "==$##" (infp## ==$## infn##) -- False
  test "==$##" (infn## ==$## infp##) -- False
  test "==$##" (infp## ==$## nan## ) -- False
  test "==$##" (infn## ==$## nan## ) -- False
  test "==$##" (nan##  ==$## infp##) -- False
  test "==$##" (nan##  ==$## infn##) -- False

  putStrLn "=== testing /=$## ==="
  test "/=$##" (0.0##  /=$## 0.0## ) -- False
  test "/=$##" (1.0##  /=$## 0.0## ) -- True
  test "/=$##" (0.0##  /=$## 1.0## ) -- True
  test "/=$##" (0.0##  /=$## nan## ) -- True
  test "/=$##" (nan##  /=$## 0.0## ) -- True
  test "/=$##" (nan##  /=$## nan## ) -- True
  test "/=$##" (infp## /=$## infp##) -- False
  test "/=$##" (infn## /=$## infn##) -- False
  test "/=$##" (infp## /=$## infn##) -- True
  test "/=$##" (infn## /=$## infp##) -- True
  test "/=$##" (infp## /=$## nan## ) -- True
  test "/=$##" (infn## /=$## nan## ) -- True
  test "/=$##" (nan##  /=$## infp##) -- True
  test "/=$##" (nan##  /=$## infn##) -- True

  -- PrimOps for comparing Float#
  putStrLn "=== testing gtFloatI# ==="
  test "gtFloatI#" (zeroF# `gtFloatI#` zeroF#) -- False
  test "gtFloatI#" (oneF#  `gtFloatI#` zeroF#) -- True
  test "gtFloatI#" (zeroF# `gtFloatI#` oneF# ) -- False
  test "gtFloatI#" (zeroF# `gtFloatI#` nanF# ) -- False
  test "gtFloatI#" (nanF#  `gtFloatI#` zeroF#) -- False
  test "gtFloatI#" (nanF#  `gtFloatI#` nanF# ) -- False
  test "gtFloatI#" (infpF# `gtFloatI#` infpF#) -- False
  test "gtFloatI#" (infnF# `gtFloatI#` infnF#) -- False
  test "gtFloatI#" (infpF# `gtFloatI#` infnF#) -- True
  test "gtFloatI#" (infnF# `gtFloatI#` infpF#) -- False
  test "gtFloatI#" (infpF# `gtFloatI#` nanF# ) -- False
  test "gtFloatI#" (infnF# `gtFloatI#` nanF# ) -- False
  test "gtFloatI#" (nanF#  `gtFloatI#` infpF#) -- False
  test "gtFloatI#" (nanF#  `gtFloatI#` infnF#) -- False

  putStrLn "=== testing ltFloatI# ==="
  test "ltFloatI#" (zeroF# `ltFloatI#` zeroF#) -- False
  test "ltFloatI#" (oneF#  `ltFloatI#` zeroF#) -- False
  test "ltFloatI#" (zeroF# `ltFloatI#` oneF# ) -- True
  test "ltFloatI#" (zeroF# `ltFloatI#` nanF# ) -- False
  test "ltFloatI#" (nanF#  `ltFloatI#` zeroF#) -- False
  test "ltFloatI#" (nanF#  `ltFloatI#` nanF# ) -- False
  test "ltFloatI#" (infpF# `ltFloatI#` infpF#) -- False
  test "ltFloatI#" (infnF# `ltFloatI#` infnF#) -- False
  test "ltFloatI#" (infpF# `ltFloatI#` infnF#) -- False
  test "ltFloatI#" (infnF# `ltFloatI#` infpF#) -- True
  test "ltFloatI#" (infpF# `ltFloatI#` nanF# ) -- False
  test "ltFloatI#" (infnF# `ltFloatI#` nanF# ) -- False
  test "ltFloatI#" (nanF#  `ltFloatI#` infpF#) -- False
  test "ltFloatI#" (nanF#  `ltFloatI#` infnF#) -- False

  putStrLn "=== testing geFloatI# ==="
  test "geFloatI#" (zeroF# `geFloatI#` zeroF#) -- True
  test "geFloatI#" (oneF#  `geFloatI#` zeroF#) -- True
  test "geFloatI#" (zeroF# `geFloatI#` oneF# ) -- False
  test "geFloatI#" (zeroF# `geFloatI#` nanF# ) -- False
  test "geFloatI#" (nanF#  `geFloatI#` zeroF#) -- False
  test "geFloatI#" (nanF#  `geFloatI#` nanF# ) -- False
  test "geFloatI#" (infpF# `geFloatI#` infpF#) -- True
  test "geFloatI#" (infnF# `geFloatI#` infnF#) -- True
  test "geFloatI#" (infpF# `geFloatI#` infnF#) -- True
  test "geFloatI#" (infnF# `geFloatI#` infpF#) -- False
  test "geFloatI#" (infpF# `geFloatI#` nanF# ) -- False
  test "geFloatI#" (infnF# `geFloatI#` nanF# ) -- False
  test "geFloatI#" (nanF#  `geFloatI#` infpF#) -- False
  test "geFloatI#" (nanF#  `geFloatI#` infnF#) -- False

  putStrLn "=== testing leFloatI# ==="
  test "leFloatI#" (zeroF# `leFloatI#` zeroF#) -- True
  test "leFloatI#" (oneF#  `leFloatI#` zeroF#) -- False
  test "leFloatI#" (zeroF# `leFloatI#` oneF# ) -- True
  test "leFloatI#" (zeroF# `leFloatI#` nanF# ) -- False
  test "leFloatI#" (nanF#  `leFloatI#` zeroF#) -- False
  test "leFloatI#" (nanF#  `leFloatI#` nanF# ) -- False
  test "leFloatI#" (infpF# `leFloatI#` infpF#) -- True
  test "leFloatI#" (infnF# `leFloatI#` infnF#) -- True
  test "leFloatI#" (infpF# `leFloatI#` infnF#) -- False
  test "leFloatI#" (infnF# `leFloatI#` infpF#) -- True
  test "leFloatI#" (infpF# `leFloatI#` nanF# ) -- False
  test "leFloatI#" (infnF# `leFloatI#` nanF# ) -- False
  test "leFloatI#" (nanF#  `leFloatI#` infpF#) -- False
  test "leFloatI#" (nanF#  `leFloatI#` infnF#) -- False

  putStrLn "=== testing eqFloatI# ==="
  test "eqFloatI#" (zeroF# `eqFloatI#` zeroF#) -- True
  test "eqFloatI#" (oneF#  `eqFloatI#` zeroF#) -- False
  test "eqFloatI#" (zeroF# `eqFloatI#` oneF# ) -- False
  test "eqFloatI#" (zeroF# `eqFloatI#` nanF# ) -- False
  test "eqFloatI#" (nanF#  `eqFloatI#` zeroF#) -- False
  test "eqFloatI#" (nanF#  `eqFloatI#` nanF# ) -- False
  test "eqFloatI#" (infpF# `eqFloatI#` infpF#) -- True
  test "eqFloatI#" (infnF# `eqFloatI#` infnF#) -- True
  test "eqFloatI#" (infpF# `eqFloatI#` infnF#) -- False
  test "eqFloatI#" (infnF# `eqFloatI#` infpF#) -- False
  test "eqFloatI#" (infpF# `eqFloatI#` nanF# ) -- False
  test "eqFloatI#" (infnF# `eqFloatI#` nanF# ) -- False
  test "eqFloatI#" (nanF#  `eqFloatI#` infpF#) -- False
  test "eqFloatI#" (nanF#  `eqFloatI#` infnF#) -- False

  putStrLn "=== testing neFloatI# ==="
  test "neFloatI#" (zeroF# `neFloatI#` zeroF#) -- False
  test "neFloatI#" (oneF#  `neFloatI#` zeroF#) -- True
  test "neFloatI#" (zeroF# `neFloatI#` oneF# ) -- True
  test "neFloatI#" (zeroF# `neFloatI#` nanF# ) -- True
  test "neFloatI#" (nanF#  `neFloatI#` zeroF#) -- True
  test "neFloatI#" (nanF#  `neFloatI#` nanF# ) -- True
  test "neFloatI#" (infpF# `neFloatI#` infpF#) -- False
  test "neFloatI#" (infnF# `neFloatI#` infnF#) -- False
  test "neFloatI#" (infpF# `neFloatI#` infnF#) -- True
  test "neFloatI#" (infnF# `neFloatI#` infpF#) -- True
  test "neFloatI#" (infpF# `neFloatI#` nanF# ) -- True
  test "neFloatI#" (infnF# `neFloatI#` nanF# ) -- True
  test "neFloatI#" (nanF#  `neFloatI#` infpF#) -- True
  test "neFloatI#" (nanF#  `neFloatI#` infnF#) -- True

  --- Now all the above tests are repeated for primop wrappers
  putStrLn "=== TESTING WRAPPERS ==="
  -- Wrappers for comparing Char
  putStrLn "=== testing Char > ==="
  testw "Char >" ((\(C# a#) (C# b#) -> a# `gtChar#` b#) a    a   ) -- False
  testw "Char >" ((\(C# a#) (C# b#) -> a# `gtChar#` b#) b    a   ) -- True
  testw "Char >" ((\(C# a#) (C# b#) -> a# `gtChar#` b#) a    b   ) -- False
  testw "Char >" ((\(C# a#) (C# b#) -> a# `gtChar#` b#) minC minC) -- False
  testw "Char >" ((\(C# a#) (C# b#) -> a# `gtChar#` b#) maxC maxC) -- False
  testw "Char >" ((\(C# a#) (C# b#) -> a# `gtChar#` b#) minC maxC) -- False
  testw "Char >" ((\(C# a#) (C# b#) -> a# `gtChar#` b#) maxC minC) -- True

  putStrLn "=== testing Char >= ==="
  testw "Char >=" ((\(C# a#) (C# b#) -> a# `geChar#` b#) a    a   ) -- True
  testw "Char >=" ((\(C# a#) (C# b#) -> a# `geChar#` b#) b    a   ) -- True
  testw "Char >=" ((\(C# a#) (C# b#) -> a# `geChar#` b#) a    b   ) -- False
  testw "Char >=" ((\(C# a#) (C# b#) -> a# `geChar#` b#) minC minC) -- True
  testw "Char >=" ((\(C# a#) (C# b#) -> a# `geChar#` b#) maxC maxC) -- True
  testw "Char >=" ((\(C# a#) (C# b#) -> a# `geChar#` b#) minC maxC) -- False
  testw "Char >=" ((\(C# a#) (C# b#) -> a# `geChar#` b#) maxC minC) -- True

  putStrLn "=== testing Char < ==="
  testw "Char <" ((\(C# a#) (C# b#) -> a# `ltChar#` b#) a    a   ) -- False
  testw "Char <" ((\(C# a#) (C# b#) -> a# `ltChar#` b#) b    a   ) -- False
  testw "Char <" ((\(C# a#) (C# b#) -> a# `ltChar#` b#) a    b   ) -- True
  testw "Char <" ((\(C# a#) (C# b#) -> a# `ltChar#` b#) minC minC) -- False
  testw "Char <" ((\(C# a#) (C# b#) -> a# `ltChar#` b#) maxC maxC) -- False
  testw "Char <" ((\(C# a#) (C# b#) -> a# `ltChar#` b#) minC maxC) -- True
  testw "Char <" ((\(C# a#) (C# b#) -> a# `ltChar#` b#) maxC minC) -- False

  putStrLn "=== testing Char <= ==="
  testw "Char <=" ((\(C# a#) (C# b#) -> a# `leChar#` b#) a    a   ) -- True
  testw "Char <=" ((\(C# a#) (C# b#) -> a# `leChar#` b#) b    a   ) -- False
  testw "Char <=" ((\(C# a#) (C# b#) -> a# `leChar#` b#) a    b   ) -- True
  testw "Char <=" ((\(C# a#) (C# b#) -> a# `leChar#` b#) minC minC) -- True
  testw "Char <=" ((\(C# a#) (C# b#) -> a# `leChar#` b#) maxC maxC) -- True
  testw "Char <=" ((\(C# a#) (C# b#) -> a# `leChar#` b#) minC maxC) -- True
  testw "Char <=" ((\(C# a#) (C# b#) -> a# `leChar#` b#) maxC minC) -- False

  putStrLn "=== testing Char == ==="
  testw "Char ==" ((\(C# a#) (C# b#) -> a# `eqChar#` b#) a    a   ) -- True
  testw "Char ==" ((\(C# a#) (C# b#) -> a# `eqChar#` b#) b    a   ) -- False
  testw "Char ==" ((\(C# a#) (C# b#) -> a# `eqChar#` b#) a    b   ) -- False
  testw "Char ==" ((\(C# a#) (C# b#) -> a# `eqChar#` b#) minC minC) -- True
  testw "Char ==" ((\(C# a#) (C# b#) -> a# `eqChar#` b#) maxC maxC) -- True
  testw "Char ==" ((\(C# a#) (C# b#) -> a# `eqChar#` b#) minC maxC) -- False
  testw "Char ==" ((\(C# a#) (C# b#) -> a# `eqChar#` b#) maxC minC) -- False

  putStrLn "=== testing Char /= ==="
  testw "Char /=" ((\(C# a#) (C# b#) -> a# `neChar#` b#) a    a   ) -- False
  testw "Char /=" ((\(C# a#) (C# b#) -> a# `neChar#` b#) b    a   ) -- True
  testw "Char /=" ((\(C# a#) (C# b#) -> a# `neChar#` b#) a    b   ) -- True
  testw "Char /=" ((\(C# a#) (C# b#) -> a# `neChar#` b#) minC minC) -- False
  testw "Char /=" ((\(C# a#) (C# b#) -> a# `neChar#` b#) maxC maxC) -- False
  testw "Char /=" ((\(C# a#) (C# b#) -> a# `neChar#` b#) minC maxC) -- True
  testw "Char /=" ((\(C# a#) (C# b#) -> a# `neChar#` b#) maxC minC) -- True

  -- Wrappers for comparing Int
  putStrLn "=== testing Int > ==="
  testw "Int >" ((\(I# a#) (I# b#) -> a# ># b#) 0        0       ) -- False
  testw "Int >" ((\(I# a#) (I# b#) -> a# ># b#) 1        0       ) -- True
  testw "Int >" ((\(I# a#) (I# b#) -> a# ># b#) 0        1       ) -- False
  testw "Int >" ((\(I# a#) (I# b#) -> a# ># b#) minI     minI    ) -- False
  testw "Int >" ((\(I# a#) (I# b#) -> a# ># b#) maxI     maxI    ) -- False
  testw "Int >" ((\(I# a#) (I# b#) -> a# ># b#) (minI+1) minI    ) -- True
  testw "Int >" ((\(I# a#) (I# b#) -> a# ># b#) minI     (minI-1)) -- False (overflow)
  testw "Int >" ((\(I# a#) (I# b#) -> a# ># b#) (maxI+1) maxI    ) -- False (overflow)
  testw "Int >" ((\(I# a#) (I# b#) -> a# ># b#) maxI     (maxI-1)) -- True

  putStrLn "=== testing Int < ==="
  testw "Int <" ((\(I# a#) (I# b#) -> a# <# b#) 0        0       ) -- False
  testw "Int <" ((\(I# a#) (I# b#) -> a# <# b#) 1        0       ) -- False
  testw "Int <" ((\(I# a#) (I# b#) -> a# <# b#) 0        1       ) -- True
  testw "Int <" ((\(I# a#) (I# b#) -> a# <# b#) minI     minI    ) -- False
  testw "Int <" ((\(I# a#) (I# b#) -> a# <# b#) maxI     maxI    ) -- False
  testw "Int <" ((\(I# a#) (I# b#) -> a# <# b#) minI     (minI+1)) -- True
  testw "Int <" ((\(I# a#) (I# b#) -> a# <# b#) (minI-1) minI    ) -- False (overflow)
  testw "Int <" ((\(I# a#) (I# b#) -> a# <# b#) maxI     (maxI+1)) -- False (overflow)
  testw "Int <" ((\(I# a#) (I# b#) -> a# <# b#) (maxI-1) maxI    ) -- True

  putStrLn "=== testing Int >= ==="
  testw "Int >=" ((\(I# a#) (I# b#) -> a# >=# b#) 0        0       ) -- True
  testw "Int >=" ((\(I# a#) (I# b#) -> a# >=# b#) 1        0       ) -- True
  testw "Int >=" ((\(I# a#) (I# b#) -> a# >=# b#) 0        1       ) -- False
  testw "Int >=" ((\(I# a#) (I# b#) -> a# >=# b#) minI     minI    ) -- True
  testw "Int >=" ((\(I# a#) (I# b#) -> a# >=# b#) maxI     maxI    ) -- True
  testw "Int >=" ((\(I# a#) (I# b#) -> a# >=# b#) (minI+1) minI    ) -- True
  testw "Int >=" ((\(I# a#) (I# b#) -> a# >=# b#) minI     (minI-1)) -- False (overflow)
  testw "Int >=" ((\(I# a#) (I# b#) -> a# >=# b#) (maxI+1) maxI    ) -- False (overflow)
  testw "Int >=" ((\(I# a#) (I# b#) -> a# >=# b#) maxI     (maxI-1)) -- True

  putStrLn "=== testing Int <= ==="
  testw "Int <=" ((\(I# a#) (I# b#) -> a# <=# b#) 0        0       ) -- True
  testw "Int <=" ((\(I# a#) (I# b#) -> a# <=# b#) 1        0       ) -- False
  testw "Int <=" ((\(I# a#) (I# b#) -> a# <=# b#) 0        1       ) -- True
  testw "Int <=" ((\(I# a#) (I# b#) -> a# <=# b#) minI     minI    ) -- True
  testw "Int <=" ((\(I# a#) (I# b#) -> a# <=# b#) maxI     maxI    ) -- True
  testw "Int <=" ((\(I# a#) (I# b#) -> a# <=# b#) minI     (minI+1)) -- True
  testw "Int <=" ((\(I# a#) (I# b#) -> a# <=# b#) (minI-1) minI    ) -- False (overflow)
  testw "Int <=" ((\(I# a#) (I# b#) -> a# <=# b#) maxI     (maxI+1)) -- False (overflow)
  testw "Int <=" ((\(I# a#) (I# b#) -> a# <=# b#) (maxI-1) maxI    ) -- True

  putStrLn "=== testing Int == ==="
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) 0        0       ) -- True
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) 1        0       ) -- False
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) 0        1       ) -- False
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) maxI     maxI    ) -- True
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) (maxI-1) maxI    ) -- False
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) minI     minI    ) -- True
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) minI     (minI+1)) -- False
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) minI     (maxI+1)) -- True (overflow)
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) (maxI+1) minI    ) -- True (overflow)
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) maxI     (minI-1)) -- True (overflow)
  testw "Int ==" ((\(I# a#) (I# b#) -> a# ==# b#) (minI-1) maxI    ) -- True (overflow)

  putStrLn "=== testing Int /= ==="
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) 0        0       ) -- False
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) 1        0       ) -- True
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) 0        1       ) -- True
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) maxI     maxI    ) -- False
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) (maxI-1) maxI    ) -- True
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) minI     minI    ) -- False
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) minI     (minI+1)) -- True
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) minI     (maxI+1)) -- False (overflow)
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) (maxI+1) minI    ) -- False (overflow)
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) maxI     (minI-1)) -- False (overflow)
  testw "Int /=" ((\(I# a#) (I# b#) -> a# /=# b#) (minI-1) maxI    ) -- False (overflow)

  -- Wrappers for comparing Word
  putStrLn "=== testing Word > ==="
  testw "Word >" ((\(W# a#) (W# b#) -> a# `gtWord#` b#) zeroW       zeroW      ) -- False
  testw "Word >" ((\(W# a#) (W# b#) -> a# `gtWord#` b#) oneW        zeroW      ) -- True
  testw "Word >" ((\(W# a#) (W# b#) -> a# `gtWord#` b#) zeroW       oneW       ) -- False
  testw "Word >" ((\(W# a#) (W# b#) -> a# `gtWord#` b#) minW        minW       ) -- False
  testw "Word >" ((\(W# a#) (W# b#) -> a# `gtWord#` b#) maxW        maxW       ) -- False
  testw "Word >" ((\(W# a#) (W# b#) -> a# `gtWord#` b#) (minW+oneW) minW       ) -- True
  testw "Word >" ((\(W# a#) (W# b#) -> a# `gtWord#` b#) minW        (minW-oneW)) -- False (overflow)
  testw "Word >" ((\(W# a#) (W# b#) -> a# `gtWord#` b#) (maxW+oneW) maxW       ) -- False (overflow)
  testw "Word >" ((\(W# a#) (W# b#) -> a# `gtWord#` b#) maxW        (maxW-oneW)) -- True

  putStrLn "=== testing Word < ==="
  testw "Word <" ((\(W# a#) (W# b#) -> a# `ltWord#` b#) zeroW       zeroW      ) -- False
  testw "Word <" ((\(W# a#) (W# b#) -> a# `ltWord#` b#) oneW        zeroW      ) -- False
  testw "Word <" ((\(W# a#) (W# b#) -> a# `ltWord#` b#) zeroW       oneW       ) -- True
  testw "Word <" ((\(W# a#) (W# b#) -> a# `ltWord#` b#) minW        minW       ) -- False
  testw "Word <" ((\(W# a#) (W# b#) -> a# `ltWord#` b#) maxW        maxW       ) -- False
  testw "Word <" ((\(W# a#) (W# b#) -> a# `ltWord#` b#) minW        (minW+oneW)) -- True
  testw "Word <" ((\(W# a#) (W# b#) -> a# `ltWord#` b#) (minW-oneW) minW       ) -- False (overflow)
  testw "Word <" ((\(W# a#) (W# b#) -> a# `ltWord#` b#) maxW        (maxW+oneW)) -- False (overflow)
  testw "Word <" ((\(W# a#) (W# b#) -> a# `ltWord#` b#) (maxW-oneW) maxW       ) -- True

  putStrLn "=== testing Word >= ==="
  testw "Word >=" ((\(W# a#) (W# b#) -> a# `geWord#` b#) zeroW       zeroW      ) -- True
  testw "Word >=" ((\(W# a#) (W# b#) -> a# `geWord#` b#) oneW        zeroW      ) -- True
  testw "Word >=" ((\(W# a#) (W# b#) -> a# `geWord#` b#) zeroW       oneW       ) -- False
  testw "Word >=" ((\(W# a#) (W# b#) -> a# `geWord#` b#) minW        minW       ) -- True
  testw "Word >=" ((\(W# a#) (W# b#) -> a# `geWord#` b#) maxW        maxW       ) -- True
  testw "Word >=" ((\(W# a#) (W# b#) -> a# `geWord#` b#) (minW+oneW) minW       ) -- True
  testw "Word >=" ((\(W# a#) (W# b#) -> a# `geWord#` b#) minW        (minW-oneW)) -- False (overflow)
  testw "Word >=" ((\(W# a#) (W# b#) -> a# `geWord#` b#) (maxW+oneW) maxW       ) -- False (overflow)
  testw "Word >=" ((\(W# a#) (W# b#) -> a# `geWord#` b#) maxW        (maxW-oneW)) -- True

  putStrLn "=== testing Word <= ==="
  testw "Word <=" ((\(W# a#) (W# b#) -> a# `leWord#` b#) zeroW       zeroW      ) -- True
  testw "Word <=" ((\(W# a#) (W# b#) -> a# `leWord#` b#) oneW        zeroW      ) -- False
  testw "Word <=" ((\(W# a#) (W# b#) -> a# `leWord#` b#) zeroW       oneW       ) -- True
  testw "Word <=" ((\(W# a#) (W# b#) -> a# `leWord#` b#) minW        minW       ) -- True
  testw "Word <=" ((\(W# a#) (W# b#) -> a# `leWord#` b#) maxW        maxW       ) -- True
  testw "Word <=" ((\(W# a#) (W# b#) -> a# `leWord#` b#) minW        (minW+oneW)) -- True
  testw "Word <=" ((\(W# a#) (W# b#) -> a# `leWord#` b#) (minW-oneW) minW       ) -- False (overflow)
  testw "Word <=" ((\(W# a#) (W# b#) -> a# `leWord#` b#) maxW        (maxW+oneW)) -- False (overflow)
  testw "Word <=" ((\(W# a#) (W# b#) -> a# `leWord#` b#) (maxW-oneW) maxW       ) -- True

  putStrLn "=== testing Word == ==="
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) zeroW       zeroW      ) -- True
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) oneW        zeroW      ) -- False
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) zeroW       oneW       ) -- False
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) maxW        maxW       ) -- True
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) (maxW-oneW) maxW       ) -- False
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) minW        minW       ) -- True
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) minW        (minW+oneW)) -- False
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) minW        (maxW+oneW)) -- True (overflow)
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) (maxW+oneW) minW       ) -- True (overflow)
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) maxW        (minW-oneW)) -- True (overflow)
  testw "Word ==" ((\(W# a#) (W# b#) -> a# `eqWord#` b#) (minW-oneW) maxW       ) -- True (overflow)

  putStrLn "=== testing Word /= ==="
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) zeroW       zeroW      ) -- False
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) oneW        zeroW      ) -- True
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) zeroW       oneW       ) -- True
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) maxW        maxW       ) -- False
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) (maxW-oneW) maxW       ) -- True
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) minW        minW       ) -- False
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) minW        (minW+oneW)) -- True
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) minW        (maxW+oneW)) -- False (overflow)
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) (maxW+oneW) minW       ) -- False (overflow)
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) maxW        (minW-oneW)) -- False (overflow)
  testw "Word /=" ((\(W# a#) (W# b#) -> a# `neWord#` b#) (minW-oneW) maxW       ) -- False (overflow)

  -- Wrappers for comparing Double
  putStrLn "=== testing Double > ==="
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  0.0  0.0 ) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  1.0  0.0 ) -- True
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  0.0  1.0 ) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  0.0  nan ) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  nan  0.0 ) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  nan  nan ) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  infp infp) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  infn infn) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  infp infn) -- True
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  infn infp) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  infp nan ) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  infn nan ) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  nan  infp) -- False
  testw "Double >"  ((\(D# a) (D# b) -> a >## b)  nan  infn) -- False

  putStrLn "=== testing Double < ==="
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  0.0  0.0 ) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  1.0  0.0 ) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  0.0  1.0 ) -- True
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  0.0  nan ) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  nan  0.0 ) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  nan  nan ) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  infp infp) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  infn infn) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  infp infn) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  infn infp) -- True
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  infp nan ) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  infn nan ) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  nan  infp) -- False
  testw "Double <"  ((\(D# a) (D# b) -> a <## b)  nan  infn) -- False

  putStrLn "=== testing Double >= ==="
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) 0.0  0.0 ) -- True
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) 1.0  0.0 ) -- True
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) 0.0  1.0 ) -- False
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) 0.0  nan ) -- False
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) nan  0.0 ) -- False
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) nan  nan ) -- False
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) infp infp) -- True
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) infn infn) -- True
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) infp infn) -- True
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) infn infp) -- False
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) infp nan ) -- False
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) infn nan ) -- False
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) nan  infp) -- False
  testw "Double >=" ((\(D# a) (D# b) -> a >=## b) nan  infn) -- False

  putStrLn "=== testing Double <= ==="
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) 0.0  0.0 ) -- True
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) 1.0  0.0 ) -- False
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) 0.0  1.0 ) -- True
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) 0.0  nan ) -- False
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) nan  0.0 ) -- False
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) nan  nan ) -- False
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) infp infp) -- True
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) infn infn) -- True
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) infp infn) -- False
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) infn infp) -- True
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) infp nan ) -- False
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) infn nan ) -- False
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) nan  infp) -- False
  testw "Double <=" ((\(D# a) (D# b) -> a <=## b) nan  infn) -- False

  putStrLn "=== testing Double == ==="
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) 0.0  0.0 ) -- True
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) 1.0  0.0 ) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) 0.0  1.0 ) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) 0.0  nan ) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) nan  0.0 ) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) nan  nan ) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) infp infp) -- True
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) infn infn) -- True
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) infp infn) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) infn infp) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) infp nan ) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) infn nan ) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) nan  infp) -- False
  testw "Double ==" ((\(D# a) (D# b) -> a ==## b) nan  infn) -- False

  putStrLn "=== testing Double /= ==="
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) 0.0  0.0 ) -- False
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) 1.0  0.0 ) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) 0.0  1.0 ) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) 0.0  nan ) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) nan  0.0 ) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) nan  nan ) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) infp infp) -- False
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) infn infn) -- False
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) infp infn) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) infn infp) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) infp nan ) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) infn nan ) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) nan  infp) -- True
  testw "Double /=" ((\(D# a) (D# b) -> a /=## b) nan  infn) -- True

  -- Wrappers for comparing Float
  putStrLn "=== testing Float > ==="
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) zeroF zeroF) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) oneF  zeroF) -- True
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) zeroF oneF ) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) zeroF nanF ) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) nanF  zeroF) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) nanF  nanF ) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) infpF infpF) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) infnF infnF) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) infpF infnF) -- True
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) infnF infpF) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) infpF nanF ) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) infnF nanF ) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) nanF  infpF) -- False
  testw "Float >"  ((\(F# a) (F# b) -> a `gtFloat#` b) nanF  infnF) -- False

  putStrLn "=== testing Float < ==="
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) zeroF zeroF) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) oneF  zeroF) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) zeroF oneF ) -- True
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) zeroF nanF ) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) nanF  zeroF) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) nanF  nanF ) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) infpF infpF) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) infnF infnF) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) infpF infnF) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) infnF infpF) -- True
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) infpF nanF ) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) infnF nanF ) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) nanF  infpF) -- False
  testw "Float <"  ((\(F# a) (F# b) -> a `ltFloat#` b) nanF  infnF) -- False

  putStrLn "=== testing Float >= ==="
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) zeroF zeroF) -- True
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) oneF  zeroF) -- True
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) zeroF oneF ) -- False
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) zeroF nanF ) -- False
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) nanF  zeroF) -- False
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) nanF  nanF ) -- False
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) infpF infpF) -- True
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) infnF infnF) -- True
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) infpF infnF) -- True
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) infnF infpF) -- False
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) infpF nanF ) -- False
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) infnF nanF ) -- False
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) nanF  infpF) -- False
  testw "Float >=" ((\(F# a) (F# b) -> a `geFloat#` b) nanF  infnF) -- False

  putStrLn "=== testing Float <= ==="
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) zeroF zeroF) -- True
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) oneF  zeroF) -- False
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) zeroF oneF ) -- True
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) zeroF nanF ) -- False
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) nanF  zeroF) -- False
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) nanF  nanF ) -- False
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) infpF infpF) -- True
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) infnF infnF) -- True
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) infpF infnF) -- False
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) infnF infpF) -- True
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) infpF nanF ) -- False
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) infnF nanF ) -- False
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) nanF  infpF) -- False
  testw "Float <=" ((\(F# a) (F# b) -> a `leFloat#` b) nanF  infnF) -- False

  putStrLn "=== testing Float == ==="
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) zeroF zeroF) -- True
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) oneF  zeroF) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) zeroF oneF ) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) zeroF nanF ) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) nanF  zeroF) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) nanF  nanF ) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) infpF infpF) -- True
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) infnF infnF) -- True
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) infpF infnF) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) infnF infpF) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) infpF nanF ) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) infnF nanF ) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) nanF  infpF) -- False
  testw "Float ==" ((\(F# a) (F# b) -> a `eqFloat#` b) nanF  infnF) -- False

  putStrLn "=== testing Float /= ==="
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) zeroF zeroF) -- False
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) oneF  zeroF) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) zeroF oneF ) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) zeroF nanF ) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) nanF  zeroF) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) nanF  nanF ) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) infpF infpF) -- False
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) infnF infnF) -- False
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) infpF infnF) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) infnF infpF) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) infpF nanF ) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) infnF nanF ) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) nanF  infpF) -- True
  testw "Float /=" ((\(F# a) (F# b) -> a `neFloat#` b) nanF  infnF) -- True
    where
      -- every integer is compared to 1 representing True. This
      -- results in printing "True" when the primop should return True
      -- and printing "False" when it should return False
      test :: String -> Int# -> IO ()
      test str x = putStrLn $ str ++ " " ++ (show (I# x == 1))
      testw :: String -> Bool -> IO ()
      testw str x = putStrLn $ str ++ " " ++ (show x)
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
