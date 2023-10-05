module T17901 where

data T = A | B | C

f1 wombat1 x = case x of
                A -> wombat1 A
                B -> wombat1 B
                C -> wombat1 C

data S = SA Int | SB

f2 wombat2 x = case x of
                 SA _ -> wombat2 x
                 SB   -> wombat2 x

data W = WB | WA Int

f3 wombat3 x = case x of
                WA _ -> wombat3 x
                WB   -> wombat3 x

