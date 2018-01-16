module T10196 where

data X = Xᵦ | Xᵤ | Xᵩ | Xᵢ | Xᵪ | Xᵣ

f :: Int
f =
  let xᵦ = 1
      xᵤ = xᵦ
      xᵩ = xᵤ
      xᵢ = xᵩ
      xᵪ = xᵢ
      xᵣ = xᵪ
  in xᵣ

-- Modifier letters are also allowed in the middle of an identifier.
-- This should not be lexed as 2 separate identifiers.
xᵦx :: Int
xᵦx = 1
