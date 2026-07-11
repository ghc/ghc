-- Boxing an automatically unpacked enum field (tagToEnum#) should index the
-- closure table in a single scaled-index addressing mode, with the 1-based
-- tag adjustment folded into the label offset.
module EnumClosureTblAmode where

data Color = Red | Green | Blue | Yellow | Purple

data Rec = Rec !Bool !Color

getColor :: Rec -> Color
getColor (Rec _ c) = c
