{-
 -  Fulsom (The Solid Modeller, written in Haskell)
 -
 -  Copyright 1990,1991,1992,1993 Duncan Sinclair
 -
 - Permissiom to use, copy, modify, and distribute this software for any 
 - purpose and without fee is hereby granted, provided that the above
 - copyright notice and this permission notice appear in all copies, and
 - that my name not be used in advertising or publicity pertaining to this
 - software without specific, written prior permission.  I makes no
 - representations about the suitability of this software for any purpose.
 - It is provided ``as is'' without express or implied warranty.
 - 
 - Duncan Sinclair 1993.
 - 
 - Colour definitions.
 -
 - (The information is this file is derived from the rgb database
 -  of colours from X11R5, which could well be copyright MIT, and
 -  is also known for it's imperfections.)
 -
 -}

module Kolor where

import Types
import Interval

-- black = mkcolor 0 0 0

unmkcolor :: Color -> (Int,Int,Int)
unmkcolor (RGB x y z) = (a,b,c)
  where
   a = round (255.5 * x)
   b = round (255.5 * y)
   c = round (255.5 * z)

-- mkcolor :: Int -> Int -> Int -> Color
mkcolor x y z = RGB a b c
  where
   a = (x / 255.0) :: FType
   b = (y / 255.0) :: FType
   c = (z / 255.0) :: FType


white			= mkcolor 255 255 255
ivory			= mkcolor 255 255 240
light_yellow		= mkcolor 255 255 224
yellow			= mkcolor 255 255   0
snow			= mkcolor 255 250 250
floral_white		= mkcolor 255 250 240
lemon_chiffon		= mkcolor 255 250 205
cornsilk		= mkcolor 255 248 220
seashell		= mkcolor 255 245 238
lavender_blush		= mkcolor 255 240 245
papaya_whip		= mkcolor 255 239 213
blanched_almond		= mkcolor 255 235 205
misty_rose		= mkcolor 255 228 225
bisque			= mkcolor 255 228 196
moccasin		= mkcolor 255 228 181
navajo_white		= mkcolor 255 222 173
peach_puff		= mkcolor 255 218 185
gold			= mkcolor 255 215   0
pink			= mkcolor 255 192 203
light_pink		= mkcolor 255 182 193
orange			= mkcolor 255 165   0
light_salmon		= mkcolor 255 160 122
dark_orange		= mkcolor 255 140   0
coral			= mkcolor 255 127  80
hot_pink		= mkcolor 255 105 180
tomato			= mkcolor 255  99  71
orange_red		= mkcolor 255  69   0
deep_pink		= mkcolor 255  20 147
magenta			= mkcolor 255   0 255
red			= mkcolor 255   0   0
old_lace		= mkcolor 253 245 230
light_goldenrod_yellow	= mkcolor 250 250 210
linen			= mkcolor 250 240 230
antique_white		= mkcolor 250 235 215
salmon			= mkcolor 250 128 114
ghost_white		= mkcolor 248 248 255
mint_cream		= mkcolor 245 255 250
white_smoke		= mkcolor 245 245 245
beige			= mkcolor 245 245 220
wheat			= mkcolor 245 222 179
sandy_brown		= mkcolor 244 164  96
azure			= mkcolor 240 255 255
honeydew		= mkcolor 240 255 240
alice_blue		= mkcolor 240 248 255
khaki			= mkcolor 240 230 140
light_coral		= mkcolor 240 128 128
pale_goldenrod		= mkcolor 238 232 170
light_goldenrod		= mkcolor 238 221 130
violet			= mkcolor 238 130 238
dark_salmon		= mkcolor 233 150 122
lavender		= mkcolor 230 230 250
light_cyan		= mkcolor 224 255 255
burlywood		= mkcolor 222 184 135
plum			= mkcolor 221 160 221
gainsboro		= mkcolor 220 220 220
pale_violet_red		= mkcolor 219 112 147
goldenrod		= mkcolor 218 165  32
orchid			= mkcolor 218 112 214
thistle			= mkcolor 216 191 216
light_grey		= mkcolor 211 211 211
-- tan			= mkcolor 210 180 140
chocolate		= mkcolor 210 105  30
violet_red		= mkcolor 208  32 144
peru			= mkcolor 205 133  63
indian_red		= mkcolor 205  92  92
medium_violet_red	= mkcolor 199  21 133
grey			= mkcolor 190 190 190
dark_khaki		= mkcolor 189 183 107
rosy_brown		= mkcolor 188 143 143
medium_orchid		= mkcolor 186  85 211
dark_goldenrod		= mkcolor 184 134  11
firebrick		= mkcolor 178  34  34
powder_blue		= mkcolor 176 224 230
light_steel_blue	= mkcolor 176 196 222
maroon			= mkcolor 176  48  96
pale_turquoise		= mkcolor 175 238 238
green_yellow		= mkcolor 173 255  47
light_blue		= mkcolor 173 216 230
brown			= mkcolor 165  42  42
sienna			= mkcolor 160  82  45
purple			= mkcolor 160  32 240
yellow_green		= mkcolor 154 205  50
dark_orchid		= mkcolor 153  50 204
pale_green		= mkcolor 152 251 152
dark_violet		= mkcolor 148   0 211
medium_purple		= mkcolor 147 112 219
dark_sea_green		= mkcolor 143 188 143
saddle_brown		= mkcolor 139  69  19
blue_violet		= mkcolor 138  43 226
light_sky_blue		= mkcolor 135 206 250
sky_blue		= mkcolor 135 206 235
light_slate_blue	= mkcolor 132 112 255
aquamarine		= mkcolor 127 255 212
chartreuse		= mkcolor 127 255   0
lawn_green		= mkcolor 124 252   0
medium_slate_blue	= mkcolor 123 104 238
light_slate_grey	= mkcolor 119 136 153
slate_grey		= mkcolor 112 128 144
olive_drab		= mkcolor 107 142  35
slate_blue		= mkcolor 106  90 205
dim_grey		= mkcolor 105 105 105
medium_aquamarine	= mkcolor 102 205 170
cornflower_blue		= mkcolor 100 149 237
cadet_blue		= mkcolor  95 158 160
dark_olive_green	= mkcolor  85 107  47
medium_turquoise	= mkcolor  72 209 204
dark_slate_blue		= mkcolor  72  61 139
steel_blue		= mkcolor  70 130 180
royal_blue		= mkcolor  65 105 225
turquoise		= mkcolor  64 224 208
medium_sea_green	= mkcolor  60 179 113
lime_green		= mkcolor  50 205  50
dark_slate_grey		= mkcolor  47  79  79
sea_green		= mkcolor  46 139  87
forest_green		= mkcolor  34 139  34
light_sea_green		= mkcolor  32 178 170
dodger_blue		= mkcolor  30 144 255
midnight_blue		= mkcolor  25  25 112
cyan			= mkcolor   0 255 255
spring_green		= mkcolor   0 255 127
green			= mkcolor   0 255   0
medium_spring_green	= mkcolor   0 250 154
dark_turquoise		= mkcolor   0 206 209
deep_sky_blue		= mkcolor   0 191 255
dark_green		= mkcolor   0 100   0
blue			= mkcolor   0   0 255
medium_blue		= mkcolor   0   0 205
navy_blue		= mkcolor   0   0 128
navy			= mkcolor   0   0 128
black			= mkcolor   0   0   0
