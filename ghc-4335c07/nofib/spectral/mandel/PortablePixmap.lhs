
\begin{verbatim}
ppm(5)                    FILE FORMATS                     ppm(5)

NAME
     ppm - portable pixmap file format

DESCRIPTION
     The portable pixmap format is a  lowest  common  denominator
     color image file format.  The definition is as follows:

     - A "magic number" for identifying the  file  type.   A  ppm
       file's magic number is the two characters "P3".

     - Whitespace (blanks, TABs, CRs, LFs).

     - A width, formatted as ASCII characters in decimal.

     - Whitespace.

     - A height, again in ASCII decimal.

     - Whitespace.

    - The maximum color-component value, again in ASCII decimal.

     - Whitespace.

     - Width * height pixels, each  three  ASCII  decimal  values
       between 0 and the specified maximum value, starting at the
       top-left corner of the pixmap, proceding in normal English
       reading  order.  The three values for each pixel represent
       red, green, and blue, respectively; a  value  of  0  means
       that  color is off, and the maximum value means that color
       is maxxed out.

     - Characters from a "#" to the next end-of-line are  ignored
       (comments).

     - No line should be longer than 70 characters.

     Here is an example of a small pixmap in this format:
     P3
     # feep.ppm
     4 4
     15
      0  0  0    0  0  0    0  0  0   15  0 15
      0  0  0    0 15  7    0  0  0    0  0  0
      0  0  0    0  0  0    0 15  7    0  0  0
     15  0 15    0  0  0    0  0  0    0  0  0

     Programs that read this format should be as lenient as  pos-
     sible, accepting anything that looks remotely like a pixmap.

     There is also a variant on the format, available by  setting
     the  RAWBITS  option  at  compile  time.   This  variant  is

Sun Release 4.1  Last change: 15 September 1990                 1

ppm(5)                    FILE FORMATS                     ppm(5)

     different in the following ways:

     - The "magic number" is "P6" instead of "P3".

     - The pixel values are stored as  plain  bytes,  instead  of
       ASCII decimal.

     - Whitespace is not allowed in the pixels area, and  only  a
       single  character  of  whitespace (typically a newline) is
       allowed after the maxval.

     - The files are smaller and many times faster  to  read  and
       write.

     Note that this raw format can only be used for maxvals  less
     than or equal to 255.  If you use the ppm library and try to
     write a file with a larger  maxval,  it  will  automatically
     fall back on the slower but more general plain format.

SEE ALSO
     giftoppm(1),   gouldtoppm(1),   ilbmtoppm(1),   imgtoppm(1),
     mtvtoppm(1),  pcxtoppm(1),  pgmtoppm(1),  pi1toppm(1), pict-
     toppm(1),    qrttoppm(1),     rawtoppm(1),     rgb3toppm(1),
     spctoppm(1),    sputoppm(1),    tgatoppm(1),    ximtoppm(1),
     xpmtoppm(1),   ppmtogif(1),    ppmtoicr(1),    ppmtoilbm(1),
     ppmtopcx(1),    ppmtopgm(1),    ppmtopi1(1),   ppmtopict(1),
     ppmtops(1),   ppmtopuzz(1),    ppmtorgb3(1),    ppmtouil(1),
     ppmtoxpm(1), ppmhist(1), ppmmake(1), ppmpat(1), ppmquant(1),
     ppmquantall(1), ppmrelief(1), pnm(5), pgm(5), pbm(5)

AUTHOR
     Copyright (C) 1989, 1991 by Jef Poskanzer.

Sun Release 4.1  Last change: 15 September 1990                 2
\end{verbatim}
\begin{code}
module PortablePixmap where

data PixMap = Pixmap Integer Integer Int [(Int,Int,Int)]

createPixmap::Integer -> Integer -> Int -> [(Int,Int,Int)] -> PixMap
createPixmap width height max colours = Pixmap width height max colours

instance Show PixMap where
	showsPrec prec (Pixmap x y z rgbs) = showHeader x y z . showRGB rgbs


showHeader::Integer -> Integer -> Int -> ShowS
showHeader x y z 	= showString "P6\n" . showBanner .
			  shows x . showReturn .
			  shows y . showReturn .
			  shows z . showReturn

showRGB::[(Int,Int,Int)] ->  ShowS
showRGB [] 		= id
showRGB ((r,g,b):rest)  = showChar (toEnum r) .
			  showChar (toEnum g) .
			  showChar (toEnum b) .
			  showRGB rest

showSpace  = showChar ' '
showReturn = showChar '\n'

showBanner = showString "# Portable pixmap created by Haskell Program :\n" .
	     showString "#\tPortablePixmap.lhs (Jon.Hill 28/5/92)\n"
\end{code}
