# Mini-driver for Haddock

# needs the following variables:
#	HADDOCKCSS
#	HADDOCKBIN

case $* in
*--css*) $HADDOCKBIN ${1+"$@"};;
*)       $HADDOCKBIN --css $HADDOCKCSS ${1+"$@"};;
esac
