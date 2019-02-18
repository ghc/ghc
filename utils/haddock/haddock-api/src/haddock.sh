# Mini-driver for Haddock

# needs the following variables:
#	HADDOCKLIB
#	HADDOCKBIN

$HADDOCKBIN --lib $HADDOCKLIB ${1+"$@"}
