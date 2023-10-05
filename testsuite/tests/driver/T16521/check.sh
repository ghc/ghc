#! /bin/sh

checkDups() {
	# Check for duplicate lines
	if [ $(uniq $1 -d | wc -l) -ne 0 ]
	then
		echo "Duplicate dependencies:"
		uniq $1 -d
	fi
}

expectDep() {
	if ! grep -q $1 "$2" $3
	then
		echo "Missing: \"$2\""
	fi
}

checkDups Makefile1.out
expectDep -F "A.o : A.hs" Makefile1.out
expectDep -F "A.o : a.h" Makefile1.out
expectDep -F "A.o : b.h" Makefile1.out
expectDep -F "A.o : b2.h" Makefile1.out
expectDep "" "A\.o : .*/ghcversion.h" Makefile1.out
expectDep "" "A\.o : .*/processFlags.h" Makefile1.out

checkDups Makefile2.out
expectDep -F "A._o A.o : A.hs" Makefile2.out
expectDep -F "A._o A.o : a.h" Makefile2.out
expectDep -F "A._o A.o : b.h" Makefile2.out
expectDep -F "A._o A.o : b2.h" Makefile2.out
expectDep "" "A\._o A\.o : .*/ghcversion.h" Makefile2.out
expectDep "" "A\._o A\.o : .*/processFlags.h" Makefile2.out