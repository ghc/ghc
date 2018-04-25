#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/bash

if [[ $# < 1 ]]; then
    echo "Usage: $0 <exe>"
    exit 1
fi

exe=$1

rm -f $exe.tix

f=$(mktemp stdio-f.XXXXXX)
g=$(mktemp stdio-g.XXXXXX)

for t in T TL; do
    echo $t.readFile > $f
    $exe $t.readFile $f > $g
    if ! diff -u $f $g; then
	errs=$((errs+1))
	echo FAIL: $t.readFile 1>&2
    fi

    $exe $t.writeFile $f $t.writeFile
    echo -n $t.writeFile > $g
    if ! diff -u $f $g; then
	errs=$((errs+1))
	echo FAIL: $t.writeFile 1>&2
    fi

    echo -n quux > $f
    $exe $t.appendFile $f $t.appendFile
    echo -n quux$t.appendFile > $g
    if ! diff -u $f $g; then
	errs=$((errs+1))
	echo FAIL: $t.appendFile 1>&2
    fi

    echo $t.interact | $exe $t.interact > $f
    echo $t.interact > $g
    if ! diff -u $f $g; then
	errs=$((errs+1))
	echo FAIL: $t.interact 1>&2
    fi

    echo $t.getContents | $exe $t.getContents > $f
    echo $t.getContents > $g
    if ! diff -u $f $g; then
	errs=$((errs+1))
	echo FAIL: $t.getContents 1>&2
    fi

    echo $t.getLine | $exe $t.getLine > $f
    echo $t.getLine > $g
    if ! diff -u $f $g; then
	errs=$((errs+1))
	echo FAIL: $t.getLine 1>&2
    fi
done

rm -f $f $g

exit $errs
