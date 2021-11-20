#!/bin/sh

# POSIX shell implementation of `realpath --relative-to=$1 $2.
# This is an adaptation of the implementation from
# <https://github.com/Offirmo/offirmo-shell-lib>.

# returns relative path to $2=$target from $1=$source
## NOTE : path are compared in text only. They don’t have to exist
##        and they WONT be normalized/escaped
## Result in "$return_value"# both $1 and $2 are absolute paths beginning with /

src="$1"
target="$2"

common_part="$src"
result=""

while test "${target#$common_part}" = "${target}" ; do
    #echo "common_part is now : \"$common_part\""
    #echo "result is now      : \"$result\""
    #echo "target#common_part : \"${target#$common_part}\""
    # no match, means that candidate common part is not correct
    # go up one level (reduce common part)
    common_part="$(dirname "$common_part")"
    # and record that we went back
    if test -z "$result" ; then
        result=".."
    else
        result="../$result"
    fi
done

#echo "common_part is     : \"$common_part\""

if test "$common_part" = "/" ; then
    # special case for root (no common path)
    result="$result/"
fi

# since we now have identified the common part,
# compute the non-common part
forward_part="${target#$common_part}"
#echo "forward_part = \"$forward_part\""

if test -n "$result" && test -n "$forward_part" ; then
    #echo "(simple concat)"
    result="$result$forward_part"
elif test -n "$forward_part" ; then
    #echo "(concat with slash removal)"
    result="$(printf "%s" "$forward_part" | cut -c 1-)"
fi

printf "%s" "$result"
