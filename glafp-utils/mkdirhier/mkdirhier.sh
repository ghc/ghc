#!/bin/sh

#
# create a hierarchy of directories
#
# Based on Noah Friedman's mkinstalldirs..
#
errs=0

for f in $*; do
    parts=`echo ":$f" | sed -ne 's/^:\//#/;s/^://;s/\// /g;s/^#/\//;p'`
    path="";
    for p in $parts; do
        path="$path$p"
        case "$path" in
          -* ) path=./$path ;;
        esac

        if test ! -d "$path"; then
           echo "mkdir $path" 1>&2

           mkdir "$path" || lasterr=$?
	   
	   if test ! -d "$path"; then
	      errs=$lasterr
           fi 
        fi
	path="$path/";
    done;
done

exit $errs

# end of story
