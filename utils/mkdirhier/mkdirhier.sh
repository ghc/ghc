#!/bin/sh

#
# create a hierarchy of directories
#
# Based on Noah Friedman's mkinstalldirs..
#

quiet=no
errs=0

if [ "$1" = "-q" ]
then
    shift
    quiet=yes
fi

for f in $*; do
    parts=`echo ":$f" | sed -ne 's/^:\//#/;s/^://;s/\// /g;s/^#/\//;p'`
    path="";
    for p in $parts; do
        path="$path$p"
        case "$path" in
          -* ) path=./$path ;;
        esac

        if test ! -d "$path"; then
           if [ "$quiet" = "no" ]
           then
               echo "mkdir $path" 1>&2
           fi

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
