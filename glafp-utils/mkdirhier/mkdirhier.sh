#!/bin/sh

#
# create a heirarchy of directories
#

for f in $*; do
    parts=`echo $f | sed 's,\(.\)/\(.\),\1 \2,g' | sed 's,/$,,'`;
    path="";
    for p in $parts; do
	if [ x"$path" = x ]; then
	    dir=$p;
	else
	    dir=$path/$p;
	fi;
	if [ ! -d $dir ]; then
	    echo mkdir $dir; 
	    mkdir $dir;
	    chmod a+rx $dir; 
	fi;
	path=$dir;
    done;
done

