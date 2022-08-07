#!/bin/sh

# $1 = executable name
# $2 = wrapper path
# $3 = bindir
# $4 = ghcbindir
# $5 = Executable binary path
# $6 = Library Directory
# $7 = Docs Directory
# $8 = Includes Directory
# We are installing wrappers to programs by searching corresponding
# wrappers. If wrapper is not found, we are attaching the common wrapper
# to it. This implementation is a bit hacky and depends on consistency
# of program names. For hadrian build this will work as programs have a
# consistent naming procedure.

echo "Installing $1 -> $2"
if [ -L "wrappers/$1" ]; then
    cp -RP "wrappers/$1" "$2"
else
    rm -f "$2" &&
    touch "$2" &&
    echo "#!$SHELL" >> "$2"  &&
    echo "exedir=\"$4\"" >> "$2"  &&
    echo "exeprog=\"$1\"" >> "$2"  &&
    echo "executablename=\"$5\"" >> "$2"  &&
    echo "bindir=\"$3\"" >> "$2"  &&
    echo "libdir=\"$6\"" >> "$2"  &&
    echo "docdir=\"$7\"" >> "$2"  &&
    echo "includedir=\"$8\"" >> "$2"  &&
    echo "" >> "$2"  &&
    cat "wrappers/$1" >> "$2"  &&
    chmod 755 "$2"
fi
