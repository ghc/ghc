#!/bin/sh
version="$1"
if [ -z "$version" ]; then
    echo "usage: $0 [unicode version]"
    exit 1
fi
url=https://www.unicode.org/Public/$version/ucd/UnicodeData.txt
curl -L $url > UnicodeData.txt
sh ubconfc $1 < UnicodeData.txt > WCsubst.c

