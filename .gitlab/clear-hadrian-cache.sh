#!/usr/bin/bash

set -e

# Build hadrian
cd hadrian
cabal new-build hadrian
cd ..
hadrian=$(find hadrian/dist-newstyle -iname hadrian -executable -type f)

# Did the executable change?
touch hadrian.sha256
sha256 $hadrian > hadrian.sha256.new
if diff -q hadrian.sha256 hadrian.sha256.new; then
    echo "Hadrian has changed. Deleting cache... "
    mv hadrian.sha256.new hadrian.sha256
    rm -R _cache
fi

