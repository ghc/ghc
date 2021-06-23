#!/bin/sh

GHC=$TEST_HC

flags="$TEST_HC_OPTS -O0 -fhide-source-paths -isrc -ibuild -dynamic-too -outputdir build -package template-haskell"

function compile()
{
    echo $@
    $GHC -c $flags $@ || exit 1
}

function setup()
{
    echo
    echo "Preparing everything ..."
    rm -rf build
    mkdir -p build
    cp src/Old_Hospital.hs src/Hospital.hs
    compile src/Hospital.hs
    compile src/Types.hs
    compile src/MetaHandler.hs
    echo "Done with preparations"
}


echo
echo "Preparing everyting with --make ..."
echo
rm -rf build
mkdir -p build
cp src/Old_Hospital.hs src/Hospital.hs
$GHC --make $flags src/MetaHandler.hs
echo "Done with preparations with --make"
echo
echo "Building with --make"
echo
cp src/New_Hospital.hs src/Hospital.hs
$GHC --make $flags src/MetaHandler.hs

setup
echo "Building file-by-file, with -fforce-recomp"
echo
cp src/New_Hospital.hs src/Hospital.hs
compile src/ShortText.hs
compile src/Hospital.hs
compile -fforce-recomp src/Types.hs
compile src/MetaHandler.hs

setup
echo
echo "Building file-by-file"
echo
cp src/New_Hospital.hs src/Hospital.hs
compile src/ShortText.hs
compile src/Hospital.hs
compile src/Types.hs
compile src/MetaHandler.hs
