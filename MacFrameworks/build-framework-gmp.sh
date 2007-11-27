#!/bin/sh
# This is the shell script used to create this framework
# from the sources available from the GMP web page at
# http://www.swox.com/gmp/
# To build your own, copy this file next to a GMP source tree,
# update the SrcDir variable if necessary and execute
# sh build-framework.sh

SrcDir=gmp-4.2.1
FrameworkName=GMP
FrameworkVersion=A
LibraryName=libgmp.dylib
ExtraThings="$SrcDir/StagingAreaPPC/info $SrcDir/README $SrcDir/COPYING.LIB $SrcDir/ChangeLog build-framework.sh"

pushd $SrcDir || exit 1

make distclean
CFLAGS='-arch ppc' ./configure --enable-cxx --host=powerpc-apple-darwin --disable-static --enable-shared --prefix=`pwd`/StagingAreaPPC || exit 1
make CCLD='gcc -Wc,-arch -Wc,ppc' || exit 1
make install || exit 1

make distclean
CFLAGS='-arch i386' ./configure --enable-cxx --host=none-apple-darwin --disable-static --enable-shared --prefix=`pwd`/StagingAreaIntel || exit 1
make CCLD='gcc -Wc,-arch -Wc,i386' || exit 1
make install || exit 1

popd


rm -rf $FrameworkName.framework

FWVDir=$FrameworkName.framework/Versions/$FrameworkVersion
mkdir -p $FWVDir

cp -R $SrcDir/StagingAreaPPC/include $FWVDir/Headers
# cp $SrcDir/StagingArea/lib/$LibraryName $FWVDir/$FrameworkName
lipo -arch ppc $SrcDir/StagingAreaPPC/lib/$LibraryName -arch i386 $SrcDir/StagingAreaIntel/lib/$LibraryName -create -output $FWVDir/$FrameworkName

install_name_tool -id $FWVDir/$FrameworkName $FWVDir/$FrameworkName

ln -sf Versions/$FrameworkVersion/$FrameworkName $FrameworkName.framework/$FrameworkName
ln -sf Versions/$FrameworkVersion/Headers $FrameworkName.framework/Headers

for i in $ExtraThings; do
    cp -R $i $FrameworkName.framework/
done

echo "Framework $FrameworkName.framework created."
