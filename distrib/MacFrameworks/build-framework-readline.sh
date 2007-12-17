
#!/bin/sh
# This is the shell script used to create this framework
# from the sources available from the bash site at
# ftp://ftp.cwru.edu/pub/bash/readline-5.2.tar.gz
# To build your own, copy this script and the above gzip file 
# into an empty directory, and run the following commands:
# 
# tar -xzf readline-5.2.tar.gz
# sh build-framework.sh

ONLY_COPY=0

SrcDir=readline-5.2
FrameworkName=GNUreadline
FrameworkVersion=A
LibraryName=libreadline.dylib
ExtraThings="$SrcDir/StagingArea/info $SrcDir/README $SrcDir/COPYING $SrcDir/CHANGELOG build-framework-readline.sh readline-5.2.tar.gz"

pushd $SrcDir || exit 1

# for getting cross compiled universal binaries running on ppc and i386
CFLAGS="-isysroot /Developer/SDKs/MacOSX10.4u.sdk -arch i386 -arch ppc"
LDFLAGS="-Wl,-syslibroot,/Developer/SDKs/MacOSX10.4u.sdk -arch i386 -arch ppc"
export CFLAGS LDFLAGS

./configure --disable-dependency-tracking --disable-static --enable-shared --prefix=`pwd`/StagingArea || exit 1
make || exit 1
make doc || exit 1
make install || exit 1

popd

rm -rf $FrameworkName.framework

FWVDir=$FrameworkName.framework/Versions/$FrameworkVersion
mkdir -p $FWVDir

# Copy the header files into our new framework.
# Change declarations of #include <readline/*.h> to <GNUreadline/readline/*.h>, so that they reference the 
# header files inside of this framework.
OLDINCLUDEDIR=$SrcDir/StagingArea/include/readline
NEWINCLUDEDIR=$FWVDir/Headers/readline
mkdir -p $NEWINCLUDEDIR
for i in `ls $OLDINCLUDEDIR`; do
    sed 's/include <readline/include <GNUreadline\/readline/' $OLDINCLUDEDIR/$i > $NEWINCLUDEDIR/$i
done
cp $SrcDir/StagingArea/lib/$LibraryName $FWVDir/$FrameworkName

chmod u+w $FWVDir/$FrameworkName

install_name_tool -id $FWVDir/$FrameworkName $FWVDir/$FrameworkName

ln -sf Versions/$FrameworkVersion/$FrameworkName $FrameworkName.framework/$FrameworkName
ln -sf Versions/$FrameworkVersion/Headers $FrameworkName.framework/Headers

for i in $ExtraThings; do
    cp -R $i $FrameworkName.framework/
done

echo "Framework $FrameworkName.framework created."
zip -qyr "$FrameworkName-framework.zip" "$FrameworkName.framework"
echo "... and zipped as $FrameworkName-framework.zip"
