#!/usr/bin/env bash

set -ex

ARCH=$1
BINUTILS=2.25.1
GCC=6.4.0
FREEBSD=10.4-RELEASE

mkdir binutils
cd binutils

# Build binutils for target platform.
curl https://ftp.gnu.org/gnu/binutils/binutils-$BINUTILS.tar.bz2 | tar xjf -

mkdir binutils-build
cd binutils-build
../binutils-$BINUTILS/configure --target=$ARCH-unknown-freebsd10
make -j$(nproc)
make install
cd ..

cd ..
rm -rf binutils

mkdir freebsd
case "$ARCH" in
    x86_64)
	FREEBSD_ARCH=amd64
    ;;
    i686)
	FREEBSD_ARCH=i386
    ;;
esac
    
URL=ftp://ftp.freebsd.org/pub/FreeBSD/releases/$FREEBSD_ARCH/$FREEBSD/base.txz
curl $URL | tar xJf - -C freebsd ./usr/include ./usr/lib ./lib

dst=/usr/local/$ARCH-unknown-freebsd10

cp -r freebsd/usr/include $dst/
cp freebsd/usr/lib/crt1.o $dst/lib
cp freebsd/usr/lib/Scrt1.o $dst/lib
cp freebsd/usr/lib/crti.o $dst/lib
cp freebsd/usr/lib/crtn.o $dst/lib
cp freebsd/usr/lib/libc.a $dst/lib
cp freebsd/usr/lib/libutil.a $dst/lib
cp freebsd/usr/lib/libutil_p.a $dst/lib
cp freebsd/usr/lib/libm.a $dst/lib
cp freebsd/usr/lib/librt.so.1 $dst/lib
cp freebsd/usr/lib/libexecinfo.so.1 $dst/lib
cp freebsd/lib/libc.so.7 $dst/lib
cp freebsd/lib/libm.so.5 $dst/lib
cp freebsd/lib/libutil.so.9 $dst/lib
cp freebsd/lib/libthr.so.3 $dst/lib
cp freebsd/lib/libncurses.so.8 $dst/lib
cp freebsd/lib/libncursesw.so.8 $dst/lib

# Install iconv port in target env.
URL_ICONV=http://pkg.freebsd.org/FreeBSD:11:$FREEBSD_ARCH/latest/All/libiconv-1.14_11.txz
curl $URL_ICONV | tar xJf - -C freebsd
cp -r freebsd/usr/local/include $dst/
cp -d freebsd/usr/local/lib/* $dst/lib

ln -s libc.so.7 $dst/lib/libc.so
ln -s libm.so.5 $dst/lib/libm.so
ln -s librt.so.1 $dst/lib/librt.so
ln -s libutil.so.9 $dst/lib/libutil.so
ln -s libexecinfo.so.1 $dst/lib/libexecinfo.so
ln -s libthr.so.3 $dst/lib/libpthread.so
ln -s libncurses.so.8 $dst/lib/libncurses.so
ln -s libncursesw.so.8 $dst/lib/libncursesw.so
rm -rf freebsd

# Build gcc for target platform.
mkdir gcc
cd gcc
curl https://ftp.gnu.org/gnu/gcc/gcc-$GCC/gcc-$GCC.tar.gz | tar xzf -
cd gcc-$GCC
./contrib/download_prerequisites

mkdir ../gcc-build
cd ../gcc-build
../gcc-$GCC/configure \
    --enable-languages=c \
    --target=$ARCH-unknown-freebsd10 \
    --disable-nls \
    --disable-libgomp \
    --disable-libquadmath \
    --disable-libssp \
    --disable-libvtv \
    --disable-libcilkrts \
    --disable-libada \
    --disable-libsanitizer \
    --disable-libquadmath-support \
    --disable-lto
make -j$(nproc)
make install
cd ..

cd ..
rm -rf gcc
