# Configure the environment
MSYSTEM=MINGW64
source /etc/profile || true # a terrible, terrible workaround for msys2 brokenness

# Don't set -e until after /etc/profile is sourced
set -ex
cd $APPVEYOR_BUILD_FOLDER

case "$1" in
    "prepare")
        # Bring msys up-to-date
        # However, we current don't do this: generally one must restart all
        # msys2 processes when updating the msys2 runtime, which this may do. We can't
        # easily do this and therefore do simply don't update.
        #pacman --noconfirm -Syuu

        # Install basic build dependencies
        pacman --noconfirm -S --needed git tar bsdtar binutils autoconf make xz curl libtool automake python python2 p7zip patch mingw-w64-$(uname -m)-python3-sphinx mingw-w64-$(uname -m)-tools-git

        # Prepare the tree
        git config remote.origin.url git://github.com/ghc/ghc.git
        git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/
        git submodule init
        git submodule --quiet update --recursive

        # Install build dependencies
        wget -q -O - https://downloads.haskell.org/~ghc/8.2.1/ghc-8.2.1-x86_64-unknown-mingw32.tar.xz | tar -xJ -C /mingw64 --strip-components=1
        mkdir -p /usr/local/bin
        wget -q -O - https://www.haskell.org/cabal/release/cabal-install-1.24.0.0/cabal-install-1.24.0.0-x86_64-unknown-mingw32.zip | bsdtar -xzf- -C /usr/local/bin
        cabal update
        cabal install -j --prefix=/usr/local alex happy
        ;;

    "build")
        # Build the compiler
        ./boot
        ./configure --enable-tarballs-autodownload
        make -j2
        ;;

    "test")
        make binary_dist
        7z a ghc-windows.zip *.tar.xz
        ;;

    *)
        echo "$0: unknown mode $1"
        exit 1
        ;;
esac
