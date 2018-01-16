#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/sh

# A script to bootstrap cabal-install.

# It works by downloading and installing the Cabal, zlib and
# HTTP packages. It then installs cabal-install itself.
# It expects to be run inside the cabal-install directory.

# Install settings, you can override these by setting environment vars. E.g. if
# you don't want profiling and dynamic versions of libraries to be installed in
# addition to vanilla, run 'EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh'

#VERBOSE
DEFAULT_CONFIGURE_OPTS="--enable-library-profiling --enable-shared"
EXTRA_CONFIGURE_OPTS=${EXTRA_CONFIGURE_OPTS-$DEFAULT_CONFIGURE_OPTS}
#EXTRA_BUILD_OPTS
#EXTRA_INSTALL_OPTS

die() {
    printf "\nError during cabal-install bootstrap:\n%s\n" "$1" >&2
    exit 2
}

# programs, you can override these by setting environment vars
GHC="${GHC:-ghc}"
GHC_PKG="${GHC_PKG:-ghc-pkg}"
GHC_VER="$(${GHC} --numeric-version)"
HADDOCK=${HADDOCK:-haddock}
WGET="${WGET:-wget}"
CURL="${CURL:-curl}"
FETCH="${FETCH:-fetch}"
TAR="${TAR:-tar}"
GZIP_PROGRAM="${GZIP_PROGRAM:-gzip}"

# The variable SCOPE_OF_INSTALLATION can be set on the command line to
# use/install the libaries needed to build cabal-install to a custom package
# database instead of the user or global package database.
#
# Example:
#
# $ ghc-pkg init /my/package/database
# $ SCOPE_OF_INSTALLATION='--package-db=/my/package/database' ./bootstrap.sh
#
# You can also combine SCOPE_OF_INSTALLATION with PREFIX:
#
# $ ghc-pkg init /my/prefix/packages.conf.d
# $ SCOPE_OF_INSTALLATION='--package-db=/my/prefix/packages.conf.d' \
#   PREFIX=/my/prefix ./bootstrap.sh
#
# If you use the --global,--user or --sandbox arguments, this will
# override the SCOPE_OF_INSTALLATION setting and not use the package
# database you pass in the SCOPE_OF_INSTALLATION variable.

SCOPE_OF_INSTALLATION="${SCOPE_OF_INSTALLATION:---user}"
DEFAULT_PREFIX="${HOME}/.cabal"

TMPDIR=$(mktemp -d -p /tmp -t cabal-XXXXXXX || mktemp -d -t cabal-XXXXXXX)
export TMPDIR

# Check for a C compiler, using user-set $CC, if any, first.
for c in $CC gcc clang cc icc; do
  $c --version 1>/dev/null 2>&1 && CC=$c &&
  echo "Using $c for C compiler. If this is not what you want, set CC." >&2 &&
  break
done

# None found.
[ -"$CC"- = -""- ] && die 'C compiler not found (or could not be run).
  If a C compiler is installed make sure it is on your PATH, or set $CC.'

# Find the correct linker/linker-wrapper.
#
# See https://github.com/haskell/cabal/pull/4187#issuecomment-269074153.
LINK="$(for link in collect2 ld; do
          if [ $($CC -print-prog-name=$link) = $link ]
          then
              continue
          else
              $CC -print-prog-name=$link && break
          fi
        done)"

# Fall back to "ld"... might work.
[ -$LINK- = -""- ] && LINK=ld

# And finally, see if we can compile and link something.
  echo 'int main(){}' | $CC -xc - -o /dev/null ||
    die "C compiler and linker could not compile a simple test program.
    Please check your toolchain."

# Warn that were's overriding $LD if set (if you want).
[ -"$LD"- != -""- ] && [ -"$LD"- != -"$LINK"- ] &&
  echo "Warning: value set in $LD is not the same as C compiler's $LINK." >&2
  echo "Using $LINK instead." >&2

# Set LD, overriding environment if necessary.
export LD=$LINK

# Check we're in the right directory, etc.
grep "cabal-install" ./cabal-install.cabal > /dev/null 2>&1 ||
  die "The bootstrap.sh script must be run in the cabal-install directory"

${GHC} --numeric-version > /dev/null 2>&1  ||
  die "${GHC} not found (or could not be run).
       If ghc is installed,  make sure it is on your PATH,
       or set the GHC and GHC_PKG vars."

${GHC_PKG} --version     > /dev/null 2>&1  || die "${GHC_PKG} not found."

GHC_PKG_VER="$(${GHC_PKG} --version | cut -d' ' -f 5)"

[ ${GHC_VER} = ${GHC_PKG_VER} ] ||
  die "Version mismatch between ${GHC} and ${GHC_PKG}.
       If you set the GHC variable then set GHC_PKG too."

JOBS="-j1"
while [ "$#" -gt 0 ]; do
  case "${1}" in
    "--user")
      SCOPE_OF_INSTALLATION="${1}"
      shift;;
    "--global")
      SCOPE_OF_INSTALLATION="${1}"
      DEFAULT_PREFIX="/usr/local"
      shift;;
    "--sandbox")
      shift
      # check if there is another argument which doesn't start with --
      if [ "$#" -le 0 ] || [ ! -z $(echo "${1}" | grep "^--") ]
      then
          SANDBOX=".cabal-sandbox"
      else
          SANDBOX="${1}"
          shift
      fi;;
    "--no-doc")
      NO_DOCUMENTATION=1
      shift;;
    "-j"|"--jobs")
        shift
        # check if there is another argument which doesn't start with - or --
        if [ "$#" -le 0 ] \
            || [ ! -z $(echo "${1}" | grep "^-") ] \
            || [ ! -z $(echo "${1}" | grep "^--") ]
        then
            JOBS="-j"
        else
            JOBS="-j${1}"
            shift
        fi;;
    *)
      echo "Unknown argument or option, quitting: ${1}"
      echo "usage: bootstrap.sh [OPTION]"
      echo
      echo "options:"
      echo "   -j/--jobs       Number of concurrent workers to use (Default: 1)"
      echo "                   -j without an argument will use all available cores"
      echo "   --user          Install for the local user (default)"
      echo "   --global        Install systemwide (must be run as root)"
      echo "   --no-doc        Do not generate documentation for installed"\
           "packages"
      echo "   --sandbox       Install to a sandbox in the default location"\
           "(.cabal-sandbox)"
      echo "   --sandbox path  Install to a sandbox located at path"
      exit;;
  esac
done

# Do not try to use -j with GHC 7.8 or older
case $GHC_VER in
    7.4*|7.6*|7.8*)
        JOBS=""
        ;;
    *)
        ;;
esac

abspath () { case "$1" in /*)printf "%s\n" "$1";; *)printf "%s\n" "$PWD/$1";;
             esac; }

if [ ! -z "$SANDBOX" ]
then # set up variables for sandbox bootstrap
  # Make the sandbox path absolute since it will be used from
  # different working directories when the dependency packages are
  # installed.
  SANDBOX=$(abspath "$SANDBOX")
  # Get the name of the package database which cabal sandbox would use.
  GHC_ARCH=$(ghc --info |
    sed -n 's/.*"Target platform".*"\([^-]\+\)-[^-]\+-\([^"]\+\)".*/\1-\2/p')
  PACKAGEDB="$SANDBOX/${GHC_ARCH}-ghc-${GHC_VER}-packages.conf.d"
  # Assume that if the directory is already there, it is already a
  # package database. We will get an error immediately below if it
  # isn't. Uses -r to try to be compatible with Solaris, and allow
  # symlinks as well as a normal dir/file.
  [ ! -r "$PACKAGEDB" ] && ghc-pkg init "$PACKAGEDB"
  PREFIX="$SANDBOX"
  SCOPE_OF_INSTALLATION="--package-db=$PACKAGEDB"
  echo Bootstrapping in sandbox at \'$SANDBOX\'.
fi

# Check for haddock unless no documentation should be generated.
if [ ! ${NO_DOCUMENTATION} ]
then
  ${HADDOCK} --version     > /dev/null 2>&1  || die "${HADDOCK} not found."
fi

PREFIX=${PREFIX:-${DEFAULT_PREFIX}}

# Versions of the packages to install.
# The version regex says what existing installed versions are ok.
PARSEC_VER="3.1.9";    PARSEC_VER_REGEXP="[3]\.[01]\."
                       # >= 3.0 && < 3.2
DEEPSEQ_VER="1.4.2.0"; DEEPSEQ_VER_REGEXP="1\.[1-9]\."
                       # >= 1.1 && < 2

case "$GHC_VER" in
    7.4*|7.6*)
        # GHC 7.4 or 7.6
        BINARY_VER="0.8.2.1"
        BINARY_VER_REGEXP="[0]\.[78]\.[0-2]\." # >= 0.7 && < 0.8.3
        ;;
    *)
        # GHC >= 7.8
        BINARY_VER="0.8.3.0"
        BINARY_VER_REGEXP="[0]\.[78]\." # >= 0.7 && < 0.9
        ;;
esac


TEXT_VER="1.2.2.1";    TEXT_VER_REGEXP="((1\.[012]\.)|(0\.([2-9]|(1[0-1]))\.))"
                       # >= 0.2 && < 1.3
NETWORK_VER="2.6.3.1"; NETWORK_VER_REGEXP="2\.[0-6]\."
                       # >= 2.0 && < 2.7
NETWORK_URI_VER="2.6.1.0"; NETWORK_URI_VER_REGEXP="2\.6\."
                       # >= 2.6 && < 2.7
CABAL_VER="2.1.0.0";  CABAL_VER_REGEXP="2\.1\.[0-9]"
                       # >= 2.1 && < 2.2
TRANS_VER="0.5.2.0";   TRANS_VER_REGEXP="0\.[45]\."
                       # >= 0.2.* && < 0.6
MTL_VER="2.2.1";       MTL_VER_REGEXP="[2]\."
                       #  >= 2.0 && < 3
HTTP_VER="4000.3.3";   HTTP_VER_REGEXP="4000\.(2\.([5-9]|1[0-9]|2[0-9])|3\.?)"
                       # >= 4000.2.5 < 4000.4
ZLIB_VER="0.6.1.2";    ZLIB_VER_REGEXP="(0\.5\.([3-9]|1[0-9])|0\.6)"
                       # >= 0.5.3 && <= 0.7
TIME_VER="1.7"         TIME_VER_REGEXP="1\.[1-7]\.?"
                       # >= 1.1 && < 1.8
RANDOM_VER="1.1"       RANDOM_VER_REGEXP="1\.[01]\.?"
                       # >= 1 && < 1.2
STM_VER="2.4.4.1";     STM_VER_REGEXP="2\."
                       # == 2.*
ASYNC_VER="2.1.0";     ASYNC_VER_REGEXP="2\."
                       # 2.*
OLD_TIME_VER="1.1.0.3"; OLD_TIME_VER_REGEXP="1\.[01]\.?"
                       # >=1.0.0.0 && <1.2
OLD_LOCALE_VER="1.0.0.7"; OLD_LOCALE_VER_REGEXP="1\.0\.?"
                       # >=1.0.0.0 && <1.1
BASE16_BYTESTRING_VER="0.1.1.6"; BASE16_BYTESTRING_VER_REGEXP="0\.1"
                       # 0.1.*
BASE64_BYTESTRING_VER="1.0.0.1"; BASE64_BYTESTRING_VER_REGEXP="1\."
                       # >=1.0
CRYPTOHASH_SHA256_VER="0.11.100.1"; CRYPTOHASH_SHA256_VER_REGEXP="0\.11\.?"
                       # 0.11.*
RESOLV_VER="0.1.1.0";  RESOLV_VER_REGEXP="0\.1\.[1-9]"
                       # >= 0.1.1 && < 0.2
MINTTY_VER="0.1";      MINTTY_VER_REGEXP="0\.1\.?"
                       # 0.1.*
ECHO_VER="0.1.3";      ECHO_VER_REGEXP="0\.1\.[3-9]"
                       # >= 0.1.3 && < 0.2
EDIT_DISTANCE_VER="0.2.2.1"; EDIT_DISTANCE_VER_REGEXP="0\.2\.2\.?"
                       # 0.2.2.*
ED25519_VER="0.0.5.0"; ED25519_VER_REGEXP="0\.0\.?"
                       # 0.0.*
HACKAGE_SECURITY_VER="0.5.2.2"; HACKAGE_SECURITY_VER_REGEXP="0\.5\.(2\.[2-9]|[3-9])"
                       # >= 0.5.2 && < 0.6
BYTESTRING_BUILDER_VER="0.10.8.1.0"; BYTESTRING_BUILDER_VER_REGEXP="0\.10\.?"
TAR_VER="0.5.0.3";     TAR_VER_REGEXP="0\.5\.([1-9]|1[0-9]|0\.[3-9]|0\.1[0-9])\.?"
                       # >= 0.5.0.3  && < 0.6
HASHABLE_VER="1.2.4.0"; HASHABLE_VER_REGEXP="1\."
                       # 1.*

HACKAGE_URL="https://hackage.haskell.org/package"

# Haddock fails for network-2.5.0.0, and for hackage-security for
# GHC <8, c.f. https://github.com/well-typed/hackage-security/issues/149
NO_DOCS_PACKAGES_VER_REGEXP="network-uri-2\.5\.[0-9]+\.[0-9]+|hackage-security-0\.5\.[0-9]+\.[0-9]+"

# Cache the list of packages:
echo "Checking installed packages for ghc-${GHC_VER}..."
${GHC_PKG} list --global ${SCOPE_OF_INSTALLATION} > ghc-pkg.list ||
  die "running '${GHC_PKG} list' failed"

# Will we need to install this package, or is a suitable version installed?
need_pkg () {
  PKG=$1
  VER_MATCH=$2
  if egrep " ${PKG}-${VER_MATCH}" ghc-pkg.list > /dev/null 2>&1
  then
    return 1;
  else
    return 0;
  fi
  #Note: we cannot use "! grep" here as Solaris 9 /bin/sh doesn't like it.
}

info_pkg () {
  PKG=$1
  VER=$2
  VER_MATCH=$3

  if need_pkg ${PKG} ${VER_MATCH}
  then
    if [ -r "${PKG}-${VER}.tar.gz" ]
    then
        echo "${PKG}-${VER} will be installed from local tarball."
    else
        echo "${PKG}-${VER} will be downloaded and installed."
    fi
  else
    echo "${PKG} is already installed and the version is ok."
  fi
}

fetch_pkg () {
  PKG=$1
  VER=$2

  URL_PKG=${HACKAGE_URL}/${PKG}-${VER}/${PKG}-${VER}.tar.gz
  URL_PKGDESC=${HACKAGE_URL}/${PKG}-${VER}/${PKG}.cabal
  if which ${CURL} > /dev/null
  then
    # TODO: switch back to resuming curl command once
    #       https://github.com/haskell/hackage-server/issues/111 is resolved
    #${CURL} -L --fail -C - -O ${URL_PKG} || die "Failed to download ${PKG}."
    ${CURL} -L --fail -O ${URL_PKG} || die "Failed to download ${PKG}."
    ${CURL} -L --fail -O ${URL_PKGDESC} \
        || die "Failed to download '${PKG}.cabal'."
  elif which ${WGET} > /dev/null
  then
    ${WGET} -c ${URL_PKG} || die "Failed to download ${PKG}."
    ${WGET} -c ${URL_PKGDESC} || die "Failed to download '${PKG}.cabal'."
  elif which ${FETCH} > /dev/null
    then
      ${FETCH} ${URL_PKG} || die "Failed to download ${PKG}."
      ${FETCH} ${URL_PKGDESC} || die "Failed to download '${PKG}.cabal'."
  else
    die "Failed to find a downloader. 'curl', 'wget' or 'fetch' is required."
  fi
  [ -f "${PKG}-${VER}.tar.gz" ] ||
     die "Downloading ${URL_PKG} did not create ${PKG}-${VER}.tar.gz"
  [ -f "${PKG}.cabal" ] ||
     die "Downloading ${URL_PKGDESC} did not create ${PKG}.cabal"
  mv "${PKG}.cabal" "${PKG}.cabal.hackage"
}

unpack_pkg () {
  PKG=$1
  VER=$2

  rm -rf "${PKG}-${VER}.tar" "${PKG}-${VER}"
  ${GZIP_PROGRAM} -d < "${PKG}-${VER}.tar.gz" | ${TAR} -xf -
  [ -d "${PKG}-${VER}" ] || die "Failed to unpack ${PKG}-${VER}.tar.gz"
  cp "${PKG}.cabal.hackage" "${PKG}-${VER}/${PKG}.cabal"
}

install_pkg () {
  PKG=$1
  VER=$2

  [ -x Setup ] && ./Setup clean
  [ -f Setup ] && rm Setup

  ${GHC} --make ${JOBS} Setup -o Setup -XRank2Types -XFlexibleContexts ||
    die "Compiling the Setup script failed."

  [ -x Setup ] || die "The Setup script does not exist or cannot be run"

  args="${SCOPE_OF_INSTALLATION} --prefix=${PREFIX} --with-compiler=${GHC}"
  args="$args --with-hc-pkg=${GHC_PKG} --with-gcc=${CC} --with-ld=${LD}"
  args="$args ${EXTRA_CONFIGURE_OPTS} ${VERBOSE}"

  ./Setup configure $args || die "Configuring the ${PKG} package failed."

  ./Setup build ${JOBS} ${EXTRA_BUILD_OPTS} ${VERBOSE} ||
     die "Building the ${PKG} package failed."

  if [ ! ${NO_DOCUMENTATION} ]
  then
    if echo "${PKG}-${VER}" | egrep ${NO_DOCS_PACKAGES_VER_REGEXP} \
        > /dev/null 2>&1
    then
      echo "Skipping documentation for the ${PKG} package."
    else
      ./Setup haddock --with-ghc=${GHC} --with-haddock=${HADDOCK} ${VERBOSE} ||
        die "Documenting the ${PKG} package failed."
    fi
  fi

  ./Setup install ${EXTRA_INSTALL_OPTS} ${VERBOSE} ||
     die "Installing the ${PKG} package failed."
}

do_pkg () {
  PKG=$1
  VER=$2
  VER_MATCH=$3

  if need_pkg ${PKG} ${VER_MATCH}
  then
    echo
    if [ -r "${PKG}-${VER}.tar.gz" ]
    then
        echo "Using local tarball for ${PKG}-${VER}."
    else
        echo "Downloading ${PKG}-${VER}..."
        fetch_pkg ${PKG} ${VER}
    fi
    unpack_pkg "${PKG}" "${VER}"
    (cd "${PKG}-${VER}" && install_pkg ${PKG} ${VER})
  fi
}

# If we're bootstrapping from a Git clone, install the local version of Cabal
# instead of downloading one from Hackage.
do_Cabal_pkg () {
    if [ -d "../.git" ]
    then
        if need_pkg "Cabal" ${CABAL_VER_REGEXP}
        then
            echo "Cabal-${CABAL_VER} will be installed from the local Git clone."
            (cd ../Cabal && install_pkg ${CABAL_VER} ${CABAL_VER_REGEXP})
        else
            echo "Cabal is already installed and the version is ok."
        fi
    else
        info_pkg "Cabal"        ${CABAL_VER}   ${CABAL_VER_REGEXP}
        do_pkg   "Cabal"        ${CABAL_VER}   ${CABAL_VER_REGEXP}
    fi
}

# Replicate the flag selection logic for network-uri in the .cabal file.
do_network_uri_pkg () {
  # Refresh installed package list.
  ${GHC_PKG} list --global ${SCOPE_OF_INSTALLATION} > ghc-pkg-stage2.list \
    || die "running '${GHC_PKG} list' failed"

  NETWORK_URI_DUMMY_VER="2.5.0.0"; NETWORK_URI_DUMMY_VER_REGEXP="2\.5\." # < 2.6
  if egrep " network-2\.[6-9]\." ghc-pkg-stage2.list > /dev/null 2>&1
  then
    # Use network >= 2.6 && network-uri >= 2.6
    info_pkg "network-uri" ${NETWORK_URI_VER} ${NETWORK_URI_VER_REGEXP}
    do_pkg   "network-uri" ${NETWORK_URI_VER} ${NETWORK_URI_VER_REGEXP}
  else
    # Use network < 2.6 && network-uri < 2.6
    info_pkg "network-uri" ${NETWORK_URI_DUMMY_VER} \
        ${NETWORK_URI_DUMMY_VER_REGEXP}
    do_pkg   "network-uri" ${NETWORK_URI_DUMMY_VER} \
        ${NETWORK_URI_DUMMY_VER_REGEXP}
  fi
}

# Conditionally install bytestring-builder if bytestring is < 0.10.2.
do_bytestring_builder_pkg () {
  if egrep "bytestring-0\.(9|10\.[0,1])\.?" ghc-pkg-stage2.list > /dev/null 2>&1
  then
      info_pkg "bytestring-builder" ${BYTESTRING_BUILDER_VER} \
               ${BYTESTRING_BUILDER_VER_REGEXP}
      do_pkg   "bytestring-builder" ${BYTESTRING_BUILDER_VER} \
               ${BYTESTRING_BUILDER_VER_REGEXP}
  fi
}

# Actually do something!

info_pkg "deepseq"      ${DEEPSEQ_VER} ${DEEPSEQ_VER_REGEXP}
info_pkg "binary"       ${BINARY_VER}  ${BINARY_VER_REGEXP}
info_pkg "time"         ${TIME_VER}    ${TIME_VER_REGEXP}
info_pkg "transformers" ${TRANS_VER}   ${TRANS_VER_REGEXP}
info_pkg "mtl"          ${MTL_VER}     ${MTL_VER_REGEXP}
info_pkg "text"         ${TEXT_VER}    ${TEXT_VER_REGEXP}
info_pkg "parsec"       ${PARSEC_VER}  ${PARSEC_VER_REGEXP}
info_pkg "network"      ${NETWORK_VER} ${NETWORK_VER_REGEXP}
info_pkg "old-locale"   ${OLD_LOCALE_VER} ${OLD_LOCALE_VER_REGEXP}
info_pkg "old-time"     ${OLD_TIME_VER}   ${OLD_TIME_VER_REGEXP}
info_pkg "HTTP"         ${HTTP_VER}    ${HTTP_VER_REGEXP}
info_pkg "zlib"         ${ZLIB_VER}    ${ZLIB_VER_REGEXP}
info_pkg "random"       ${RANDOM_VER}  ${RANDOM_VER_REGEXP}
info_pkg "stm"          ${STM_VER}     ${STM_VER_REGEXP}
info_pkg "async"        ${ASYNC_VER}   ${ASYNC_VER_REGEXP}
info_pkg "base16-bytestring" ${BASE16_BYTESTRING_VER} \
    ${BASE16_BYTESTRING_VER_REGEXP}
info_pkg "base64-bytestring" ${BASE64_BYTESTRING_VER} \
    ${BASE64_BYTESTRING_VER_REGEXP}
info_pkg "cryptohash-sha256" ${CRYPTOHASH_SHA256_VER} \
    ${CRYPTOHASH_SHA256_VER_REGEXP}
info_pkg "resolv"        ${RESOLV_VER}        ${RESOLV_VER_REGEXP}
info_pkg "mintty"        ${MINTTY_VER}        ${MINTTY_VER_REGEXP}
info_pkg "echo"          ${ECHO_VER}          ${ECHO_VER_REGEXP}
info_pkg "edit-distance" ${EDIT_DISTANCE_VER} ${EDIT_DISTANCE_VER_REGEXP}
info_pkg "ed25519"           ${ED25519_VER}          ${ED25519_VER_REGEXP}
info_pkg "tar"               ${TAR_VER}              ${TAR_VER_REGEXP}
info_pkg "hashable"          ${HASHABLE_VER}         ${HASHABLE_VER_REGEXP}
info_pkg "hackage-security"  ${HACKAGE_SECURITY_VER} \
    ${HACKAGE_SECURITY_VER_REGEXP}

do_pkg   "deepseq"      ${DEEPSEQ_VER} ${DEEPSEQ_VER_REGEXP}
do_pkg   "binary"       ${BINARY_VER}  ${BINARY_VER_REGEXP}
do_pkg   "time"         ${TIME_VER}    ${TIME_VER_REGEXP}

# Cabal might depend on these
do_pkg   "transformers" ${TRANS_VER}   ${TRANS_VER_REGEXP}
do_pkg   "mtl"          ${MTL_VER}     ${MTL_VER_REGEXP}
do_pkg   "text"         ${TEXT_VER}    ${TEXT_VER_REGEXP}
do_pkg   "parsec"       ${PARSEC_VER}  ${PARSEC_VER_REGEXP}

# Install the Cabal library from the local Git clone if possible.
do_Cabal_pkg

do_pkg   "network"      ${NETWORK_VER} ${NETWORK_VER_REGEXP}

# We conditionally install network-uri, depending on the network version.
do_network_uri_pkg

do_pkg   "old-locale"   ${OLD_LOCALE_VER} ${OLD_LOCALE_VER_REGEXP}
do_pkg   "old-time"     ${OLD_TIME_VER}   ${OLD_TIME_VER_REGEXP}
do_pkg   "HTTP"         ${HTTP_VER}       ${HTTP_VER_REGEXP}
do_pkg   "zlib"         ${ZLIB_VER}       ${ZLIB_VER_REGEXP}
do_pkg   "random"       ${RANDOM_VER}     ${RANDOM_VER_REGEXP}
do_pkg   "stm"          ${STM_VER}        ${STM_VER_REGEXP}
do_pkg   "async"        ${ASYNC_VER}      ${ASYNC_VER_REGEXP}
do_pkg   "base16-bytestring" ${BASE16_BYTESTRING_VER} \
    ${BASE16_BYTESTRING_VER_REGEXP}
do_pkg   "base64-bytestring" ${BASE64_BYTESTRING_VER} \
    ${BASE64_BYTESTRING_VER_REGEXP}
do_pkg   "cryptohash-sha256" ${CRYPTOHASH_SHA256_VER} \
    ${CRYPTOHASH_SHA256_VER_REGEXP}
do_pkg "resolv"        ${RESOLV_VER}        ${RESOLV_VER_REGEXP}
do_pkg "mintty"        ${MINTTY_VER}        ${MINTTY_VER_REGEXP}
do_pkg "echo"          ${ECHO_VER}          ${ECHO_VER_REGEXP}
do_pkg "edit-distance" ${EDIT_DISTANCE_VER} ${EDIT_DISTANCE_VER_REGEXP}
do_pkg   "ed25519"           ${ED25519_VER}          ${ED25519_VER_REGEXP}

# We conditionally install bytestring-builder, depending on the bytestring
# version.
do_bytestring_builder_pkg

do_pkg   "tar"               ${TAR_VER}              ${TAR_VER_REGEXP}
do_pkg   "hashable"          ${HASHABLE_VER}         ${HASHABLE_VER_REGEXP}
do_pkg   "hackage-security"  ${HACKAGE_SECURITY_VER} \
    ${HACKAGE_SECURITY_VER_REGEXP}


install_pkg "cabal-install"

# Use the newly built cabal to turn the prefix/package database into a
# legit cabal sandbox. This works because 'cabal sandbox init' will
# reuse the already existing package database and other files if they
# are in the expected locations.
[ ! -z "$SANDBOX" ] && $SANDBOX/bin/cabal sandbox init --sandbox $SANDBOX

echo
echo "==========================================="
CABAL_BIN="$PREFIX/bin"
if [ -x "$CABAL_BIN/cabal" ]
then
    echo "The 'cabal' program has been installed in $CABAL_BIN/"
    echo "You should either add $CABAL_BIN to your PATH"
    echo "or copy the cabal program to a directory that is on your PATH."
    echo
    echo "The first thing to do is to get the latest list of packages with:"
    echo "  cabal update"
    echo "This will also create a default config file (if it does not already"
    echo "exist) at $HOME/.cabal/config"
    echo
    echo "By default cabal will install programs to $HOME/.cabal/bin"
    echo "If you do not want to add this directory to your PATH then you can"
    echo "change the setting in the config file, for example you could use:"
    echo "symlink-bindir: $HOME/bin"
else
    echo "Sorry, something went wrong."
    echo "The 'cabal' executable was not successfully installed into"
    echo "$CABAL_BIN/"
fi
echo

rm ghc-pkg.list
