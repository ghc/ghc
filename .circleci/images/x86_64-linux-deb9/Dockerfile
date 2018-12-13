FROM debian:stretch

ENV LANG C.UTF-8

RUN apt-get update -qq; apt-get install -qy gnupg

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main' > /etc/apt/sources.list.d/ghc.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286
RUN apt-get update -qq

# Core build utilities
RUN apt-get install -qy zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev \
    ca-certificates g++ git make automake autoconf gcc \
    perl python3 texinfo xz-utils lbzip2 patch openssh-client sudo \
    jq wget curl

# Documentation tools
RUN apt-get install -qy python3-sphinx texlive-xetex texlive-latex-extra

# Basic Haskell toolchain
RUN apt-get install -qy cabal-install-2.2 ghc-8.4.2

ENV PATH /home/ghc/.local/bin:/opt/cabal/2.2/bin:/opt/ghc/8.4.2/bin:$PATH

# LLVM
ENV LLVM_TARBALL http://releases.llvm.org/7.0.0/clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-16.04.tar.xz
ENV PATH /opt/llvm/bin:$PATH
RUN curl $LLVM_TARBALL | tar -xJC ..; \
    mkdir /opt/llvm; \
    cp -R clang+llvm*/* /opt/llvm; \
    rm -R clang+llvm*; \
    llc --version

# Create a normal user.
RUN adduser ghc --gecos "GHC builds" --disabled-password
RUN echo "ghc ALL = NOPASSWD : ALL" > /etc/sudoers.d/ghc
USER ghc

# Build Haskell tools
RUN cabal update && \
    cabal install hscolour happy alex
ENV PATH /home/ghc/.cabal/bin:$PATH

WORKDIR /home/ghc/

CMD ["bash"]
