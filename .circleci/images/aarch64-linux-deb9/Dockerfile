FROM aarch64/debian:stretch

ENV LANG C.UTF-8

# Core build utilities
RUN apt-get update -qq
RUN apt-get install -qy zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev \
    ca-certificates g++ git make automake autoconf gcc \
    perl python3 texinfo xz-utils lbzip2 patch openssh-client sudo \
    jq wget curl libnuma-dev

# Documentation tools
RUN apt-get install -qy python3-sphinx texlive-xetex texlive-latex-extra

# Boot LLVM
ENV PATH /usr/local/bin:$PATH
ENV LLVM_TARBALL http://releases.llvm.org/6.0.0/clang+llvm-6.0.0-aarch64-linux-gnu.tar.xz
RUN curl $LLVM_TARBALL | tar -xJC .. && \
    mkdir /opt/llvm6 && \
    cp -R clang+llvm*/* /opt/llvm6 && \
    rm -R clang+llvm* && \
    /opt/llvm6/bin/llc --version

# GHC
#RUN curl http://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-aarch64-deb8-linux.tar.xz | tar -xJ && \
COPY ghc-8.6.2-aarch64-unknown-linux.tar.xz .
RUN cat ghc-8.6.2-aarch64-unknown-linux.tar.xz | tar -xJ && \
    cd ghc-8.6.2 && \
    ./configure --prefix=/usr/local LLC=/opt/llvm6/bin/llc OPT=/opt/llvm6/bin/opt && \
    make install && \
    cd .. && \
    rm -Rf ghc-*
RUN ghc --version

# LLVM
ENV LLVM_TARBALL http://releases.llvm.org/7.0.0/clang+llvm-7.0.0-aarch64-linux-gnu.tar.xz
ENV LLC /opt/llvm7/bin/llc
ENV OPT /opt/llvm7/bin/opt
RUN curl $LLVM_TARBALL | tar -xJC .. && \
    mkdir /opt/llvm7 && \
    cp -R clang+llvm*/* /opt/llvm7 && \
    rm -R clang+llvm* && \
    $LLC --version

# Cabal
RUN git clone https://github.com/haskell/Cabal && \
    cd Cabal && \
    git checkout cabal-install-v2.4.1.0 && \
    cd cabal-install && \
    ./bootstrap.sh --global --no-doc

RUN ls Cabal/cabal-install/dist/build

ENV PATH /home/ghc/.local/bin:/opt/cabal/2.2/bin:/opt/ghc/8.4.2/bin:$PATH

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

