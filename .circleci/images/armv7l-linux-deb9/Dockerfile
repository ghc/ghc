FROM arm32v7/debian:stretch

ENV LANG C.UTF-8

# Core build utilities
RUN apt-get update -qq
RUN apt-get install -qy zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev \
    ca-certificates g++ git make automake autoconf gcc \
    perl python3 texinfo xz-utils lbzip2 patch openssh-client sudo \
    jq wget curl libgmp-dev

# Documentation tools
# RUN apt-get install -qy python3-sphinx texlive-xetex texlive-latex-extra

# Boot LLVM

# Using a local one I fixed the user and owner in because Docker can't
# deal with the userid not being mapped on my aystem (and its chown
# doesn't occur until after it fails to extract).
ADD clang-6.0.1.tar.xz /opt/
ENV PATH /usr/local/bin:/usr/local/clang-6.0.1/bin:$PATH
ENV LLC /opt/clang-6.0.1/bin/llc
ENV OPT /opt/clang-6.0.1/bin/opt

# ENV PATH /usr/local/bin:$PATH
# ENV LLVM_TARBALL http://releases.llvm.org/6.0.1/clang+llvm-6.0.1-armv7a-linux-gnueabihf.tar.xz
# RUN curl $LLVM_TARBALL | tar -xJC .. && \
#     mkdir /opt/llvm6 && \
#     cp -R clang+llvm*/* /opt/llvm6 && \
#     rm -R clang+llvm* && \
#     /opt/llvm6/bin/llc --version

# GHC
#RUN curl http://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-aarch64-deb8-linux.tar.xz | tar -xJ && \
ADD ghc-8.2.2-armv7-deb8-linux.tar.xz /tmp
RUN (cd /tmp/ghc-8.2.2 && ./configure --prefix=/usr/local/ghc-8.2.2 && make install && rm -rf /tmp/ghc-8.2.2)
ENV PATH /usr/local/ghc-8.2.2/bin:$PATH
RUN ghc --version

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

