FROM debian:jessie

ENV LANG C.UTF-8

# Core build utilities
RUN apt-get update -qq
RUN apt-get install -qy zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev \
    ca-certificates g++ git make automake autoconf gcc \
    perl python3 texinfo xz-utils lbzip2 patch openssh-client sudo libgmp-dev \
    jq wget curl

# Documentation tools
RUN apt-get install -qy python3-sphinx texlive-xetex texlive-latex-extra

# Install GHC and cabal
RUN cd /tmp && curl https://downloads.haskell.org/~ghc/8.4.4/ghc-8.4.4-x86_64-deb8-linux.tar.xz | tar -Jx; \
    cd /tmp/ghc-8.4.4 && ./configure --prefix=/opt/ghc/8.4.4; \
    cd /tmp/ghc-8.4.4 && make install; \
    rm -rf /tmp/ghc-8.4.4
ENV PATH /opt/ghc/8.4.4/bin:$PATH

# Get Cabal
RUN cd /tmp && \
    curl https://downloads.haskell.org/cabal/cabal-install-2.4.1.0/cabal-install-2.4.1.0-x86_64-unknown-linux.tar.xz | tar -Jx && \
    mv cabal /usr/local/bin/cabal

# Create a normal user.
RUN adduser ghc --gecos "GHC builds" --disabled-password
RUN echo "ghc ALL = NOPASSWD : ALL" > /etc/sudoers.d/ghc
USER ghc
WORKDIR /home/ghc/

# Build Haskell tools
RUN cabal v2-update && \
    cabal v2-install hscolour happy alex
ENV PATH /home/ghc/.cabal/bin:$PATH

CMD ["bash"]
