FROM fedora:27

ENV LANG C.UTF-8

RUN dnf -y install coreutils binutils which git make automake autoconf gcc perl python3 texinfo xz lbzip2 patch openssh-clients sudo curl zlib-devel sqlite ncurses-compat-libs gmp-devel ncurses-devel gcc-c++ findutils

# Install GHC and cabal
RUN cd /tmp && curl https://downloads.haskell.org/~ghc/8.4.2/ghc-8.4.2-x86_64-deb8-linux.tar.xz | tar -Jx
RUN cd /tmp/ghc-8.4.2 && ./configure --prefix=/opt/ghc/8.4.2
RUN cd /tmp/ghc-8.4.2 && make install
RUN mkdir -p /opt/cabal/bin
RUN cd /opt/cabal/bin && curl https://www.haskell.org/cabal/release/cabal-install-2.2.0.0/cabal-install-2.2.0.0-x86_64-unknown-linux.tar.gz | tar -zx
ENV PATH /opt/ghc/8.4.2/bin:/opt/cabal/bin:$PATH

# Create a normal user.
RUN adduser ghc --comment "GHC builds"
RUN echo "ghc ALL = NOPASSWD : ALL" > /etc/sudoers.d/ghc
USER ghc
WORKDIR /home/ghc/

# Install Alex, Happy, and HsColor with Cabal
RUN cabal update
RUN cabal install alex happy hscolour
ENV PATH /home/ghc/.cabal/bin:$PATH

CMD ["bash"]
