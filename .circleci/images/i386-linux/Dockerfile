FROM i386/debian:jessie

ENV LANG C.UTF-8

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286
RUN apt-get update -qq
RUN apt-get install -qy git make automake autoconf gcc perl python3 texinfo xz-utils lbzip2 bzip2 patch openssh-client sudo curl zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev ca-certificates g++ cabal-install-2.2 ghc-8.4.2 happy alex
ENV PATH /home/ghc/.cabal/bin:/home/ghc/.local/bin:/opt/cabal/2.2/bin:/opt/ghc/8.4.2/bin:$PATH

# Get i386 GHC bindist for 32 bit CI builds.
RUN cd /tmp && curl https://downloads.haskell.org/~ghc/8.4.2/ghc-8.4.2-i386-deb8-linux.tar.xz | tar -Jx
RUN cd /tmp/ghc-8.4.2 && setarch i386 ./configure --prefix=/opt/ghc-i386/8.4.2 CFLAGS=-m32 --target=i386-unknown-linux --build=i386-unknown-linux --host=i386-unknown-linux
RUN cd /tmp/ghc-8.4.2 && make install
RUN rm -rf /tmp/ghc-8.4.2
ENV PATH /opt/ghc-i386/8.4.2/bin:$PATH

# Create a normal user.
RUN adduser ghc --gecos "GHC builds" --disabled-password
RUN echo "ghc ALL = NOPASSWD : ALL" > /etc/sudoers.d/ghc
USER ghc

WORKDIR /home/ghc/

CMD ["bash"]
