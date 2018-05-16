FROM debian:jessie

ENV LANG C.UTF-8

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286
RUN apt-get update -qq
RUN apt-get install -qy cabal-install-2.2 ghc-8.4.2 happy alex zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev ca-certificates g++ git curl git make automake autoconf gcc perl python3 texinfo xz-utils lbzip2 patch openssh-client sudo

# Stack intallation
RUN curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.6.5/stack-1.6.5-linux-x86_64-static.tar.gz -o stack.tar.gz
RUN curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.6.5/stack-1.6.5-linux-x86_64-static.tar.gz.asc -o stack.tar.gz.asc
RUN export GNUPGHOME="$(mktemp -d)"
RUN gpg --keyserver ha.pool.sks-keyservers.net --recv-keys C5705533DA4F78D8664B5DC0575159689BEFB442
RUN gpg --batch --verify stack.tar.gz.asc stack.tar.gz
RUN tar -xf stack.tar.gz -C /usr/local/bin --strip-components=1
RUN /usr/local/bin/stack config set system-ghc --global true
RUN rm -rf "$GNUPGHOME" /var/lib/apt/lists/* /stack.tar.gz.asc /stack.tar.gz

ENV PATH /home/ghc/.cabal/bin:/home/ghc/.local/bin:/opt/cabal/2.2/bin:/opt/ghc/8.4.2/bin:$PATH

# Create a normal user.
RUN adduser ghc --gecos "GHC builds" --disabled-password
RUN echo "ghc ALL = NOPASSWD : ALL" > /etc/sudoers.d/ghc
USER ghc

WORKDIR /home/ghc/

CMD ["bash"]
