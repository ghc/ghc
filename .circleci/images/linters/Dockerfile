FROM debian:stretch

ENV LANG C.UTF-8

RUN apt-get update -qq; apt-get install -qy gnupg sudo git python3

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main' > /etc/apt/sources.list.d/ghc.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286
RUN apt-get update -qq

# Basic Haskell toolchain
RUN apt-get install -qy cabal-install-2.2 ghc-8.4.2
ENV PATH /home/ghc/.local/bin:/opt/cabal/2.2/bin:/opt/ghc/8.4.2/bin:$PATH

# Create a normal user.
RUN adduser ghc --gecos "GHC builds" --disabled-password
RUN echo "ghc ALL = NOPASSWD : ALL" > /etc/sudoers.d/ghc
USER ghc
WORKDIR /home/ghc/

# Build Linting tools
RUN cabal update

RUN git clone git://github.com/haskell-infra/git-haskell-org-hooks && \
    cd git-haskell-org-hooks && \
    cabal install

ENV PATH /home/ghc/.cabal/bin:$PATH

CMD ["bash"]
