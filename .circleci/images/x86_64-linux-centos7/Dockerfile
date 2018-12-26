FROM centos:7

ENV LANG C.UTF-8

# Core build utilities
RUN yum -y install coreutils binutils which git make \
    automake autoconf gcc perl python3 texinfo xz lbzip2 \
    patch openssh-clients sudo zlib-devel sqlite \
    ncurses-compat-libs gmp-devel ncurses-devel gcc-c++ findutils \
    curl wget jq

# Python 3
RUN yum install -y https://centos7.iuscommunity.org/ius-release.rpm; \
    yum install -y python34u python34u-libs

# Documentation tools
RUN yum -y install python-sphinx \
    texlive texlive-latex texlive-xetex \
    texlive-collection-latex texlive-collection-latexrecommended \
    texlive-xetex-def texlive-collection-xetex \
    python-sphinx-latex dejavu-sans-fonts dejavu-serif-fonts \
    dejavu-sans-mono-fonts

# This is in the PATH when I ssh into the CircleCI machine but somehow
# sphinx-build isn't found during configure unless we explicitly 
# add it here as well; perhaps PATH is being overridden by CircleCI's
# infrastructure?
ENV PATH /usr/libexec/python3-sphinx:$PATH

# systemd isn't running so remove it from nsswitch.conf
# Failing to do this will result in testsuite failures due to
# non-functional user lookup (#15230).
RUN sed -i -e 's/systemd//g' /etc/nsswitch.conf

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
RUN cabal update && \
    cabal install hscolour happy alex
ENV PATH /home/ghc/.cabal/bin:$PATH

CMD ["bash"]
