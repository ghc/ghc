FROM haskell:8.2.2

RUN apt-get update
RUN apt-get install -y
RUN apt-get install -y python3 \
        autoconf automake libtool make libgmp-dev ncurses-dev g++ python bzip2 ca-certificates \
        alex happy
# /bin/sh in this docker image is a soft link to dash which doesn't work
# with the `test_bindist` we have. Changing this to point to bash instead.
RUN rm /bin/sh ; ln -s /bin/bash /bin/sh
