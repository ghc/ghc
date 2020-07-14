1. Install docker
2. `sudo docker build .`, then note down the image
```
[lolotp@lolotp-fedora-R90RRK8K fbghc]$ sudo docker build .
Sending build context to Docker daemon  4.417GB
Step 1/5 : FROM haskell:8.2.2
 ---> 6caf6c4c552e
Step 2/5 : RUN apt-get update
 ---> Using cache
 ---> 90f5f6b0823d
Step 3/5 : RUN apt-get install -y
 ---> Using cache
 ---> ca55df8aeb85
Step 4/5 : RUN apt-get install -y python3         autoconf automake libtool make libgmp-dev ncurses-dev g++ python bzip2 ca-certificates         alex happy
 ---> Using cache
 ---> cf17eb5e3098
Step 5/5 : RUN rm /bin/sh ; ln -s /bin/bash /bin/sh
 ---> Running in 4da290ff0a67
Removing intermediate container 4da290ff0a67
 ---> cdb402158579
Successfully built cdb402158579
```
The image hash is `cdb402158579` in the previous example

3. Run the docker image and change directory into fbghc to build
```
$ sudo docker run -it -v $(pwd):/fbghc cdb402158579  /bin/bash
root@03465d0a7999:/fbghc# ./validate --build-only
```
Please note that the string cdb402158579 should be the hash of the docker image you built in step 2 instead.
