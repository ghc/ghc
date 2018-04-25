# Continuous integration on Travis

This folder contains scripts for running CI on Travis.

The most unusual thing about our Travis setup is that, after we finish
building our project, we upload the build products to Git (via the
haskell-pushbot account) to a separate repository to do testing.
There are two reasons we do this:

1. On our slowest configuration (GHC 8 on Mac OS X), the time to
   build and run tests was easily bumping up against the Travis time
   limit.  By uploading our build products to a separate account,
   we get twice as much time to run our builds.

2. Travis parallelism is limited on a per-account basis; if we
   upload build products to another account, we get more parallelism!
   (Travis, let us know if you don't like this :)

Here is the general lifecycle of a Travis run:

1. For each build matrix configuration, we run ../travis-script.sh
   to build Cabal, cabal-install, and all of the test suites.

2. Once the build is successful, we invoke upload.sh to upload
   the build products to the cabal-binaries repository.  This is done
   using the private key id_rsa (associated with haskell-pushbot's
   account).  This upload contains its own .travis.yml (customized
   for the particular build matrix configuration), and some special
   JSON metadata in the commit message.

3. Triggered by the push to cabal-binaries, Travis on haskell-pushbot
   will run the tests.  After this finishes, it will invoke a webhook
   that invokes sake-bot (https://github.com/ezyang/sake-bot, currently
   installed at https://sake-bot.herokuapp.com/) which will post
   back the GitHub status result to the upstream Cabal repository.

## Who maintains this setup

Unfortunately, there are some infrastructural permissions which
don't coincide with the GitHub permissions for the Cabal project.
Here are the relevant bits:

* The GitHub account haskell-pushbot is owned by @ezyang (along
  with the associated Travis account.)

* The Heroku instance https://sake-bot.herokuapp.com/ is maintained by @ezyang.
  It has a private key for a GitHub integration on the Haskell
  GitHub organization (which gives it permissions to update
  statuses on the Cabal project).

Fortunately, if @ezyang ever gets run over a bus, all of these
infrastructural bits can be reconfigured.  Here is what you
would need to do:

* Create a new GitHub account to replace haskell-pushbot

* Generate a new private key, associate it with the GH account, and
  replace id_rsa and id_rsa.pub with the new account

* Create a new binaries repository, modify the invocation of
  "git remote add" in upload.sh to point to the new location.
  Enable Travis for this repository.

* Create a new Heroku instance of https://github.com/ezyang/sake-bot
  (use the "Deploy with Heroku" button.)  Follow the instructions
  there; you'll need a private key for an integration associated
  with the Haskell organization; talk to one of the admins there
  to get the key.

That's it!

## Limitations

If you push to your local account and run Travis, the builds will
still take place in the shared cabal-binaries Travis instance, and
you won't get status updates (because sake-bot doesn't have permissions
to update the status update on any repository besides its own.)
In principle, it should be possible for upload.sh to autodetect if you
have a cabal-binaries repository setup on your local account, and use
that instead, but as the sake-bot integration can only be installed
on the same account it was created for, getting sake-bot setup would
be annoyingly fiddly.  Shout if you need this to work.
