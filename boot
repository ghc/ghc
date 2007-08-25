#! /bin/sh
set -e

# Check that we have all core packages.
for dir in `cat libraries/core-packages`; do
  if test ! -d libraries/$dir; then
    echo "Looks like you're missing libraries/$dir, maybe you haven't done './darcs-all get'?" >&2
    exit 1
  fi
done

# We don't recurse into the library packages with autoreconf anymore, so we
# have to do this manually. To avoid a strict dependency on autoreconf, we
# are careful to call autoreconf only when configure does not exist yet or the
# corresponding configure.ac is newer. This would be dead easy if every shell
# supported the "-nt" option for "test", but this is not the case. The only
# portable solution seems to be via find's "-newer" option or to basically give
# up and replace find with perl:   :-P
#
#   perl -e 'print "configure.ac\n" if -M "configure.ac" < -M "configure"'
for dir in . libraries/*; do
  if test -f $dir/configure.ac; then
    ( cd $dir ; { test ! -f configure || test -n "`find configure.ac -newer configure`"; } && autoreconf )
  fi
done

# Alas, darcs doesn't handle file permissions, so fix a few of them.
for f in boot darcs-all push-all validate; do
  test -f $f && chmod +x $f
done
