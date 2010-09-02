# Put the Happy-generated .hs files in the right place in the source dist.
set -e
rm -f dist/haddock-*.tar.gz
rm -rf dist/haddock-*/
./Setup sdist
cd dist
tar xvzf haddock-*.tar.gz
cd haddock-*/
mkdir dist
mkdir dist/build
mv haddock dist/build
cd ..
tar cvzf haddock-*.tar.gz haddock-*/

# Steps for doing a release:
#  * Update version number in .cabal, doc/haddock.xml, haddock.spec
#  * Update CHANGES
#  * Source:
#    - do the above
#    - upload the dist to haskell.org:haddock/dist/${version}
#    - scp CHANGES haskell.org:haddock/CHANGES.txt
#  * Binaries:
#    - build the Windows binary zip (see build-windows-dist.sh)
#    - scp haddock-<version>-Win32.zip haskell.org:haddock/dist
#  * Documentation:
#    - cd doc
#    - make html
#    - mv haddock haddock-html
#    - tar cvzf haddock-doc-html-${version}.tar.gz haddock-html
#    - scp haddock-doc-html-${version}.tar.gz www.haskell.org:../haskell/haddock/doc
#    - ssh haskell.org
#        - cd haddock/doc
#        - tar xvzf haddock-doc-html-${version}.tar.gz
#        - rm -rf html-OLD
#        - mv html html-OLD && mv haddock-html html
#  * Update the web page (~/darcs/www/haddock/index.html), and push it
