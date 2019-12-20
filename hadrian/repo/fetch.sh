#!/bin/sh
# This file is generated with cabal-bundler

set -ex

cat <<EOF > SHA256SUMS
5143ec26d740c1a508c93a8860e64407e7546c29b9817db20ff1595c1968d287  Cabal-3.0.0.0.tar.gz
7b560baa5853de777702dc23a6f2126ae4adbfdab163295bc56323a706914610  QuickCheck-2.13.2.tar.gz
b77c8a1270767c64e2adb21a6e91ee7cd904ba17edae17bc20fd03da5256e0e3  alex-3.2.5.tar.gz
08a35c5294009040f1e5eb721a21b60df7af6584092bb3d376ab1b2e57e26914  clock-0.8.tar.gz
9e81788ea870cc94e0cd809f3258aec0a361981783f59b122aeea20801256d4b  extra-1.6.18.tar.gz
f7fc5bdcfef0d43a793a3c64e7c0fd3b1d35eea97a37f0e69d6612ab255c9b4b  filepattern-0.1.1.tar.gz
fb9a23e41401711a3b288f93cf0a66db9f97da1ce32ec4fffea4b78a0daeb40f  happy-1.19.12.tar.gz
822e5413fbccca6ae884d3aba4066422c8b5d58d23d18b9ecb5c03273bb19ab4  hashable-1.3.0.0.tar.gz
91d552f3c8992f745607de39239b950db78295b533eda43d083699872a4ee36d  heaps-0.3.6.1.tar.gz
e28dd65bee8083b17210134e22e01c6349dc33c3b7bd17705973cd014e9f20ac  js-dgtable-0.5.2.tar.gz
1ba2f2a6b8d85da76c41f526c98903cbb107f8642e506c072c1e7e3c20fe5e7a  js-flot-0.8.3.tar.gz
e0e0681f0da1130ede4e03a051630ea439c458cb97216cdb01771ebdbe44069b  js-jquery-3.3.1.tar.gz
786a44fea328caf704b762ebc887e9e8476c4378fdf3a06c94e86ef1878d1576  primitive-0.7.0.0.tar.gz
7b67624fd76ddf97c206de0801dc7e888097e9d572974be9b9ea6551d76965df  random-1.1.cabal
b718a41057e25a3a71df693ab0fe2263d492e759679b3c2fea6ea33b171d3a5a  random-1.1.tar.gz
79e761e64b862564a3470d5d356cb6b060b14452d675859aed3b2d1e14646648  semigroups-0.19.1.tar.gz
4efe2ded1a11aefe60d6fe68d576c79a4fbbcaa563953daef8fb234ffb6f8c7b  shake-0.18.3.tar.gz
fce462557f490c6c3d264ca70ef98a2c644ba341a71e6ee9f87ee7f3e7ab0acc  splitmix-0.0.3.tar.gz
5e9b095a9283d9e2f064fec73a81a6b6ea0b7fda3f219a8175785d2d2a3de204  unordered-containers-0.2.10.0.cabal
65f117bdbdea9efc75fb9fd539873de7687e005d8898bb21821020a4b383c573  unordered-containers-0.2.10.0.tar.gz
68cc6cf665e7212334a51b63d6936daeaca023b2cfe8637d130acfe95f91700b  utf8-string-1.0.1.1.cabal
fb0b9e3acbe0605bcd1c63e51f290a7bbbe6628dfa3294ff453e4235fbaef140  utf8-string-1.0.1.1.tar.gz
EOF

curl --silent --location --output Cabal-3.0.0.0.tar.gz 'http://hackage.haskell.org/package/Cabal-3.0.0.0/Cabal-3.0.0.0.tar.gz'
curl --silent --location --output QuickCheck-2.13.2.tar.gz 'http://hackage.haskell.org/package/QuickCheck-2.13.2/QuickCheck-2.13.2.tar.gz'
curl --silent --location --output alex-3.2.5.tar.gz 'http://hackage.haskell.org/package/alex-3.2.5/alex-3.2.5.tar.gz'
curl --silent --location --output clock-0.8.tar.gz 'http://hackage.haskell.org/package/clock-0.8/clock-0.8.tar.gz'
curl --silent --location --output extra-1.6.18.tar.gz 'http://hackage.haskell.org/package/extra-1.6.18/extra-1.6.18.tar.gz'
curl --silent --location --output filepattern-0.1.1.tar.gz 'http://hackage.haskell.org/package/filepattern-0.1.1/filepattern-0.1.1.tar.gz'
curl --silent --location --output happy-1.19.12.tar.gz 'http://hackage.haskell.org/package/happy-1.19.12/happy-1.19.12.tar.gz'
curl --silent --location --output hashable-1.3.0.0.tar.gz 'http://hackage.haskell.org/package/hashable-1.3.0.0/hashable-1.3.0.0.tar.gz'
curl --silent --location --output heaps-0.3.6.1.tar.gz 'http://hackage.haskell.org/package/heaps-0.3.6.1/heaps-0.3.6.1.tar.gz'
curl --silent --location --output js-dgtable-0.5.2.tar.gz 'http://hackage.haskell.org/package/js-dgtable-0.5.2/js-dgtable-0.5.2.tar.gz'
curl --silent --location --output js-flot-0.8.3.tar.gz 'http://hackage.haskell.org/package/js-flot-0.8.3/js-flot-0.8.3.tar.gz'
curl --silent --location --output js-jquery-3.3.1.tar.gz 'http://hackage.haskell.org/package/js-jquery-3.3.1/js-jquery-3.3.1.tar.gz'
curl --silent --location --output primitive-0.7.0.0.tar.gz 'http://hackage.haskell.org/package/primitive-0.7.0.0/primitive-0.7.0.0.tar.gz'
curl --silent --location --output random-1.1.cabal 'http://hackage.haskell.org/package/random-1.1/revision/1.cabal'
curl --silent --location --output random-1.1.tar.gz 'http://hackage.haskell.org/package/random-1.1/random-1.1.tar.gz'
curl --silent --location --output semigroups-0.19.1.tar.gz 'http://hackage.haskell.org/package/semigroups-0.19.1/semigroups-0.19.1.tar.gz'
curl --silent --location --output shake-0.18.3.tar.gz 'http://hackage.haskell.org/package/shake-0.18.3/shake-0.18.3.tar.gz'
curl --silent --location --output splitmix-0.0.3.tar.gz 'http://hackage.haskell.org/package/splitmix-0.0.3/splitmix-0.0.3.tar.gz'
curl --silent --location --output unordered-containers-0.2.10.0.cabal 'http://hackage.haskell.org/package/unordered-containers-0.2.10.0/revision/1.cabal'
curl --silent --location --output unordered-containers-0.2.10.0.tar.gz 'http://hackage.haskell.org/package/unordered-containers-0.2.10.0/unordered-containers-0.2.10.0.tar.gz'
curl --silent --location --output utf8-string-1.0.1.1.cabal 'http://hackage.haskell.org/package/utf8-string-1.0.1.1/revision/3.cabal'
curl --silent --location --output utf8-string-1.0.1.1.tar.gz 'http://hackage.haskell.org/package/utf8-string-1.0.1.1/utf8-string-1.0.1.1.tar.gz'

sha256sum -c SHA256SUMS
