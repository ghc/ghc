-- Test vectors for SHA1 are taken from GEC2: www.secg.org/collateral/gec2.pdf
-- Test vectors for SHA224, SHA256, SHA384, SHA512 are taken from RFC 6979
{-# LANGUAGE OverloadedStrings #-}
module KAT_PubKey.ECDSA (ecdsaTests) where

import Crypto.Number.Serialize

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as ECC
import Crypto.Hash (SHA1(..), SHA224(..), SHA256(..), SHA384(..), SHA512(..))

import Imports

data VectorECDSA = VectorECDSA
    { curve :: ECC.Curve
    , msg   :: ByteString
    , d     :: Integer
    , q     :: ECC.Point
    , k     :: Integer
    , r     :: Integer
    , s     :: Integer
    }

vectorsSHA1 =
    [ VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p160r1
        , msg   = "abc"
        , d     = 971761939728640320549601132085879836204587084162
        , q     = ECC.Point 466448783855397898016055842232266600516272889280
                            1110706324081757720403272427311003102474457754220
        , k     = 702232148019446860144825009548118511996283736794
        , r     = 1176954224688105769566774212902092897866168635793
        , s     = 299742580584132926933316745664091704165278518100
        }
    -- from official ECDSA KATs
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_t163k1
        , msg   = i2osp 0xa2c1a03fdd00521bb08fc88d20344321977aaf637ef9d5470dd7d2c8628fc8d0d1f1d3587c6b3fd02386f8c13db341b14748a9475cc63baf065df64054b27d5c2cdf0f98e3bbb81d0b5dc94f8cdb87acf75720f6163de394c8c6af360bc1acb85b923a493b7b27cc111a257e36337bd94eb0fab9d5e633befb1ae7f1b244bfaa
        , d     = 0x00000011f2626d90d26cb4c0379043b26e64107fc
        , q     = ECC.Point 0x0389fa5ad7f8304325a8c060ef7dcb83042c045bc
                            0x0eefa094a5054da196943cc80509dcb9f59e5bc2e
        , k     = 0x0000000c3a4ff97286126dab1e5089395fcc47ebb
        , r     = 0x0dbe6c3a1dc851e7f2338b5c26c62b4b37bf8035c
        , s     = 0x1c76458135b1ff9fbd23009b8414a47996126b56a
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_t163k1
        , msg   = i2osp 0x67048080daaeb77d3ac31babdf8be23dbe75ceb4dfb94aa8113db5c5dcb6fe14b70f717b7b0ed0881835a66a86e6d840ffcb7d976c75ef2d1d4322fbbc86357384e24707aef88cea2c41a01a9a3d1b9e72ce650c7fdecc4f9448d3a77df6cdf13647ab295bb3132de0b1b2c402d8d2de7d452f1e003e0695de1470d1064eee16
        , d     = 0x00000006a3803301daee9af09bb5b6c991a4f49a4
        , q     = ECC.Point 0x4b500f555e857da8c299780130c5c3f48f02ee322 0x5c1c0ae25b47f06cc46fb86b12d2d8c0ba6a4bf07
        , k     = 0x0000002f39fbf77f3e0dc046116de692b6cf91b16
        , r     = 0x3d3eeda42f65d727f4a564f1415654356c6c57a6c
        , s     = 0x35e4d43c5f08baddf138449db1ad0b7872552b7cd
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_t163k1
        , msg   = i2osp 0x77e007dc2acd7248256165a4b30e98986f51a81efd926b85f74c81bc2a6d2bcd030060a844091e22fbb0ff3db5a20caaefb5d58ccdcbc27f0ff8a4d940e78f303079ec1ca5b0ca3d4ecc7580f8b34a9f0496c9e719d2ec3e1614b7644bc11179e895d2c0b58a1da204fbf0f6e509f97f983eacb6487092caf6e8e4e6b3c458b2
        , d     = 0x0000002e28676514bd93fea11b62db0f6e324b18d
        , q     = ECC.Point 0x3f9c90b71f6a1de20a2716f38ef1b5f98c757bd42 0x2ff0a5d266d447ef62d43fbca6c34c08c1ce35a40
        , k     = 0x00000001233ae699883e74e7f4dfb5279ff22280a
        , r     = 0x39de3cd2cf04145e522b8fba3f23e9218226e0860
        , s     = 0x2af62bfb3cfa202e2342606ee5bb0934c3b0375b6
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_t163k1
        , msg   = i2osp 0xfbacfcce4688748406ddf5c3495021eef8fb399865b649eb2395a04a1ab28335da2c236d306fcc59f7b65ea931cf0139571e1538ede5688958c3ac69f47a285362f5ad201f89cc735b7b465408c2c41b310fc8908d0be45054df2a7351fae36b390e842f3b5cdd9ad832940df5b2d25c2ed43ce86eaf2508bcf401ae58bb1d47
        , d     = 0x000000361dd088e3a6d3c910686c8dce57e5d4d8e
        , q     = ECC.Point 0x064f905c1da9d7e9c32d81890ae6f30dcc7839d32 0x06f1faedb6d9032016d3b681e7cf69c29d29eb27b
        , k     = 0x00000022f723e9f5da56d3d0837d5dca2f937395f
        , r     = 0x374cdc8571083fecfbd4e25e1cd69ecc66b715f2d
        , s     = 0x313b10949222929b2f20b15d446c27d6dcae3f086
        }
    ]

rfc6979_vectorsSHA224 =
    [ VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , msg   = "sample"
        , d     = 0x6fab034934e4c0fc9ae67f5b5659a9d7d1fefd187ee09fd4
        , q     = ECC.Point 0xac2c77f529f91689fea0ea5efec7f210d8eea0b9e047ed56
                            0x3bc723e57670bd4887ebc732c523063d0a7c957bc97c1c43
        , k     = 0x4381526b3fc1e7128f202e194505592f01d5ff4c5af015d8
        , r     = 0xa1f00dad97aeec91c95585f36200c65f3c01812aa60378f5
        , s     = 0xe07ec1304c7c6c9debbe980b9692668f81d4de7922a0f97a
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , msg   = "test"
        , d     = 0x6fab034934e4c0fc9ae67f5b5659a9d7d1fefd187ee09fd4
        , q     = ECC.Point 0xac2c77f529f91689fea0ea5efec7f210d8eea0b9e047ed56
                            0x3bc723e57670bd4887ebc732c523063d0a7c957bc97c1c43
        , k     = 0xf5dc805f76ef851800700cce82e7b98d8911b7d510059fbe
        , r     = 0x6945a1c1d1b2206b8145548f633bb61cef04891baf26ed34
        , s     = 0xb7fb7fdfc339c0b9bd61a9f5a8eaf9be58fc5cba2cb15293
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p224r1
        , msg   = "sample"
        , d     = 0xf220266e1105bfe3083e03ec7a3a654651f45e37167e88600bf257c1
        , q     = ECC.Point 0x00cf08da5ad719e42707fa431292dea11244d64fc51610d94b130d6c
                            0xeeab6f3debe455e3dbf85416f7030cbd94f34f2d6f232c69f3c1385a
        , k     = 0xc1d1f2f10881088301880506805feb4825fe09acb6816c36991aa06d
        , r     = 0x1cdfe6662dde1e4a1ec4cdedf6a1f5a2fb7fbd9145c12113e6abfd3e
        , s     = 0xa6694fd7718a21053f225d3f46197ca699d45006c06f871808f43ebc
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p224r1
        , msg   = "test"
        , d     = 0xf220266e1105bfe3083e03ec7a3a654651f45e37167e88600bf257c1
        , q     = ECC.Point 0x00cf08da5ad719e42707fa431292dea11244d64fc51610d94b130d6c
                            0xeeab6f3debe455e3dbf85416f7030cbd94f34f2d6f232c69f3c1385a
        , k     = 0xdf8b38d40dca3e077d0ac520bf56b6d565134d9b5f2eae0d34900524
        , r     = 0xc441ce8e261ded634e4cf84910e4c5d1d22c5cf3b732bb204dbef019
        , s     = 0x902f42847a63bdc5f6046ada114953120f99442d76510150f372a3f4
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p256r1
        , msg   = "sample"
        , d     = 0xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721
        , q     = ECC.Point 0x60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6
                            0x7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299
        , k     = 0x103f90ee9dc52e5e7fb5132b7033c63066d194321491862059967c715985d473
        , r     = 0x53b2fff5d1752b2c689df257c04c40a587fababb3f6fc2702f1343af7ca9aa3f
        , s     = 0xb9afb64fdc03dc1a131c7d2386d11e349f070aa432a4acc918bea988bf75c74c
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p256r1
        , msg   = "test"
        , d     = 0xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721
        , q     = ECC.Point 0x60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6
                            0x7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299
        , k     = 0x669f4426f2688b8be0db3a6bd1989bdaefff84b649eeb84f3dd26080f667faa7
        , r     = 0xc37edb6f0ae79d47c3c27e962fa269bb4f441770357e114ee511f662ec34a692
        , s     = 0xc820053a05791e521fcaad6042d40aea1d6b1a540138558f47d0719800e18f2d
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p384r1
        , msg   = "sample"
        , d     = 0x6b9d3dad2e1b8c1c05b19875b6659f4de23c3b667bf297ba9aa47740787137d896d5724e4c70a825f872c9ea60d2edf5
        , q     = ECC.Point 0xec3a4e415b4e19a4568618029f427fa5da9a8bc4ae92e02e06aae5286b300c64def8f0ea9055866064a254515480bc13
                            0x8015d9b72d7d57244ea8ef9ac0c621896708a59367f9dfb9f54ca84b3f1c9db1288b231c3ae0d4fe7344fd2533264720
        , k     = 0xa4e4d2f0e729eb786b31fc20ad5d849e304450e0ae8e3e341134a5c1afa03cab8083ee4e3c45b06a5899ea56c51b5879
        , r     = 0x42356e76b55a6d9b4631c865445dbe54e056d3b3431766d0509244793c3f9366450f76ee3de43f5a125333a6be060122
        , s     = 0x9da0c81787064021e78df658f2fbb0b042bf304665db721f077a4298b095e4834c082c03d83028efbf93a3c23940ca8d
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p384r1
        , msg   = "test"
        , d     = 0x6b9d3dad2e1b8c1c05b19875b6659f4de23c3b667bf297ba9aa47740787137d896d5724e4c70a825f872c9ea60d2edf5
        , q     = ECC.Point 0xec3a4e415b4e19a4568618029f427fa5da9a8bc4ae92e02e06aae5286b300c64def8f0ea9055866064a254515480bc13
                            0x8015d9b72d7d57244ea8ef9ac0c621896708a59367f9dfb9f54ca84b3f1c9db1288b231c3ae0d4fe7344fd2533264720
        , k     = 0x18fa39db95aa5f561f30fa3591dc59c0fa3653a80daffa0b48d1a4c6dfcbff6e3d33be4dc5eb8886a8ecd093f2935726
        , r     = 0xe8c9d0b6ea72a0e7837fea1d14a1a9557f29faa45d3e7ee888fc5bf954b5e62464a9a817c47ff78b8c11066b24080e72
        , s     = 0x07041d4a7a0379ac7232ff72e6f77b6ddb8f09b16cce0ec3286b2bd43fa8c6141c53ea5abef0d8231077a04540a96b66
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p521r1
        , msg   = "sample"
        , d     = 0x0fad06daa62ba3b25d2fb40133da757205de67f5bb0018fee8c86e1b68c7e75caa896eb32f1f47c70855836a6d16fcc1466f6d8fbec67db89ec0c08b0e996b83538
        , q     = ECC.Point 0x1894550d0785932e00eaa23b694f213f8c3121f86dc97a04e5a7167db4e5bcd371123d46e45db6b5d5370a7f20fb633155d38ffa16d2bd761dcac474b9a2f5023a4
                            0x0493101c962cd4d2fddf782285e64584139c2f91b47f87ff82354d6630f746a28a0db25741b5b34a828008b22acc23f924faafbd4d33f81ea66956dfeaa2bfdfcf5
        , k     = 0x121415ec2cd7726330a61f7f3fa5de14be9436019c4db8cb4041f3b54cf31be0493ee3f427fb906393d895a19c9523f3a1d54bb8702bd4aa9c99dab2597b92113f3
        , r     = 0x1776331cfcdf927d666e032e00cf776187bc9fdd8e69d0dabb4109ffe1b5e2a30715f4cc923a4a5e94d2503e9acfed92857b7f31d7152e0f8c00c15ff3d87e2ed2e
        , s     = 0x050cb5265417fe2320bbb5a122b8e1a32bd699089851128e360e620a30c7e17ba41a666af126ce100e5799b153b60528d5300d08489ca9178fb610a2006c254b41f
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p521r1
        , msg   = "test"
        , d     = 0x0fad06daa62ba3b25d2fb40133da757205de67f5bb0018fee8c86e1b68c7e75caa896eb32f1f47c70855836a6d16fcc1466f6d8fbec67db89ec0c08b0e996b83538
        , q     = ECC.Point 0x1894550d0785932e00eaa23b694f213f8c3121f86dc97a04e5a7167db4e5bcd371123d46e45db6b5d5370a7f20fb633155d38ffa16d2bd761dcac474b9a2f5023a4
                            0x0493101c962cd4d2fddf782285e64584139c2f91b47f87ff82354d6630f746a28a0db25741b5b34a828008b22acc23f924faafbd4d33f81ea66956dfeaa2bfdfcf5
        , k     = 0x040d09fcf3c8a5f62cf4fb223cbbb2b9937f6b0577c27020a99602c25a01136987e452988781484edbbcf1c47e554e7fc901bc3085e5206d9f619cff07e73d6f706
        , r     = 0x1c7ed902e123e6815546065a2c4af977b22aa8eaddb68b2c1110e7ea44d42086bfe4a34b67ddc0e17e96536e358219b23a706c6a6e16ba77b65e1c595d43cae17fb
        , s     = 0x177336676304fcb343ce028b38e7b4fba76c1c1b277da18cad2a8478b2a9a9f5bec0f3ba04f35db3e4263569ec6aade8c92746e4c82f8299ae1b8f1739f8fd519a4
        }
    ]

rfc6979_vectorsSHA256 =
    [ VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , msg   = "sample"
        , d     = 0x6fab034934e4c0fc9ae67f5b5659a9d7d1fefd187ee09fd4
        , q     = ECC.Point 0xac2c77f529f91689fea0ea5efec7f210d8eea0b9e047ed56
                            0x3bc723e57670bd4887ebc732c523063d0a7c957bc97c1c43
        , k     = 0x32b1b6d7d42a05cb449065727a84804fb1a3e34d8f261496
        , r     = 0x4b0b8ce98a92866a2820e20aa6b75b56382e0f9bfd5ecb55
        , s     = 0xccdb006926ea9565cbadc840829d8c384e06de1f1e381b85
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , msg   = "test"
        , d     = 0x6fab034934e4c0fc9ae67f5b5659a9d7d1fefd187ee09fd4
        , q     = ECC.Point 0xac2c77f529f91689fea0ea5efec7f210d8eea0b9e047ed56
                            0x3bc723e57670bd4887ebc732c523063d0a7c957bc97c1c43
        , k     = 0x5c4ce89cf56d9e7c77c8585339b006b97b5f0680b4306c6c
        , r     = 0x3a718bd8b4926c3b52ee6bbe67ef79b18cb6eb62b1ad97ae
        , s     = 0x5662e6848a4a19b1f1ae2f72acd4b8bbe50f1eac65d9124f
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p224r1
        , msg   = "sample"
        , d     = 0xf220266e1105bfe3083e03ec7a3a654651f45e37167e88600bf257c1
        , q     = ECC.Point 0x00cf08da5ad719e42707fa431292dea11244d64fc51610d94b130d6c
                            0xeeab6f3debe455e3dbf85416f7030cbd94f34f2d6f232c69f3c1385a
        , k     = 0xad3029e0278f80643de33917ce6908c70a8ff50a411f06e41dedfcdc
        , r     = 0x61aa3da010e8e8406c656bc477a7a7189895e7e840cdfe8ff42307ba
        , s     = 0xbc814050dab5d23770879494f9e0a680dc1af7161991bde692b10101
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p224r1
        , msg   = "test"
        , d     = 0xf220266e1105bfe3083e03ec7a3a654651f45e37167e88600bf257c1
        , q     = ECC.Point 0x00cf08da5ad719e42707fa431292dea11244d64fc51610d94b130d6c
                            0xeeab6f3debe455e3dbf85416f7030cbd94f34f2d6f232c69f3c1385a
        , k     = 0xff86f57924da248d6e44e8154eb69f0ae2aebaee9931d0b5a969f904
        , r     = 0xad04dde87b84747a243a631ea47a1ba6d1faa059149ad2440de6fba6
        , s     = 0x178d49b1ae90e3d8b629be3db5683915f4e8c99fdf6e666cf37adcfd
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p256r1
        , msg   = "sample"
        , d     = 0xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721
        , q     = ECC.Point 0x60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6
                            0x7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299
        , k     = 0xa6e3c57dd01abe90086538398355dd4c3b17aa873382b0f24d6129493d8aad60
        , r     = 0xefd48b2aacb6a8fd1140dd9cd45e81d69d2c877b56aaf991c34d0ea84eaf3716
        , s     = 0xf7cb1c942d657c41d436c7a1b6e29f65f3e900dbb9aff4064dc4ab2f843acda8
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p256r1
        , msg   = "test"
        , d     = 0xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721
        , q     = ECC.Point 0x60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6
                            0x7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299
        , k     = 0xd16b6ae827f17175e040871a1c7ec3500192c4c92677336ec2537acaee0008e0
        , r     = 0xf1abb023518351cd71d881567b1ea663ed3efcf6c5132b354f28d3b0b7d38367
        , s     = 0x019f4113742a2b14bd25926b49c649155f267e60d3814b4c0cc84250e46f0083
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p384r1
        , msg   = "sample"
        , d     = 0x6b9d3dad2e1b8c1c05b19875b6659f4de23c3b667bf297ba9aa47740787137d896d5724e4c70a825f872c9ea60d2edf5
        , q     = ECC.Point 0xec3a4e415b4e19a4568618029f427fa5da9a8bc4ae92e02e06aae5286b300c64def8f0ea9055866064a254515480bc13
                            0x8015d9b72d7d57244ea8ef9ac0c621896708a59367f9dfb9f54ca84b3f1c9db1288b231c3ae0d4fe7344fd2533264720
        , k     = 0x180ae9f9aec5438a44bc159a1fcb277c7be54fa20e7cf404b490650a8acc414e375572342863c899f9f2edf9747a9b60
        , r     = 0x21b13d1e013c7fa1392d03c5f99af8b30c570c6f98d4ea8e354b63a21d3daa33bde1e888e63355d92fa2b3c36d8fb2cd
        , s     = 0xf3aa443fb107745bf4bd77cb3891674632068a10ca67e3d45db2266fa7d1feebefdc63eccd1ac42ec0cb8668a4fa0ab0
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p384r1
        , msg   = "test"
        , d     = 0x6b9d3dad2e1b8c1c05b19875b6659f4de23c3b667bf297ba9aa47740787137d896d5724e4c70a825f872c9ea60d2edf5
        , q     = ECC.Point 0xec3a4e415b4e19a4568618029f427fa5da9a8bc4ae92e02e06aae5286b300c64def8f0ea9055866064a254515480bc13
                            0x8015d9b72d7d57244ea8ef9ac0c621896708a59367f9dfb9f54ca84b3f1c9db1288b231c3ae0d4fe7344fd2533264720
        , k     = 0x0cfac37587532347dc3389fdc98286bba8c73807285b184c83e62e26c401c0faa48dd070ba79921a3457abff2d630ad7
        , r     = 0x6d6defac9ab64dabafe36c6bf510352a4cc27001263638e5b16d9bb51d451559f918eedaf2293be5b475cc8f0188636b
        , s     = 0x2d46f3becbcc523d5f1a1256bf0c9b024d879ba9e838144c8ba6baeb4b53b47d51ab373f9845c0514eefb14024787265
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p521r1
        , msg   = "sample"
        , d     = 0x0fad06daa62ba3b25d2fb40133da757205de67f5bb0018fee8c86e1b68c7e75caa896eb32f1f47c70855836a6d16fcc1466f6d8fbec67db89ec0c08b0e996b83538
        , q     = ECC.Point 0x1894550d0785932e00eaa23b694f213f8c3121f86dc97a04e5a7167db4e5bcd371123d46e45db6b5d5370a7f20fb633155d38ffa16d2bd761dcac474b9a2f5023a4
                            0x0493101c962cd4d2fddf782285e64584139c2f91b47f87ff82354d6630f746a28a0db25741b5b34a828008b22acc23f924faafbd4d33f81ea66956dfeaa2bfdfcf5
        , k     = 0x0edf38afcaaecab4383358b34d67c9f2216c8382aaea44a3dad5fdc9c32575761793fef24eb0fc276dfc4f6e3ec476752f043cf01415387470bcbd8678ed2c7e1a0
        , r     = 0x1511bb4d675114fe266fc4372b87682baecc01d3cc62cf2303c92b3526012659d16876e25c7c1e57648f23b73564d67f61c6f14d527d54972810421e7d87589e1a7
        , s     = 0x04a171143a83163d6df460aaf61522695f207a58b95c0644d87e52aa1a347916e4f7a72930b1bc06dbe22ce3f58264afd23704cbb63b29b931f7de6c9d949a7ecfc
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p521r1
        , msg   = "test"
        , d     = 0x0fad06daa62ba3b25d2fb40133da757205de67f5bb0018fee8c86e1b68c7e75caa896eb32f1f47c70855836a6d16fcc1466f6d8fbec67db89ec0c08b0e996b83538
        , q     = ECC.Point 0x1894550d0785932e00eaa23b694f213f8c3121f86dc97a04e5a7167db4e5bcd371123d46e45db6b5d5370a7f20fb633155d38ffa16d2bd761dcac474b9a2f5023a4
                            0x0493101c962cd4d2fddf782285e64584139c2f91b47f87ff82354d6630f746a28a0db25741b5b34a828008b22acc23f924faafbd4d33f81ea66956dfeaa2bfdfcf5
        , k     = 0x01de74955efaabc4c4f17f8e84d881d1310b5392d7700275f82f145c61e843841af09035bf7a6210f5a431a6a9e81c9323354a9e69135d44ebd2fcaa7731b909258
        , r     = 0x00e871c4a14f993c6c7369501900c4bc1e9c7b0b4ba44e04868b30b41d8071042eb28c4c250411d0ce08cd197e4188ea4876f279f90b3d8d74a3c76e6f1e4656aa8
        , s     = 0x0cd52dbaa33b063c3a6cd8058a1fb0a46a4754b034fcc644766ca14da8ca5ca9fde00e88c1ad60ccba759025299079d7a427ec3cc5b619bfbc828e7769bcd694e86
        }
    ]

rfc6979_vectorsSHA384 =
    [ VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , msg   = "sample"
        , d     = 0x6fab034934e4c0fc9ae67f5b5659a9d7d1fefd187ee09fd4
        , q     = ECC.Point 0xac2c77f529f91689fea0ea5efec7f210d8eea0b9e047ed56
                            0x3bc723e57670bd4887ebc732c523063d0a7c957bc97c1c43
        , k     = 0x4730005c4fcb01834c063a7b6760096dbe284b8252ef4311
        , r     = 0xda63bf0b9abcf948fbb1e9167f136145f7a20426dcc287d5
        , s     = 0xc3aa2c960972bd7a2003a57e1c4c77f0578f8ae95e31ec5e
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , msg   = "test"
        , d     = 0x6fab034934e4c0fc9ae67f5b5659a9d7d1fefd187ee09fd4
        , q     = ECC.Point 0xac2c77f529f91689fea0ea5efec7f210d8eea0b9e047ed56
                            0x3bc723e57670bd4887ebc732c523063d0a7c957bc97c1c43
        , k     = 0x5afefb5d3393261b828db6c91fbc68c230727b030c975693
        , r     = 0xb234b60b4db75a733e19280a7a6034bd6b1ee88af5332367
        , s     = 0x7994090b2d59bb782be57e74a44c9a1c700413f8abefe77a
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p224r1
        , msg   = "sample"
        , d     = 0xf220266e1105bfe3083e03ec7a3a654651f45e37167e88600bf257c1
        , q     = ECC.Point 0x00cf08da5ad719e42707fa431292dea11244d64fc51610d94b130d6c
                            0xeeab6f3debe455e3dbf85416f7030cbd94f34f2d6f232c69f3c1385a
        , k     = 0x52b40f5a9d3d13040f494e83d3906c6079f29981035c7bd51e5cac40
        , r     = 0x0b115e5e36f0f9ec81f1325a5952878d745e19d7bb3eabfaba77e953
        , s     = 0x830f34ccdfe826ccfdc81eb4129772e20e122348a2bbd889a1b1af1d
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p224r1
        , msg   = "test"
        , d     = 0xf220266e1105bfe3083e03ec7a3a654651f45e37167e88600bf257c1
        , q     = ECC.Point 0x00cf08da5ad719e42707fa431292dea11244d64fc51610d94b130d6c
                            0xeeab6f3debe455e3dbf85416f7030cbd94f34f2d6f232c69f3c1385a
        , k     = 0x7046742b839478c1b5bd31db2e862ad868e1a45c863585b5f22bdc2d
        , r     = 0x389b92682e399b26518a95506b52c03bc9379a9dadf3391a21fb0ea4
        , s     = 0x414a718ed3249ff6dbc5b50c27f71f01f070944da22ab1f78f559aab
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p256r1
        , msg   = "sample"
        , d     = 0xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721
        , q     = ECC.Point 0x60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6
                            0x7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299
        , k     = 0x09f634b188cefd98e7ec88b1aa9852d734d0bc272f7d2a47decc6ebeb375aad4
        , r     = 0x0eafea039b20e9b42309fb1d89e213057cbf973dc0cfc8f129edddc800ef7719
        , s     = 0x4861f0491e6998b9455193e34e7b0d284ddd7149a74b95b9261f13abde940954
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p256r1
        , msg   = "test"
        , d     = 0xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721
        , q     = ECC.Point 0x60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6
                            0x7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299
        , k     = 0x16aeffa357260b04b1dd199693960740066c1a8f3e8edd79070aa914d361b3b8
        , r     = 0x83910e8b48bb0c74244ebdf7f07a1c5413d61472bd941ef3920e623fbccebeb6
        , s     = 0x8ddbec54cf8cd5874883841d712142a56a8d0f218f5003cb0296b6b509619f2c
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p384r1
        , msg   = "sample"
        , d     = 0x6b9d3dad2e1b8c1c05b19875b6659f4de23c3b667bf297ba9aa47740787137d896d5724e4c70a825f872c9ea60d2edf5
        , q     = ECC.Point 0xec3a4e415b4e19a4568618029f427fa5da9a8bc4ae92e02e06aae5286b300c64def8f0ea9055866064a254515480bc13
                            0x8015d9b72d7d57244ea8ef9ac0c621896708a59367f9dfb9f54ca84b3f1c9db1288b231c3ae0d4fe7344fd2533264720
        , k     = 0x94ed910d1a099dad3254e9242ae85abde4ba15168eaf0ca87a555fd56d10fbca2907e3e83ba95368623b8c4686915cf9
        , r     = 0x94edbb92a5ecb8aad4736e56c691916b3f88140666ce9fa73d64c4ea95ad133c81a648152e44acf96e36dd1e80fabe46
        , s     = 0x99ef4aeb15f178cea1fe40db2603138f130e740a19624526203b6351d0a3a94fa329c145786e679e7b82c71a38628ac8
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p384r1
        , msg   = "test"
        , d     = 0x6b9d3dad2e1b8c1c05b19875b6659f4de23c3b667bf297ba9aa47740787137d896d5724e4c70a825f872c9ea60d2edf5
        , q     = ECC.Point 0xec3a4e415b4e19a4568618029f427fa5da9a8bc4ae92e02e06aae5286b300c64def8f0ea9055866064a254515480bc13
                            0x8015d9b72d7d57244ea8ef9ac0c621896708a59367f9dfb9f54ca84b3f1c9db1288b231c3ae0d4fe7344fd2533264720
        , k     = 0x015ee46a5bf88773ed9123a5ab0807962d193719503c527b031b4c2d225092ada71f4a459bc0da98adb95837db8312ea
        , r     = 0x8203b63d3c853e8d77227fb377bcf7b7b772e97892a80f36ab775d509d7a5feb0542a7f0812998da8f1dd3ca3cf023db
        , s     = 0xddd0760448d42d8a43af45af836fce4de8be06b485e9b61b827c2f13173923e06a739f040649a667bf3b828246baa5a5
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p521r1
        , msg   = "sample"
        , d     = 0x0fad06daa62ba3b25d2fb40133da757205de67f5bb0018fee8c86e1b68c7e75caa896eb32f1f47c70855836a6d16fcc1466f6d8fbec67db89ec0c08b0e996b83538
        , q     = ECC.Point 0x1894550d0785932e00eaa23b694f213f8c3121f86dc97a04e5a7167db4e5bcd371123d46e45db6b5d5370a7f20fb633155d38ffa16d2bd761dcac474b9a2f5023a4
                            0x0493101c962cd4d2fddf782285e64584139c2f91b47f87ff82354d6630f746a28a0db25741b5b34a828008b22acc23f924faafbd4d33f81ea66956dfeaa2bfdfcf5
        , k     = 0x1546a108bc23a15d6f21872f7ded661fa8431ddbd922d0dcdb77cc878c8553ffad064c95a920a750ac9137e527390d2d92f153e66196966ea554d9adfcb109c4211
        , r     = 0x1ea842a0e17d2de4f92c15315c63ddf72685c18195c2bb95e572b9c5136ca4b4b576ad712a52be9730627d16054ba40cc0b8d3ff035b12ae75168397f5d50c67451
        , s     = 0x1f21a3cee066e1961025fb048bd5fe2b7924d0cd797babe0a83b66f1e35eeaf5fde143fa85dc394a7dee766523393784484bdf3e00114a1c857cde1aa203db65d61
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p521r1
        , msg   = "test"
        , d     = 0x0fad06daa62ba3b25d2fb40133da757205de67f5bb0018fee8c86e1b68c7e75caa896eb32f1f47c70855836a6d16fcc1466f6d8fbec67db89ec0c08b0e996b83538
        , q     = ECC.Point 0x1894550d0785932e00eaa23b694f213f8c3121f86dc97a04e5a7167db4e5bcd371123d46e45db6b5d5370a7f20fb633155d38ffa16d2bd761dcac474b9a2f5023a4
                            0x0493101c962cd4d2fddf782285e64584139c2f91b47f87ff82354d6630f746a28a0db25741b5b34a828008b22acc23f924faafbd4d33f81ea66956dfeaa2bfdfcf5
        , k     = 0x1f1fc4a349a7da9a9e116bfdd055dc08e78252ff8e23ac276ac88b1770ae0b5dceb1ed14a4916b769a523ce1e90ba22846af11df8b300c38818f713dadd85de0c88
        , r     = 0x14bee21a18b6d8b3c93fab08d43e739707953244fdbe924fa926d76669e7ac8c89df62ed8975c2d8397a65a49dcc09f6b0ac62272741924d479354d74ff6075578c
        , s     = 0x133330865c067a0eaf72362a65e2d7bc4e461e8c8995c3b6226a21bd1aa78f0ed94fe536a0dca35534f0cd1510c41525d163fe9d74d134881e35141ed5e8e95b979
        }
    ]

rfc6979_vectorsSHA512 =
    [ VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , msg   = "sample"
        , d     = 0x6fab034934e4c0fc9ae67f5b5659a9d7d1fefd187ee09fd4
        , q     = ECC.Point 0xac2c77f529f91689fea0ea5efec7f210d8eea0b9e047ed56
                            0x3bc723e57670bd4887ebc732c523063d0a7c957bc97c1c43
        , k     = 0xa2ac7ab055e4f20692d49209544c203a7d1f2c0bfbc75db1
        , r     = 0x4d60c5ab1996bd848343b31c00850205e2ea6922dac2e4b8
        , s     = 0x3f6e837448f027a1bf4b34e796e32a811cbb4050908d8f67
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , msg   = "test"
        , d     = 0x6fab034934e4c0fc9ae67f5b5659a9d7d1fefd187ee09fd4
        , q     = ECC.Point 0xac2c77f529f91689fea0ea5efec7f210d8eea0b9e047ed56
                            0x3bc723e57670bd4887ebc732c523063d0a7c957bc97c1c43
        , k     = 0x0758753a5254759c7cfbad2e2d9b0792eee44136c9480527
        , r     = 0xfe4f4ae86a58b6507946715934fe2d8ff9d95b6b098fe739
        , s     = 0x74cf5605c98fba0e1ef34d4b5a1577a7dcf59457cae52290
           }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p224r1
        , msg   = "sample"
        , d     = 0xf220266e1105bfe3083e03ec7a3a654651f45e37167e88600bf257c1
        , q     = ECC.Point 0x00cf08da5ad719e42707fa431292dea11244d64fc51610d94b130d6c
                            0xeeab6f3debe455e3dbf85416f7030cbd94f34f2d6f232c69f3c1385a
        , k     = 0x9db103ffededf9cfdba05184f925400c1653b8501bab89cea0fbec14
        , r     = 0x074bd1d979d5f32bf958ddc61e4fb4872adcafeb2256497cdac30397
        , s     = 0xa4ceca196c3d5a1ff31027b33185dc8ee43f288b21ab342e5d8eb084
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p224r1
        , msg   = "test"
        , d     = 0xf220266e1105bfe3083e03ec7a3a654651f45e37167e88600bf257c1
        , q     = ECC.Point 0x00cf08da5ad719e42707fa431292dea11244d64fc51610d94b130d6c
                            0xeeab6f3debe455e3dbf85416f7030cbd94f34f2d6f232c69f3c1385a
        , k     = 0xe39c2aa4ea6be2306c72126d40ed77bf9739bb4d6ef2bbb1dcb6169d
        , r     = 0x049f050477c5add858cac56208394b5a55baebbe887fdf765047c17c
        , s     = 0x077eb13e7005929cefa3cd0403c7cdcc077adf4e44f3c41b2f60ecff
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p256r1
        , msg   = "sample"
        , d     = 0xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721
        , q     = ECC.Point 0x60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6
                            0x7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299
        , k     = 0x5fa81c63109badb88c1f367b47da606da28cad69aa22c4fe6ad7df73a7173aa5
        , r     = 0x8496a60b5e9b47c825488827e0495b0e3fa109ec4568fd3f8d1097678eb97f00
        , s     = 0x2362ab1adbe2b8adf9cb9edab740ea6049c028114f2460f96554f61fae3302fe
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p256r1
        , msg   = "test"
        , d     = 0xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721
        , q     = ECC.Point 0x60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6
                            0x7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299
        , k     = 0x6915d11632aca3c40d5d51c08daf9c555933819548784480e93499000d9f0b7f
        , r     = 0x461d93f31b6540894788fd206c07cfa0cc35f46fa3c91816fff1040ad1581a04
        , s     = 0x39af9f15de0db8d97e72719c74820d304ce5226e32dedae67519e840d1194e55
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p384r1
        , msg   = "sample"
        , d     = 0x6b9d3dad2e1b8c1c05b19875b6659f4de23c3b667bf297ba9aa47740787137d896d5724e4c70a825f872c9ea60d2edf5
        , q     = ECC.Point 0xec3a4e415b4e19a4568618029f427fa5da9a8bc4ae92e02e06aae5286b300c64def8f0ea9055866064a254515480bc13
                            0x8015d9b72d7d57244ea8ef9ac0c621896708a59367f9dfb9f54ca84b3f1c9db1288b231c3ae0d4fe7344fd2533264720
        , k     = 0x92fc3c7183a883e24216d1141f1a8976c5b0dd797dfa597e3d7b32198bd35331a4e966532593a52980d0e3aaa5e10ec3
        , r     = 0xed0959d5880ab2d869ae7f6c2915c6d60f96507f9cb3e047c0046861da4a799cfe30f35cc900056d7c99cd7882433709
        , s     = 0x512c8cceee3890a84058ce1e22dbc2198f42323ce8aca9135329f03c068e5112dc7cc3ef3446defceb01a45c2667fdd5
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p384r1
        , msg   = "test"
        , d     = 0x6b9d3dad2e1b8c1c05b19875b6659f4de23c3b667bf297ba9aa47740787137d896d5724e4c70a825f872c9ea60d2edf5
        , q     = ECC.Point 0xec3a4e415b4e19a4568618029f427fa5da9a8bc4ae92e02e06aae5286b300c64def8f0ea9055866064a254515480bc13
                            0x8015d9b72d7d57244ea8ef9ac0c621896708a59367f9dfb9f54ca84b3f1c9db1288b231c3ae0d4fe7344fd2533264720
        , k     = 0x3780c4f67cb15518b6acae34c9f83568d2e12e47deab6c50a4e4ee5319d1e8ce0e2cc8a136036dc4b9c00e6888f66b6c
        , r     = 0xa0d5d090c9980faf3c2ce57b7ae951d31977dd11c775d314af55f76c676447d06fb6495cd21b4b6e340fc236584fb277
        , s     = 0x976984e59b4c77b0e8e4460dca3d9f20e07b9bb1f63beefaf576f6b2e8b224634a2092cd3792e0159ad9cee37659c736
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p521r1
        , msg   = "sample"
        , d     = 0x0fad06daa62ba3b25d2fb40133da757205de67f5bb0018fee8c86e1b68c7e75caa896eb32f1f47c70855836a6d16fcc1466f6d8fbec67db89ec0c08b0e996b83538
        , q     = ECC.Point 0x1894550d0785932e00eaa23b694f213f8c3121f86dc97a04e5a7167db4e5bcd371123d46e45db6b5d5370a7f20fb633155d38ffa16d2bd761dcac474b9a2f5023a4
                            0x0493101c962cd4d2fddf782285e64584139c2f91b47f87ff82354d6630f746a28a0db25741b5b34a828008b22acc23f924faafbd4d33f81ea66956dfeaa2bfdfcf5
        , k     = 0x1dae2ea071f8110dc26882d4d5eae0621a3256fc8847fb9022e2b7d28e6f10198b1574fdd03a9053c08a1854a168aa5a57470ec97dd5ce090124ef52a2f7ecbffd3
        , r     = 0x0c328fafcbd79dd77850370c46325d987cb525569fb63c5d3bc53950e6d4c5f174e25a1ee9017b5d450606add152b534931d7d4e8455cc91f9b15bf05ec36e377fa
        , s     = 0x0617cce7cf5064806c467f678d3b4080d6f1cc50af26ca209417308281b68af282623eaa63e5b5c0723d8b8c37ff0777b1a20f8ccb1dccc43997f1ee0e44da4a67a
        }
    , VectorECDSA
        { curve = ECC.getCurveByName ECC.SEC_p521r1
        , msg   = "test"
        , d     = 0x0fad06daa62ba3b25d2fb40133da757205de67f5bb0018fee8c86e1b68c7e75caa896eb32f1f47c70855836a6d16fcc1466f6d8fbec67db89ec0c08b0e996b83538
        , q     = ECC.Point 0x1894550d0785932e00eaa23b694f213f8c3121f86dc97a04e5a7167db4e5bcd371123d46e45db6b5d5370a7f20fb633155d38ffa16d2bd761dcac474b9a2f5023a4
                            0x0493101c962cd4d2fddf782285e64584139c2f91b47f87ff82354d6630f746a28a0db25741b5b34a828008b22acc23f924faafbd4d33f81ea66956dfeaa2bfdfcf5
        , k     = 0x16200813020ec986863bedfc1b121f605c1215645018aea1a7b215a564de9eb1b38a67aa1128b80ce391c4fb71187654aaa3431027bfc7f395766ca988c964dc56d
        , r     = 0x13e99020abf5cee7525d16b69b229652ab6bdf2affcaef38773b4b7d08725f10cdb93482fdcc54edcee91eca4166b2a7c6265ef0ce2bd7051b7cef945babd47ee6d
        , s     = 0x1fbd0013c674aa79cb39849527916ce301c66ea7ce8b80682786ad60f98f7e78a19ca69eff5c57400e3b3a0ad66ce0978214d13baf4e9ac60752f7b155e2de4dce3
        }
    ]

vectorToPrivate :: VectorECDSA -> ECDSA.PrivateKey
vectorToPrivate vector = ECDSA.PrivateKey (curve vector) (d vector)

vectorToPublic :: VectorECDSA -> ECDSA.PublicKey
vectorToPublic vector = ECDSA.PublicKey (curve vector) (q vector)

doSignatureTest hashAlg i vector = testCase (show i) (expected @=? actual)
  where expected = Just $ ECDSA.Signature (r vector) (s vector)
        actual   = ECDSA.signWith (k vector) (vectorToPrivate vector) hashAlg (msg vector)

doVerifyTest hashAlg i vector = testCase (show i) (True @=? actual)
  where actual = ECDSA.verify hashAlg (vectorToPublic vector) (ECDSA.Signature (r vector) (s vector)) (msg vector)

ecdsaTests = testGroup "ECDSA"
    [ testGroup "SHA1"
        [ testGroup "signature" $ zipWith (doSignatureTest SHA1) [katZero..] vectorsSHA1
        , testGroup "verify" $ zipWith (doVerifyTest SHA1) [katZero..] vectorsSHA1
        ]
    , testGroup "SHA224"
        [ testGroup "signature" $ zipWith (doSignatureTest SHA224) [katZero..] rfc6979_vectorsSHA224
        , testGroup "verify" $ zipWith (doVerifyTest SHA224) [katZero..] rfc6979_vectorsSHA224
        ]
    , testGroup "SHA256"
        [ testGroup "signature" $ zipWith (doSignatureTest SHA256) [katZero..] rfc6979_vectorsSHA256
        , testGroup "verify" $ zipWith (doVerifyTest SHA256) [katZero..] rfc6979_vectorsSHA256
        ]
    , testGroup "SHA384"
        [ testGroup "signature" $ zipWith (doSignatureTest SHA384) [katZero..] rfc6979_vectorsSHA384
        , testGroup "verify" $ zipWith (doVerifyTest SHA384) [katZero..] rfc6979_vectorsSHA384
        ]
    , testGroup "SHA512"
        [ testGroup "signature" $ zipWith (doSignatureTest SHA512) [katZero..] rfc6979_vectorsSHA512
        , testGroup "verify" $ zipWith (doVerifyTest SHA512) [katZero..] rfc6979_vectorsSHA512
        ]
    ]
