{-# LANGUAGE OverloadedStrings #-}
module KAT_PubKey.Rabin (rabinTests) where

import qualified Data.ByteString as B

import           Crypto.Hash
import           Crypto.Number.Serialize (os2ip)
import qualified Crypto.PubKey.Rabin.Basic as BRabin
import qualified Crypto.PubKey.Rabin.Modified as MRabin
import qualified Crypto.PubKey.Rabin.OAEP as OAEP
import qualified Crypto.PubKey.Rabin.RW as RW

import           Imports

basicRabinKey = BRabin.PrivateKey
    { BRabin.private_pub = BRabin.PublicKey
        { BRabin.public_n = 0xc9c4b0df9db989d93df4137fc2de2a9cee2610523f7a450ecbbf252babe98fba2f8e389c3e420c081e18f584c5746ca43f77f6af1fc79161f8bf8fbcb9564779986ecbe656dd16740cb8e399c33ff1dcc679e73c9c98a58c65a8673b7de57290a2d3191cb27e29d627f7ec6e874b1406051ffe9181e4d90d1b487b100ad30685 
        , BRabin.public_size = 128
        }
    , BRabin.private_p = 0xe071f231ab5912285a1f8db199795f5efdea4c32f646a3436eaec091ba853a3092216f26b539bbac1fe2ab2e4fbb20aad272a434a1e909bf6d3028aecae2a7b7
    , BRabin.private_q = 0xe6229470dc7da58bfcd962f1b3ddcf52304efbfb91d31c8ed84dbae2380c1ad2e338a523b4250863a689b3f262f949bd7a9f1a603c36634bb932dd71bf5daba3
    , BRabin.private_a = 0x65956653f711a63b776ce45862d4cd78f1ad7b1f8ed118bb8b5ea5fffd59762da5dc7c5298e236a8e45d5c93477cbc51f214b1cd1a4980eda859c1cb05e55666
    , BRabin.private_b = -0x63126dd9c5d6b5215f62012885570e1306b6a47ec1c46553f3b13ceae869149d14544438dbb976800cd62fbb52266f9a6405bc91f192a462c974bc8a6f832e03
    }

modifiedRabinKey = MRabin.PrivateKey
    { MRabin.private_pub = MRabin.PublicKey
        { MRabin.public_n = 0x9461a6e7c55cb610f20fd9af5d642404a63332a8d7c4fe7aa559cbcaec691e7216eed5d9322cb6a8619c220a0241b44e0d0a7cefda01fb84e59722b4e842ab5e190d214424bbdfed6d523426fc57a28045dfbb6e8159123077c542c0278ee2daf2d8993e286bf709a10a948da6b13008441581a22233f0ad3d5ebc5858ff7be5 
        , MRabin.public_size = 128
        }
    , MRabin.private_p = 0xc401e0ddbe565a8797292389bebb561c35eb019116ba25cc6c865a8d3d7bc599626ddf0bc4f575c22f89144fe99fc3300dd497ec2b7acc0221e729a61756b3f3
    , MRabin.private_q = 0xc1cc0e35f23f5086691a18c755881e3fe6937581948b109f47605b45d055e7b352e19ff729dfb33fbecb1d28b115e590449e5e4e228ab1876d889d3d41d87ec7 
    , MRabin.private_d = 0x128c34dcf8ab96c21e41fb35ebac848094c666551af89fcf54ab39795d8d23ce42dddabb264596d50c33844140483689c1a14f9dfb403f709cb2e4569d08556b9267e6460e84c69beda1defabd0285c4852c288b7ac27b78987bd19da337a6b1c7b123476732d9c0f656cc62a17f70e8fe34516cfa85ce6475bddeae9ffa0926
    }

rwKey = RW.PrivateKey
    { RW.private_pub = RW.PublicKey
        { RW.public_n = 0x992db4c84564c68d4ee2fe0903d938b41e83bcac48dfe8f2219ccee2ccbdefda4cbeea9f1c98a515c5f39a458f5ea11bca97102aaa3d9ac69e000093024e7b968359287cdf57bdacff5df1893df3539c7e358f037d49b5c6ae7110ab8117220c73b6265987039c2c97078fccacdd3f5a560aff5076fdc3958c532db28ab9a855 
        , RW.public_size = 128
        }
    , RW.private_p = 0xc144dd739c45397d61868ca944a9729a7ad34cf90466c8f5c98a88f5ab5e3288bcfd31d4af1d441d23a756a60abd4cf05c3e0b0053eb150166a327ae31e9347b
    , RW.private_q = 0xcae5a381f25a27ae2c359068753118fc384471cd6027e88b8b910306fb940781261089259a3c569546677aebd268704c767a071dbd4f50cb9f15fe448788856f
    , RW.private_d = 0x1325b69908ac98d1a9dc5fc1207b271683d07795891bfd1e443399dc5997bdfb4997dd53e39314a2b8be7348b1ebd4237952e2055547b358d3c000126049cf729ee5d4f0ea170b902e343a8ef0831900b963ba07a3176088ab2ab095db449d0052150d6be7b5402f459f17c759f6f043b06a5da64cb86bb910d340f7fa28fdce
    }

data EncryptionVector = EncryptionVector
    { seed :: ByteString
    , plainText :: ByteString
    , cipherText :: ByteString
    }

data SignatureVector = SignatureVector 
    { message :: ByteString
    , padding :: ByteString
    , signature :: Integer
    }

basicRabinEncryptionVectors =
    [ EncryptionVector
        { plainText = "\x75\x0c\x40\x47\xf5\x47\xe8\xe4\x14\x11\x85\x65\x23\x29\x8a\xc9\xba\xe2\x45\xef\xaf\x13\x97\xfb\xe5\x6f\x9d\xd5"
        , seed = "\x0c\xc7\x42\xce\x4a\x9b\x7f\x32\xf9\x51\xbc\xb2\x51\xef\xd9\x25\xfe\x4f\xe3\x5f"
        , cipherText = "\xaf\xc7\x03\xe3\x9d\x2f\x81\xc6\x3a\x80\x2a\xd1\x44\x26\x3f\x17\x0c\x0a\xe6\x48\x68\x98\x23\x14\x8f\x95\xd2\xce\xbb\xe7\x3f\x49\x34\x76\x1d\x99\x30\x7b\xeb\x84\xe5\x2a\x10\xd2\x1e\x11\x7e\x65\xe8\x88\x24\xc1\x12\xeb\x19\x0d\x97\xcd\x12\x25\x6b\x1f\x9b\x0c\x40\x40\xa3\x47\x00\xb7\x11\xf8\x50\x08\x51\x79\xe8\x1b\xd1\x77\xe0\x99\xa7\xe1\x5c\x63\xda\x29\xc7\xde\x28\x5d\x60\xed\x8e\xb2\x12\xd4\xfe\xb8\x1a\x5d\x17\x65\x80\x62\x6e\x65\x5c\x37\x07\x1c\xfa\xff\xe6\x21\xa5\x9f\xcd\x6a\x6a\xce\xa6\x96\xb2\xc5\x08\xe6"
        }   
    ]

basicRabinSignatureVectors =
    [ SignatureVector
        { message = "\x75\x0c\x40\x47\xf5\x47\xe8\xe4\x14\x11\x85\x65\x23\x29\x8a\xc9\xba\xe2\x45\xef\xaf\x13\x97\xfb\xe5\x6f\x9d\xd5"
        , padding = "\xe9\x87\x17\x15\xa2\xe4\x30\x15"
        , signature = 0xac95807bdd03ca975690151d39d23d75e5db2731c4ba30b83c3f3ea74709e4d4e340d7dab952356a76c9b8705b214e28d59f5bdc7c7fdff4e104569e30359b5c65c2dcd5b94db58505cd8b188267121700beebd7edbee492e374514646471b5c3fa252a2580dc7343f455683815d6d7c590dd3bcaa7df41d8b08197ccb183408
        }   
    ]

modifiedRabinSignatureVectors =
    [ SignatureVector
        { message = "\x75\x0c\x40\x47\xf5\x47\xe8\xe4\x14\x11\x85\x65\x23\x29\x8a\xc9\xba\xe2\x45\xef\xaf\x13\x97\xfb\xe5\x6f\x9d\xd5"
        , padding = B.empty -- not used
        , signature = 0x278c7c269119218ab7f501ea53a97ab15a3a5a263c6daed8980abec78291e9729e0e3457731cdea8ec31a7566e93d10fc9b2615fe3e54f4533a5506ac24a3bd286e270324e538066f0ddf503f9b5e0c18e18379659834906ebd99c0d31588c66e70fc653bc8865b9239999cbd35704917d8647d1199286c533233e3e03582dd
        }   
    ]
    
rwEncryptionVectors =
    [ EncryptionVector
        { plainText = "\x75\x0c\x40\x47\xf5\x47\xe8\xe4\x14\x11\x85\x65\x23\x29\x8a\xc9\xba\xe2\x45\xef\xaf\x13\x97\xfb\xe5\x6f\x9d\xd5"
        , seed = "\x0c\xc7\x42\xce\x4a\x9b\x7f\x32\xf9\x51\xbc\xb2\x51\xef\xd9\x25\xfe\x4f\xe3\x5f"
        , cipherText = "\x40\xc2\xe3\x36\xac\x46\x72\x8a\xaf\x33\x75\xe1\x27\xd0\x38\x40\xe2\x24\x4e\x20\xa7\x5d\x85\xd3\x74\x81\x21\xfd\xc9\x40\x90\x80\x8c\xed\x2d\xd3\x5b\xc4\xb7\xc9\x7c\x80\xa5\x2f\x63\x86\x34\x4e\x8c\x92\x07\x86\x9e\xda\xfd\xf8\x11\x83\x8a\x5a\x23\xc1\xe6\x77\x37\x5d\xf9\x5c\x60\xd1\x6d\xfd\x0c\x54\xd1\x00\xe9\xab\x97\x6d\x8e\x83\x8b\x6e\x1a\x38\x73\x43\xe2\x24\xc2\xe2\x4e\x74\x3f\xe4\x4d\xdd\x27\xed\xc7\x72\x88\xd3\x0f\x93\xb3\xdb\xa2\xb7\xaf\x6d\xe9\xab\x76\x53\x63\xf9\x62\xd7\x52\x44\x61\x60\x5d\x2e\x9b\xf7"
        }   
    ]

rwSignatureVectors =
    [ SignatureVector
        { message = "\x75\x0c\x40\x47\xf5\x47\xe8\xe4\x14\x11\x85\x65\x23\x29\x8a\xc9\xba\xe2\x45\xef\xaf\x13\x97\xfb\xe5\x6f\x9d\xd5"
        , padding = B.empty -- not used
        , signature = 0x1e57b554a8e83aacd9d4067f9535991e7db47803250cded5cc8af5458a6bb11fea852139e0afe143f9339dd94a518e354e702134d1ae222460127829d92e8bf6441336f5ae7044ec7b6c3ad8b9aeeb1ea02a49798e020cb5b558120bbb51f060eb1608ba68f90cac7edb1051c177d3bdbb99d1ad92e8d75d6f72f1d06f1d25be
        }   
    ]

doBasicRabinEncryptTest key i vector = testCase (show i) (Right (cipherText vector) @=? actual)
    where actual = BRabin.encryptWithSeed (seed vector) (OAEP.defaultOAEPParams SHA1) key (plainText vector)

doBasicRabinDecryptTest key i vector = testCase (show i) (Just (plainText vector) @=? actual)
    where actual = BRabin.decrypt (OAEP.defaultOAEPParams SHA1) key (cipherText vector)

doBasicRabinSignTest key i vector = testCase (show i) (Right (BRabin.Signature ((os2ip $ padding vector), (signature vector))) @=? actual)
    where actual = BRabin.signWith (padding vector) key SHA1 (message vector)

doBasicRabinVerifyTest key i vector = testCase (show i) (True @=? actual)
    where actual = BRabin.verify key SHA1 (message vector) (BRabin.Signature ((os2ip $ padding vector), (signature vector)))

doModifiedRabinSignTest key i vector = testCase (show i) (Right (signature vector) @=? actual)
    where actual = MRabin.sign key SHA1 (message vector)

doModifiedRabinVerifyTest key i vector = testCase (show i) (True @=? actual)
    where actual = MRabin.verify key SHA1 (message vector) (signature vector)

doRwEncryptTest key i vector = testCase (show i) (Right (cipherText vector) @=? actual)
    where actual = RW.encryptWithSeed (seed vector) (OAEP.defaultOAEPParams SHA1) key (plainText vector)

doRwDecryptTest key i vector = testCase (show i) (Just (plainText vector) @=? actual)
    where actual = RW.decrypt (OAEP.defaultOAEPParams SHA1) key (cipherText vector)

doRwSignTest key i vector = testCase (show i) (Right (signature vector) @=? actual)
    where actual = RW.sign key SHA1 (message vector)

doRwVerifyTest key i vector = testCase (show i) (True @=? actual)
    where actual = RW.verify key SHA1 (message vector) (signature vector)

rabinTests = testGroup "Rabin"
    [ testGroup "Basic"
        [ testGroup "encrypt" $ zipWith (doBasicRabinEncryptTest $ BRabin.private_pub basicRabinKey) [katZero..] basicRabinEncryptionVectors
        , testGroup "decrypt" $ zipWith (doBasicRabinDecryptTest basicRabinKey) [katZero..] basicRabinEncryptionVectors
        , testGroup "sign" $ zipWith (doBasicRabinSignTest basicRabinKey) [katZero..] basicRabinSignatureVectors
        , testGroup "verify" $ zipWith (doBasicRabinVerifyTest $ BRabin.private_pub basicRabinKey) [katZero..] basicRabinSignatureVectors
        ]
    , testGroup "Modified"
        [ testGroup "sign" $ zipWith (doModifiedRabinSignTest modifiedRabinKey) [katZero..] modifiedRabinSignatureVectors
        , testGroup "verify" $ zipWith (doModifiedRabinVerifyTest $ MRabin.private_pub modifiedRabinKey) [katZero..] modifiedRabinSignatureVectors
        ]
    , testGroup "RW"
        [ testGroup "encrypt" $ zipWith (doRwEncryptTest $ RW.private_pub rwKey) [katZero..] rwEncryptionVectors
        , testGroup "decrypt" $ zipWith (doRwDecryptTest rwKey) [katZero..] rwEncryptionVectors
        , testGroup "sign" $ zipWith (doRwSignTest rwKey) [katZero..] rwSignatureVectors
        , testGroup "verify" $ zipWith (doRwVerifyTest $ RW.private_pub rwKey) [katZero..] rwSignatureVectors
        ]
    ]
