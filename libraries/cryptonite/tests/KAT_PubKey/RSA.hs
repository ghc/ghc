{-# LANGUAGE OverloadedStrings #-}
module KAT_PubKey.RSA (rsaTests) where

import qualified Crypto.PubKey.RSA        as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import           Crypto.Hash

import           Imports

import           Data.Either (isRight)

data VectorRSA = VectorRSA
    { size :: Int
    , msg  :: ByteString
    , n    :: Integer
    , e    :: Integer
    , d    :: Integer
    , p    :: Integer
    , q    :: Integer
    , dP   :: Integer
    , dQ   :: Integer
    , qinv :: Integer
    , sig  :: Either RSA.Error ByteString
    }

vectorsSHA1 =
    [ VectorRSA
        { size = 2048 `div` 8
        , msg  = "The quick brown fox jumps over the lazy dog"
        , n    = 0x00c896c245fcca81775346c5f4f958229cab1aee08196dab4ee5959018b856aab93e4486f37a32da1a6804403c88473ecf9f1b9266fc682400d45329b6ec195710c98d9ba728bc09d767e7e9d9b8b102c3b7e7529b87f649a2a5ebe165da21863ec7842de600a834a8be2227bc989145b52f84ba685d45484a3d530745598a5d8a9e7551b3278bf139a770929f776aed5d43559205fe937df93eeb8ff3fb3f2ce22d4b8a5c17aeafd19758ac5e6251df09ef6a2858e8558a7f476dde4efe859ff2fcb97767614563033fd1d2d300196b1abf256f7badb16c17def3804946d1bf9cd51760176b41bbd506b44ff2bfe5bcd5052180da3cfbbc6cd6f662c06a8baed9
        , e    = 0x10001
        , d    = 0x58aa533bae8f310536d95cdd796e5cf655a7f4b9bdcbbd62859743f7b95c0de10e462a44ebaa18c07d640ba4f6344fee648d427ca56bbf2662b45407187be70173a655bc6104257182eb7f720ef2a79f2de6619c804ffca299a7179df6fac4a57179daf4052c550295f0f111ab7ae38e406ff219f9c88b38cdbcaac51bdc4e961361b87e100d168fc08b298626a806b3bfeaa9579f400bbe6e3e6e4ae9b27446e1c5ce8c10c848b9ad7b6ed3a6b3871ad6a1a88af24e581da054845c197e8bae1582858410087c1180c4f0cc61689abfd0f61b8031910f3b3779e11a7fbe823d9a704c63c313f78c994975de834ee9ead5faf6c18b3e4248c51ba307776bf845
        , p    = 0x00f85bfcfe55af59445f21f67ab1d8617d1f84360556eeb660d5c466f29e4d2228f9cc3fde4c594ea97069a19c666b68b6d905b65738ae63de6c11f9181ee9262313e5165591651bb3abec192abbc8c3694550bcffa451a2e2d1976bf3ecbc4480354f8d8646133298156aaa626b8807c5295850f93686400835466b6a5ccec61b
        , q    = 0x00cec28b22b1d37c6c60d25e9747cb1bebd1270f0306db56ed8533f392d6a0cfe6b3dde13789758cf89febac214ba96667e46599f89ca210dced550ca6092a854ff95dff80ea48ff1a83455f4bb93f2ececa782da03b85a789239e8be5264130628724ceab57c8f76e4c7e822bf4fbf334c7d32610bec65047433e0e3b636afe1b
        , dP   = 0x52fe0a50c339514f33ab19be6e67ac4c2f97f2a55e236ef674f8a89e329ffbe64d731f749d76ca7e7c7e0fef3f9a6ce78d260784a600408736fdda8b60e8f0419088612a3ee7d695f7c171b78200d8abf8e9bdfe7f5e785beb45fa610c9eed151abb76c383ef2e5cfbeb24fcb68a426e741e7b108c53d859e5d39e5970a1f839
        , dQ   = 0x39ef91853b47038a6ae707d2642fa9b73e782f60adbf307085eeb4c5e496532b56234a4481a40ac870275da846c74506bf9d28b3dd501c618baf5548013185018fe2a301c0a48bb726297e367dc6129ba7685d8094ad32f0dea64295074f24fbb6dabd7e8daea686a5b09d512be89d91a09cae01eb332eb389480e3cddf2d119
        , qinv = 0x09ce1fa29008ef4b9798e5b8ec213dbdfec4fab4403ebf4b8786ad401ef33bc880c40a990b0826f72415192a206a504b27d2ba45ca555706200ea8e7a9b42d4077e9e6e0d80d4144966c53a36d23d30d987322dcc0013efe8df3b6b5914a2ceefc22cc5de6d569731794e9894f18f11d36a79558dc4c3ae5db1ce9bd05e7bf2e
        , sig  = Right "\x56\x66\x99\x0f\xd4\xea\x2b\xe0\x6d\x46\x3b\x10\x99\x5b\x06\x32\x5e\xec\x29\xfe\xa4\x63\x4d\x54\xf6\x31\x74\x5d\x01\x5a\x67\x09\x2e\xa7\x02\x8a\x48\x00\x3c\x0d\xef\x04\xe7\x52\x46\xe0\xfa\xb1\x42\x26\x89\xe7\xec\x25\x44\x76\xa0\x86\x33\xb0\xbe\x22\x17\x88\x9b\x18\x4d\x3e\xc2\x9b\xd4\x61\x2b\x9e\xde\x08\x56\xf8\xd5\xee\xb8\x38\xf4\x3d\xda\x9a\xbb\x34\x58\x87\x71\x1d\x1a\x7e\xc7\x3d\x46\x39\x01\x79\x29\x8b\xa4\xcd\xce\xd7\xab\xcb\x2e\x94\x5c\xfd\x54\xcc\xef\x80\x31\xfc\x5e\x8f\xc2\x4d\x76\x1e\x4c\xbc\x50\x7a\x9b\x08\xae\x85\xeb\x6a\xe0\x80\xdc\xff\x60\x13\xb0\x31\x94\x14\x9d\x8f\x9f\x48\x38\xcf\x4c\x82\x9d\x3b\x68\xc6\xe4\xe9\x5d\x94\x74\xa2\xac\x1f\xb9\x84\x41\x86\x11\xeb\x2c\x50\x64\xd7\x00\xe0\x85\x21\x5a\xd7\xae\x9b\x4c\x8e\x6a\x92\x97\xac\xcc\xb8\x38\x4f\x41\xb9\x3d\xa9\xfe\x69\x8b\x04\x81\xad\xfb\x0f\x49\x74\xfe\x26\x9c\x86\x0c\xf3\xd1\x8e\xa1\xb5\xaf\xef\x85\x3d\xfe\xd0\x7c\xcf\x18\xe4\x0f\x14\x99\xea\x93\x61\x79\x16\xbf\x38\xac\xa2\xa2\xac\xac\x2d\xae\x21\x85\x71\x94\xda\x5d\xa1\x82\xa8\x76\x82\xe5\x2f"
        }
    , VectorRSA
        { size = 360 `div` 8
        , msg  = "The quick brown fox jumps over the lazy dog"
        , n    = 0x00bc2d7481c83c8be55da4caeaf1a30dbf9a1226ba7443c0a66213180d3eb8e29c3162401b7be067dff8f571a8eb
        , e    = 0x10001
        , d    = 0x726fb62d82c707507a2d5055a6934136270d28ce350c3a36d89066e26fb54f5b33da0bc9a05c2084f2b39be4e1
        , p    = 0x0e3ff89e1f95a461c9f5ee480fd7b13529a225f3ee07fb
        , q    = 0x0d349ebc89329b493c03451ad20155de9775df55c55fd1
        , dP   = 0x00943adef9fb93a561967bab33f198c2c7414e777df997
        , dQ   = 0x078de99ceb5392f7f327dfb97717a27ae2e4606dddaa71
        , qinv = 0x0c54d59eaa029844fb3fe33a180161590b1cb103cc668e
        , sig  = Left RSA.SignatureTooLong
        }
    , VectorRSA
        { size = 368 `div` 8
        , msg  = "The quick brown fox jumps over the lazy dog"
        , n    = 0x009cff2fd20246e390d6860b48a3926e83086d1386f7147e9f195623cf8f18546ceb20d428b77e0748864c8f611cb7
        , e    = 0x10001
        , d    = 0x0097706cbf6624dd448c3a36ce35c27d49762a4948ca33804178d2ff826f8d336aaed622801c8d76d442be371da841
        , p    = 0x00d12519f81441069ab1a86c38e0065e9578a46e655d5a17
        , q    = 0x00c02b485ac3ee241d57b6b282f830d7d5bf6f4de75c1661
        , dP   = 0x00a1af4611444f34f4d88d7504cf23fd711e70382c42ec07
        , dQ   = 0x04226a4219a90bf9dda33e9ff6bb0649c0fea20c723cc1
        , qinv = 0x5dd87bf3c1e295dcc8602859a7cd74f05a2fe91a9d5877
        , sig  = Right "\x51\xe4\xdd\x98\xee\xd5\x06\xef\x7a\xa5\x3c\xaf\x29\x33\xa4\x91\xfa\x8b\xb8\x09\xcf\x3e\xa1\x64\x92\x71\xad\x7b\x3a\x83\xb2\xa0\x77\x94\x4e\x59\xdf\x69\x58\x2e\xc8\x8d\xa0\x70\xfe\x7d"
        }
    ]

vectorToPrivate :: VectorRSA -> RSA.PrivateKey
vectorToPrivate vector = RSA.PrivateKey
    { RSA.private_pub  = vectorToPublic vector
    , RSA.private_d    = d vector
    , RSA.private_p    = p vector
    , RSA.private_q    = q vector
    , RSA.private_dP   = dP vector
    , RSA.private_dQ   = dQ vector
    , RSA.private_qinv = qinv vector
    }

vectorToPublic :: VectorRSA -> RSA.PublicKey
vectorToPublic vector = RSA.PublicKey
    { RSA.public_size = size vector
    , RSA.public_n    = n vector
    , RSA.public_e    = e vector
    }

vectorHasSignature :: VectorRSA -> Bool
vectorHasSignature = isRight . sig

doSignatureTest i vector = testCase (show i) (expected @=? actual)
    where expected = sig vector
          actual   = RSA.sign Nothing (Just SHA1) (vectorToPrivate vector) (msg vector)

doVerifyTest i vector = testCase (show i) (True @=? actual)
    where actual = RSA.verify (Just SHA1) (vectorToPublic vector) (msg vector) bs
          Right bs = sig vector

rsaTests = testGroup "RSA"
    [ testGroup "SHA1"
        [ testGroup "signature" $ zipWith doSignatureTest [katZero..] vectorsSHA1
        , testGroup "verify" $ zipWith doVerifyTest [katZero..] $ filter vectorHasSignature vectorsSHA1
        ]
    ]
