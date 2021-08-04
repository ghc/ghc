{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
module Hash
    ( tests
    ) where

import Crypto.Hash

import qualified Data.ByteString as B
import           Data.ByteArray (convert)
import qualified Data.ByteArray.Encoding as B (convertToBase, Base(..))
import           GHC.TypeLits
import Imports

v0,v1,v2 :: ByteString
v0 = ""
v1 = "The quick brown fox jumps over the lazy dog"
v2 = "The quick brown fox jumps over the lazy cog"
vectors = [ v0, v1, v2 ]

instance Arbitrary ByteString where
    arbitrary = B.pack `fmap` arbitrary

data HashAlg = forall alg . HashAlgorithm alg => HashAlg alg

expected :: [ (String, HashAlg, [ByteString]) ]
expected = [
    ("MD2", HashAlg MD2, [
        "8350e5a3e24c153df2275c9f80692773",
        "03d85a0d629d2c442e987525319fc471",
        "6b890c9292668cdbbfda00a4ebf31f05" ]),
    ("MD4", HashAlg MD4, [
        "31d6cfe0d16ae931b73c59d7e0c089c0",
        "1bee69a46ba811185c194762abaeae90",
        "b86e130ce7028da59e672d56ad0113df" ]),
    ("MD5", HashAlg MD5, [
        "d41d8cd98f00b204e9800998ecf8427e",
        "9e107d9d372bb6826bd81d3542a419d6",
        "1055d3e698d289f2af8663725127bd4b" ]),
    ("SHA1", HashAlg SHA1, [
        "da39a3ee5e6b4b0d3255bfef95601890afd80709",
        "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12",
        "de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3" ]),
    ("SHA224", HashAlg SHA224, [
        "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f",
        "730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525",
        "fee755f44a55f20fb3362cdc3c493615b3cb574ed95ce610ee5b1e9b" ]),
    ("SHA256", HashAlg SHA256, [
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592",
        "e4c4d8f3bf76b692de791a173e05321150f7a345b46484fe427f6acc7ecc81be" ]),
    ("SHA384", HashAlg SHA384, [
        "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b",
        "ca737f1014a48f4c0b6dd43cb177b0afd9e5169367544c494011e3317dbf9a509cb1e5dc1e85a941bbee3d7f2afbc9b1",
        "098cea620b0978caa5f0befba6ddcf22764bea977e1c70b3483edfdf1de25f4b40d6cea3cadf00f809d422feb1f0161b" ]),
    ("SHA512", HashAlg SHA512, [
        "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e",
        "07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6",
        "3eeee1d0e11733ef152a6c29503b3ae20c4f1f3cda4cb26f1bc1a41f91c7fe4ab3bd86494049e201c4bd5155f31ecb7a3c8606843c4cc8dfcab7da11c8ae5045" ]),
    ("SHA512/224", HashAlg SHA512t_224, [
        "6ed0dd02806fa89e25de060c19d3ac86cabb87d6a0ddd05c333b84f4",
        "944cd2847fb54558d4775db0485a50003111c8e5daa63fe722c6aa37",
        "2b9d6565a7e40f780ba8ab7c8dcf41e3ed3b77997f4c55aa987eede5" ]),
    ("SHA512/256", HashAlg SHA512t_256, [
        "c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a",
        "dd9d67b371519c339ed8dbd25af90e976a1eeefd4ad3d889005e532fc5bef04d",
        "cc8d255a7f2f38fd50388fd1f65ea7910835c5c1e73da46fba01ea50d5dd76fb" ]),
    ("RIPEMD160", HashAlg RIPEMD160, [
        "9c1185a5c5e9fc54612808977ee8f548b2258d31",
        "37f332f68db77bd9d7edd4969571ad671cf9dd3b",
        "132072df690933835eb8b6ad0b77e7b6f14acad7" ]),
    ("Tiger", HashAlg Tiger, [
        "3293ac630c13f0245f92bbb1766e16167a4e58492dde73f3",
        "6d12a41e72e644f017b6f0e2f7b44c6285f06dd5d2c5b075",
        "a8f04b0f7201a0d728101c9d26525b31764a3493fcd8458f" ])
{-
    , ("Skein256-160", HashAlg Skein256_160, [
        "ff800bed6d2044ee9d604a674e3fda50d9b24a72",
        "3265703c166aa3e0d7da070b9cf1b1a5953f0a77",
        "17b29aa1424b3ec022505bd215ff73fd2e6d1e5a" ])
-}
    , ("Skein256-256", HashAlg Skein256_256, [
        "c8877087da56e072870daa843f176e9453115929094c3a40c463a196c29bf7ba",
        "c0fbd7d779b20f0a4614a66697f9e41859eaf382f14bf857e8cdb210adb9b3fe",
        "fb2f2f2deed0e1dd7ee2b91cee34e2d1c22072e1f5eaee288c35a0723eb653cd" ])
{-
    , ("Skein512-160", HashAlg Skein512_160, [
        "49daf1ccebb3544bc93cb5019ba91b0eea8876ee",
        "826325ee55a6dd18c3b2dbbc9c10420f5475975e",
        "7544ec7a35712ec953f02b0d0c86641cae4eb6e5" ])
-}
    , ("Skein512-384", HashAlg Skein512_384, [
        "dd5aaf4589dc227bd1eb7bc68771f5baeaa3586ef6c7680167a023ec8ce26980f06c4082c488b4ac9ef313f8cbe70808",
        "f814c107f3465e7c54048a5503547deddc377264f05c706b0d19db4847b354855ee52ab6a785c238c9e710d848542041",
        "e06520eeadc1d0a44fee1d2492547499c1e58526387c8b9c53905e5edb79f9840575cbf844e21b1ad1ea126dd8a8ca6f" ])
    , ("Skein512-512", HashAlg Skein512_512, [
        "bc5b4c50925519c290cc634277ae3d6257212395cba733bbad37a4af0fa06af41fca7903d06564fea7a2d3730dbdb80c1f85562dfcc070334ea4d1d9e72cba7a",
        "94c2ae036dba8783d0b3f7d6cc111ff810702f5c77707999be7e1c9486ff238a7044de734293147359b4ac7e1d09cd247c351d69826b78dcddd951f0ef912713",
        "7f81113575e4b4d3441940e87aca331e6d63d103fe5107f29cd877af0d0f5e0ea34164258c60da5190189d0872e63a96596d2ef25e709099842da71d64111e0f" ])
{-
    , ("Skein512-896", HashAlg Skein512_896, [
        "b95175236c83a459ce7ec6c12b761a838b22d750e765b3fdaa892201b2aa714bc3d1d887dd64028bbf177c1dd11baa09c6c4ddb598fd07d6a8c131a09fc5b958e2999a8006754b25abe3bf8492b7eabec70e52e04e5ac867df2393c573f16eee3244554f1d2b724f2c0437c62007f770",
        "3265708553e7d146e5c7bcbc97b3e9e9f5b53a5e4af53612bdd6454da4fa7b13d413184fe34ed57b6574be10e389d0ec4b1d2b1dd2c80e0257d5a76b2cd86a19a27b1bcb3cc24d911b5dc5ee74d19ad558fd85b5f024e99f56d1d3199f1f9f88ed85fab9f945f11cf9fc00e94e3ca4c7",
        "3d23d3db9be719bbd2119f8402a28f38d8225faa79d5b68b80738c64a82004aafc7a840cd6dd9bced6644fa894a3d8d7d2ee89525fd1956a2db052c4c2f8d2111c91ef46b0997540d42bcf384826af1a5ef6510077f52d0574cf2b46f1b6a5dad07ed40f3d21a13ca2d079fa602ff02d" ])
-}
    , ("Whirlpool", HashAlg Whirlpool, [
        "19fa61d75522a4669b44e39c1d2e1726c530232130d407f89afee0964997f7a73e83be698b288febcf88e3e03c4f0757ea8964e59b63d93708b138cc42a66eb3",
        "b97de512e91e3828b40d2b0fdce9ceb3c4a71f9bea8d88e75c4fa854df36725fd2b52eb6544edcacd6f8beddfea403cb55ae31f03ad62a5ef54e42ee82c3fb35",
        "dce81fc695cfea3d7e1446509238daf89f24cc61896f2d265927daa70f2108f8902f0dfd68be085d5abb9fcd2e482c1dc24f2fabf81f40b73495cad44d7360d3"])
    , ("Keccak-224", HashAlg Keccak_224, [
        "f71837502ba8e10837bdd8d365adb85591895602fc552b48b7390abd",
        "310aee6b30c47350576ac2873fa89fd190cdc488442f3ef654cf23fe",
        "0b27ff3b732133287f6831e2af47cf342b7ef1f3fcdee248811090cd" ])
    , ("Keccak-256", HashAlg Keccak_256, [
        "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470",
        "4d741b6f1eb29cb2a9b9911c82f56fa8d73b04959d3d9d222895df6c0b28aa15",
        "ed6c07f044d7573cc53bf1276f8cba3dac497919597a45b4599c8f73e22aa334" ])
    , ("Keccak-384", HashAlg Keccak_384, [
        "2c23146a63a29acf99e73b88f8c24eaa7dc60aa771780ccc006afbfa8fe2479b2dd2b21362337441ac12b515911957ff",
        "283990fa9d5fb731d786c5bbee94ea4db4910f18c62c03d173fc0a5e494422e8a0b3da7574dae7fa0baf005e504063b3",
        "1cc515e1812491058d8b8b226fd85045e746b4937a58b0111b6b7a39dd431b6295bd6b6d05e01e225586b4dab3cbb87a" ])
    , ("Keccak-512", HashAlg Keccak_512, [
        "0eab42de4c3ceb9235fc91acffe746b29c29a8c366b7c60e4e67c466f36a4304c00fa9caf9d87976ba469bcbe06713b435f091ef2769fb160cdab33d3670680e",
        "d135bb84d0439dbac432247ee573a23ea7d3c9deb2a968eb31d47c4fb45f1ef4422d6c531b5b9bd6f449ebcc449ea94d0a8f05f62130fda612da53c79659f609",
        "10f8caabb5b179861da5e447d34b84d604e3eb81830880e1c2135ffc94580a47cb21f6243ec0053d58b1124d13af2090033659075ee718e0f111bb3f69fb24cf" ])
    , ("SHA3-224", HashAlg SHA3_224, [
        "6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7",
        "d15dadceaa4d5d7bb3b48f446421d542e08ad8887305e28d58335795",
        "b770eb6ac3ac52bd2f9e8dc186d6b604e7c3b7ffc8bd9220b0078ced" ])
    , ("SHA3-256", HashAlg SHA3_256, [
        "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a",
        "69070dda01975c8c120c3aada1b282394e7f032fa9cf32f4cb2259a0897dfc04",
        "cc80b0b13ba89613d93f02ee7ccbe72ee26c6edfe577f22e63a1380221caedbc" ])
    , ("SHA3-384", HashAlg SHA3_384, [
        "0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004",
        "7063465e08a93bce31cd89d2e3ca8f602498696e253592ed26f07bf7e703cf328581e1471a7ba7ab119b1a9ebdf8be41",
        "e414797403c7d01ab64b41e90df4165d59b7f147e4292ba2da336acba242fd651949eb1cfff7e9012e134b40981842e1" ])
    , ("SHA3-512", HashAlg SHA3_512, [
        "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26",
        "01dedd5de4ef14642445ba5f5b97c15e47b9ad931326e4b0727cd94cefc44fff23f07bf543139939b49128caf436dc1bdee54fcb24023a08d9403f9b4bf0d450",
        "28e361fe8c56e617caa56c28c7c36e5c13be552b77081be82b642f08bb7ef085b9a81910fe98269386b9aacfd2349076c9506126e198f6f6ad44c12017ca77b1" ])
    , ("Blake2b-160", HashAlg Blake2b_160, [
        "3345524abf6bbe1809449224b5972c41790b6cf2",
        "3c523ed102ab45a37d54f5610d5a983162fde84f",
        "a3d365b5fba5d36fbb19c03b7fde496058969c5a" ])
    , ("Blake2b-224", HashAlg Blake2b_224, [
        "836cc68931c2e4e3e838602eca1902591d216837bafddfe6f0c8cb07",
        "477c3985751dd4d1b8c93827ea5310b33bb02a26463a050dffd3e857",
        "a4a1b6851be66891a3deff406c4d7556879ebf952407450755f90eb6" ])
    , ("Blake2b-256", HashAlg Blake2b_256, [
        "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        "01718cec35cd3d796dd00020e0bfecb473ad23457d063b75eff29c0ffa2e58a9",
        "036c13096926b3dfccfe3f233bd1b2f583b818b8b15c01be65af69238e900b2c" ])
    , ("Blake2b-384", HashAlg Blake2b_384, [
        "b32811423377f52d7862286ee1a72ee540524380fda1724a6f25d7978c6fd3244a6caf0498812673c5e05ef583825100",
        "b7c81b228b6bd912930e8f0b5387989691c1cee1e65aade4da3b86a3c9f678fc8018f6ed9e2906720c8d2a3aeda9c03d",
        "927a1f297873cbe887a93b2183c4e2eba53966ba92c6db8b87029a1d8c673471d09740676cced79c5016838973f630c3" ])
    , ("Blake2b-512", HashAlg Blake2b_512, [
        "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce",
        "a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918",
        "af438eea5d8cdb209336a7e85bf58090dc21b49d823f89a7d064c119f127bd361af9c7d109edda0f0e91bdce078d1d86b8e6f25727c98f6d3bb6f50acb2dd376" ])
    , ("Blake2s-160", HashAlg Blake2s_160, [
        "354c9c33f735962418bdacb9479873429c34916f",
        "5a604fec9713c369e84b0ed68daed7d7504ef240",
        "759bef6d041bcbd861b8b51baaece6c8fffd0acf" ])
    , ("Blake2s-224", HashAlg Blake2s_224, [
        "1fa1291e65248b37b3433475b2a0dd63d54a11ecc4e3e034e7bc1ef4",
        "e4e5cb6c7cae41982b397bf7b7d2d9d1949823ae78435326e8db4912",
        "e220025fd46a9a635c3f7f60bb96a84c01019ac0817f5901e7eeaa2c" ])
    , ("Blake2s-256", HashAlg Blake2s_256, [
        "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9",
        "606beeec743ccbeff6cbcdf5d5302aa855c256c29b88c8ed331ea1a6bf3c8812",
        "94662583a600a12dff357c0a6f1b514a710ef0f587a38e8d2e4d7f67e9c81667" ])
    , ("SHAKE128_4096", HashAlg (SHAKE128 :: SHAKE128 4096), [
        "7f9c2ba4e88f827d616045507605853ed73b8093f6efbc88eb1a6eacfa66ef263cb1eea988004b93103cfb0aeefd2a686e01fa4a58e8a3639ca8a1e3f9ae57e235b8cc873c23dc62b8d260169afa2f75ab916a58d974918835d25e6a435085b2badfd6dfaac359a5efbb7bcc4b59d538df9a04302e10c8bc1cbf1a0b3a5120ea17cda7cfad765f5623474d368ccca8af0007cd9f5e4c849f167a580b14aabdefaee7eef47cb0fca9767be1fda69419dfb927e9df07348b196691abaeb580b32def58538b8d23f87732ea63b02b4fa0f4873360e2841928cd60dd4cee8cc0d4c922a96188d032675c8ac850933c7aff1533b94c834adbb69c6115bad4692d8619f90b0cdf8a7b9c264029ac185b70b83f2801f2f4b3f70c593ea3aeeb613a7f1b1de33fd75081f592305f2e4526edc09631b10958f464d889f31ba010250fda7f1368ec2967fc84ef2ae9aff268e0b1700affc6820b523a3d917135f2dff2ee06bfe72b3124721d4a26c04e53a75e30e73a7a9c4a95d91c55d495e9f51dd0b5e9d83c6d5e8ce803aa62b8d654db53d09b8dcff273cdfeb573fad8bcd45578bec2e770d01efde86e721a3f7c6cce275dabe6e2143f1af18da7efddc4c7b70b5e345db93cc936bea323491ccb38a388f546a9ff00dd4e1300b9b2153d2041d205b443e41b45a653f2a5c4492c1add544512dda2529833462b71a41a45be97290b6f",
        "f4202e3c5852f9182a0430fd8144f0a74b95e7417ecae17db0f8cfeed0e3e66eb5585ec6f86021cacf272c798bcf97d368b886b18fec3a571f096086a523717a3732d50db2b0b7998b4117ae66a761ccf1847a1616f4c07d5178d0d965f9feba351420f8bfb6f5ab9a0cb102568eabf3dfa4e22279f8082dce8143eb78235a1a54914ab71abb07f2f3648468370b9fbb071e074f1c030a4030225f40c39480339f3dc71d0f04f71326de1381674cc89e259e219927fae8ea2799a03da862a55afafe670957a2af3318d919d0a3358f3b891236d6a8e8d19999d1076b529968faefbd880d77bb300829dca87e9c8e4c28e0800ff37490a5bd8c36c0b0bdb2701a5d58d03378b9dbd384389e3ef0fd4003b08998fd3f32fe1a0810fc0eccaad94bca8dd83b34559c333f0b16dfc2896ed87b30ba14c81f87cd8b4bb6317db89b0e7e94c0616f9a665fba5b0e6fb3549c9d7b68e66d08a86eb2faec05cc462a771806b93cc38b0a4feb9935c6c8945da6a589891ba5ee99753cfdd38e1abc7147fd74b7c7d1ce0609b6680a2e18888d84949b6e6cf6a2aa4113535aaee079459e3f257b569a9450523c41f5b5ba4b79b3ba5949140a74bb048de0657d04954bdd71dae76f61e2a1f88aecb91cfa5b36c1bf3350a798dc4dcf48628effe3a0c5340c756bd922f78d0e36ef7df12ce78c179cc721ad087e15ea496bf5f60b21b5822d",
        "22fd225fb8f2c8d0d2097e1f8d38b6a9e619d39664dad3795f0336fda544d305e1be56a9953ecd6e4bec14b622f53da492eccbe257b9af84775a6bdf81aab6b1ba3492149b7ce4ea402c56e343939f78b9a9727193269798420c9323a9eb1fa63006e1482fcf15e3696aa1ae5cb66eb8aad4ac6041ca0b576b78785ad95bc5fb6f8a420ba9e1726552c0f2b97050ae69fc018d3f63b3a812a2d39ef64b8ae368472dec4341511b02cf77363c74f216055d5905412bc2bf57b4010b1bdce2882cb28fc7e4d470ed05219fc4d1ce11d11e10db369c807cd71891653638956b2f9d176e116188aff2fbb8519d9ad8c3fb52fa8bb4a0413364e89d9f9d7db627f2a2288babedcc314a19e45c6b7fb93b16a15d9953ff619ab4d523c481552588f711b450f854c858ebcb8cf49492d6240a753bde2109c486cac666bdba767c6e9254cc723f67855534c34d08ed486c67c1b8dd288c6010ce8e6c1c9b7f927acf4d71ee99729cee55ecec544245d0b51b31dd78eb99c2723e8ba5cf92ac7720e8933d9fd3596b90073f6980c5ed3f1cbfada26bdb6946e72391198e3d1cedebdba092324500e32b8e04e32550f6dd6c4befafa95acc206c5708300d2a8df2765751102f9738a1449c4c0d588f0076d0c1a5ee445eb85c97ada5837d5dc1d34ea081c8411d64a4d9ce9279bfe9feb25696cf741c705ed46f171e2239216d6c45dc8d15" ])
    , ("SHAKE256_4096", HashAlg (SHAKE256 :: SHAKE256 4096), [
        "46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762fd75dc4ddd8c0f200cb05019d67b592f6fc821c49479ab48640292eacb3b7c4be141e96616fb13957692cc7edd0b45ae3dc07223c8e92937bef84bc0eab862853349ec75546f58fb7c2775c38462c5010d846c185c15111e595522a6bcd16cf86f3d122109e3b1fdd943b6aec468a2d621a7c06c6a957c62b54dafc3be87567d677231395f6147293b68ceab7a9e0c58d864e8efde4e1b9a46cbe854713672f5caaae314ed9083dab4b099f8e300f01b8650f1f4b1d8fcf3f3cb53fb8e9eb2ea203bdc970f50ae55428a91f7f53ac266b28419c3778a15fd248d339ede785fb7f5a1aaa96d313eacc890936c173cdcd0fab882c45755feb3aed96d477ff96390bf9a66d1368b208e21f7c10d04a3dbd4e360633e5db4b602601c14cea737db3dcf722632cc77851cbdde2aaf0a33a07b373445df490cc8fc1e4160ff118378f11f0477de055a81a9eda57a4a2cfb0c83929d310912f729ec6cfa36c6ac6a75837143045d791cc85eff5b21932f23861bcf23a52b5da67eaf7baae0f5fb1369db78f3ac45f8c4ac5671d85735cdddb09d2b1e34a1fc066ff4a162cb263d6541274ae2fcc865f618abe27c124cd8b074ccd516301b91875824d09958f341ef274bdab0bae316339894304e35877b0c28a9b1fd166c796b9cc258a064a8f57e27f2a",
        "2f671343d9b2e1604dc9dcf0753e5fe15c7c64a0d283cbbf722d411a0e36f6ca1d01d1369a23539cd80f7c054b6e5daf9c962cad5b8ed5bd11998b40d5734442bed798f6e5c915bd8bb07e0188d0a55c1290074f1c287af06352299184492cbdec9acba737ee292e5adaa445547355e72a03a3bac3aac770fe5d6b66600ff15d37d5b4789994ea2aeb097f550aa5e88e4d8ff0ba07b88c1c88573063f5d96df820abc2abd177ab037f351c375e553af917132cf2f563c79a619e1bb76e8e2266b0c5617d695f2c496a25f4073b6840c1833757ebb386f16757a8e16a21e9355e9b248f3b33be672da700266be99b8f8725e8ab06075f0219e655ebc188976364b7db139390d34a6ea67b4b223229183a94cf455ece91fdaf5b9c707fa4b40ec39816c1120c7aaaf47920977be900e6b9ca4b8940e192b927c475bd58e836f512ae3e52924e36ff8e9b1d0251047770a5e465905622b1f159be121ab93819c5e5c6dae299ac73bf1c4ed4a1e2c7fa3caa1039b05e94c9f993d04feb272b6e00bb0276939cf746c42936831fc8f2b4cb0cf94808ae0af405ce4bc67d1e7acfc6fd6590d3de91f795df5aaf57e2cee1845a303d0ea564be3f1299acdce67efe0d62cfc6d6829ff4ecc0a05153c24696c4d34c076453827e796f3062f94f62f4528b7cfc870f0dcd615b7c97b95da4b9be5830e8b3f66cce71e0f622c771994443e2",
        "fffcaac0606c0edb7bc0d15f033accb68538159016e5ae8470bf9ebea89fa6c9fcc3e027d94f7f967b7246346bd9f6b8084e45a057b976847c4db03bf383c834054866f6a8282a497368c46e1852fc09e20f22c45607a27c8b2a4798ebefada54f8d3795b9f07606b1cd6e41f90d765480ef5c0d5790659cf1d210adfd412378b92e1dd9bd7fd95a1a66677fc6baa0e3a53c9031c1fb59cbad9f5dc5881a3c8e25c80ecb1abf0971488ada1f533dcbf8d37031335378574b8d3fad61159c9fae28caa543b3072ce308d369be340e78c6edc664cc6dde9b2f0a4ad2e60ce9c8b1e5722b8d5b73d0962b74fb9ed86307a180f53933339f9d56d3b345c2a0e98fcf5de7754f3845f6be30089f0e142ad4602f18abdc750bda7c91c3f32872e66640db46045ab4c276b379f1b834c2cbb1bd8601305649ec6b3bf20618695136dee6541492d1d985ea1fb765fd7a559e810eba30f2f710233ae5a411b94ddcaa01a08f1c31320d111c0714422cd5e987c9a76fc865de34003ab12664081be8017d23d977f2bf4ed9e3ce09ea3d64bb4ae8ebfa9d0721f57841008c297e2f455a0441a2bd618ca379dbd239a21e410defb4001b1e11f87e36bf894c222f76f12ddcc3771bbb17d5c0dfd86d89a3e13e084f6dc1c4762bcd393c1757db7afb1434221569e7ddaaffd6318253ec3df8cf5f826b81896d6474ee06a2e30ccc8c6a96bdd5" ])
    , ("Blake2b 160", HashAlg (Blake2b :: Blake2b 160), [
        "3345524abf6bbe1809449224b5972c41790b6cf2",
        "3c523ed102ab45a37d54f5610d5a983162fde84f",
        "a3d365b5fba5d36fbb19c03b7fde496058969c5a" ])
    , ("Blake2b 224", HashAlg (Blake2b :: Blake2b 224), [
        "836cc68931c2e4e3e838602eca1902591d216837bafddfe6f0c8cb07",
        "477c3985751dd4d1b8c93827ea5310b33bb02a26463a050dffd3e857",
        "a4a1b6851be66891a3deff406c4d7556879ebf952407450755f90eb6" ])
    , ("Blake2b 256", HashAlg (Blake2b :: Blake2b 256), [
        "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        "01718cec35cd3d796dd00020e0bfecb473ad23457d063b75eff29c0ffa2e58a9",
        "036c13096926b3dfccfe3f233bd1b2f583b818b8b15c01be65af69238e900b2c" ])
    , ("Blake2b 384", HashAlg (Blake2b :: Blake2b 384), [
        "b32811423377f52d7862286ee1a72ee540524380fda1724a6f25d7978c6fd3244a6caf0498812673c5e05ef583825100",
        "b7c81b228b6bd912930e8f0b5387989691c1cee1e65aade4da3b86a3c9f678fc8018f6ed9e2906720c8d2a3aeda9c03d",
        "927a1f297873cbe887a93b2183c4e2eba53966ba92c6db8b87029a1d8c673471d09740676cced79c5016838973f630c3" ])
    , ("Blake2b 512", HashAlg (Blake2b :: Blake2b 512), [
        "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce",
        "a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918",
        "af438eea5d8cdb209336a7e85bf58090dc21b49d823f89a7d064c119f127bd361af9c7d109edda0f0e91bdce078d1d86b8e6f25727c98f6d3bb6f50acb2dd376" ])
    , ("Blake2s 160", HashAlg (Blake2s :: Blake2s 160), [
        "354c9c33f735962418bdacb9479873429c34916f",
        "5a604fec9713c369e84b0ed68daed7d7504ef240",
        "759bef6d041bcbd861b8b51baaece6c8fffd0acf" ])
    , ("Blake2s 224", HashAlg (Blake2s :: Blake2s 224), [
        "1fa1291e65248b37b3433475b2a0dd63d54a11ecc4e3e034e7bc1ef4",
        "e4e5cb6c7cae41982b397bf7b7d2d9d1949823ae78435326e8db4912",
        "e220025fd46a9a635c3f7f60bb96a84c01019ac0817f5901e7eeaa2c" ])
    , ("Blake2s 256", HashAlg (Blake2s ::Blake2s 256), [
        "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9",
        "606beeec743ccbeff6cbcdf5d5302aa855c256c29b88c8ed331ea1a6bf3c8812",
        "94662583a600a12dff357c0a6f1b514a710ef0f587a38e8d2e4d7f67e9c81667" ])
    ]

runhash :: HashAlg -> ByteString -> ByteString
runhash (HashAlg hashAlg) v = B.convertToBase B.Base16 $ hashWith hashAlg $ v

runhashinc :: HashAlg -> [ByteString] -> ByteString
runhashinc (HashAlg hashAlg) v = B.convertToBase B.Base16 $ hashinc $ v
  where hashinc = hashFinalize . foldl hashUpdate (hashInitWith hashAlg)

data HashPrefixAlg = forall alg . HashAlgorithmPrefix alg => HashPrefixAlg alg

expectedPrefix :: [ (String, HashPrefixAlg) ]
expectedPrefix =
    [ ("MD5", HashPrefixAlg MD5)
    , ("SHA1", HashPrefixAlg SHA1)
    , ("SHA224", HashPrefixAlg SHA224)
    , ("SHA256", HashPrefixAlg SHA256)
    , ("SHA384", HashPrefixAlg SHA384)
    , ("SHA512", HashPrefixAlg SHA512)
    ]

runhashpfx :: HashPrefixAlg -> ByteString -> ByteString
runhashpfx (HashPrefixAlg hashAlg) v = B.convertToBase B.Base16 $ hashWith hashAlg v

runhashpfxpfx :: HashPrefixAlg -> ByteString -> Int -> ByteString
runhashpfxpfx (HashPrefixAlg hashAlg) v len = B.convertToBase B.Base16 $ hashPrefixWith hashAlg v len

makeTestAlg (name, hashAlg, results) =
    testGroup name $ concatMap maketest (zip3 is vectors results)
  where
        is :: [Int]
        is = [1..]

        maketest (i, v, r) =
            [ testCase (show i) (r @=? runhash hashAlg v)
            ]

makeTestChunk (hashName, hashAlg, _) =
    [ testProperty hashName $ \ckLen (ArbitraryBS0_2901 inp) ->
        runhash hashAlg inp `propertyEq` runhashinc hashAlg (chunkS ckLen inp)
    ]

makeTestPrefix (hashName, hashAlg) =
    [ testProperty hashName $ \(ArbitraryBS0_2901 inp) (Int0_2901 len) ->
        runhashpfx hashAlg (B.take len inp) `propertyEq` runhashpfxpfx hashAlg inp len
    ]

makeTestHybrid (hashName, HashPrefixAlg alg) =
    [ testProperty hashName $ \(ArbitraryBS0_2901 start) (ArbitraryBS0_2901 end) -> do
        len <- choose (0, B.length end)
        let ref = hashWith alg (start `B.append` B.take len end)
            hyb = hashFinalizePrefix (hashUpdate (hashInitWith alg) start) end len
        return (ref `propertyEq` hyb)
    ]

-- SHAKE128 truncation example with expected byte at final position
-- <https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/ShakeTruncation.pdf>
shake128TruncationBytes = [0x01, 0x03, 0x07, 0x0f, 0x0f, 0x2f, 0x6f, 0x6f]

makeTestSHAKE128Truncation i byte =
    testCase (show i) $ xof 4088 `B.snoc` byte @=? xof (4088 + i)
  where
    hashEmpty :: KnownNat n => proxy n -> Digest (SHAKE128 n)
    hashEmpty _ = hash B.empty

    xof n = case someNatVal n of
                Nothing          -> error ("invalid Nat: " ++ show n)
                Just (SomeNat p) -> convert (hashEmpty p)

tests = testGroup "hash"
    [ testGroup "KATs" (map makeTestAlg expected)
    , testGroup "Chunking" (concatMap makeTestChunk expected)
    , testGroup "Prefix" (concatMap makeTestPrefix expectedPrefix)
    , testGroup "Hybrid" (concatMap makeTestHybrid expectedPrefix)
    , testGroup "Truncating"
        [ testGroup "SHAKE128"
            (zipWith makeTestSHAKE128Truncation [1..] shake128TruncationBytes)
        ]
    ]
