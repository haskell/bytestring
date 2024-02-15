{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.ByteString.Builder.RealFloat.D2S
-- Copyright   : (c) Lawrence Wu 2021
-- License     : BSD-style
-- Maintainer  : lawrencejwu@gmail.com
--
-- Implementation of double-to-string conversion

module Data.ByteString.Builder.RealFloat.D2S
    ( FloatingDecimal(..)
    , d2s
    , d2Intermediate
    ) where

import Control.Arrow (first)
import Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)
import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.RealFloat.Internal
import Data.Maybe (fromMaybe)
import GHC.Int (Int32(..))
import GHC.Word (Word64(..))

#if !PURE_HASKELL
import GHC.Ptr (Ptr(..))
#endif

-- See Data.ByteString.Builder.RealFloat.TableGenerator for a high-level
-- explanation of the ryu algorithm

#if !PURE_HASKELL
-- | Table of 2^k / 5^q + 1
--
-- > splitWord128s $ fmap (finv double_pow5_inv_bitcount) [0..double_max_inv_split]
foreign import ccall "&hs_bytestring_double_pow5_inv_split"
  double_pow5_inv_split :: Ptr Word64

-- | Table of 5^(-e2-q) / 2^k + 1
--
-- > splitWord128s $ fmap (fnorm double_pow5_bitcount) [0..double_max_split]
foreign import ccall "&hs_bytestring_double_pow5_split"
  double_pow5_split :: Ptr Word64
#endif

-- | Number of mantissa bits of a 64-bit float. The number of significant bits
-- (floatDigits (undefined :: Double)) is 53 since we have a leading 1 for
-- normal floats and 0 for subnormal floats
double_mantissa_bits :: Int
double_mantissa_bits = 52

-- | Number of exponent bits of a 64-bit float
double_exponent_bits :: Int
double_exponent_bits = 11

-- | Bias in encoded 64-bit float representation (2^10 - 1)
double_bias :: Int
double_bias = 1023

data FloatingDecimal = FloatingDecimal
  { dmantissa :: !Word64
  , dexponent :: !Int32
  } deriving (Show, Eq)

-- | Quick check for small integers
d2dSmallInt :: Word64 -> Word64 -> Maybe FloatingDecimal
d2dSmallInt m e =
  let m2 = (1 `unsafeShiftL` double_mantissa_bits) .|. m
      e2 = word64ToInt e - (double_bias + double_mantissa_bits)
      fraction = m2 .&. mask (-e2)
   in case () of
        _ -- f = m2 * 2^e2 >= 2^53 is an integer.
          -- Ignore this case for now.
          | e2 > 0 -> Nothing
          -- f < 1
          | e2 < -52 -> Nothing
          -- Since 2^52 <= m2 < 2^53 and 0 <= -e2 <= 52:
          --    1 <= f = m2 / 2^-e2 < 2^53.
          -- Test if the lower -e2 bits of the significand are 0, i.e.
          -- whether the fraction is 0.
          | fraction /= 0 -> Nothing
          -- f is an integer in the range [1, 2^53).
          -- Note: mantissa might contain trailing (decimal) 0's.
          -- Note: since 2^53 < 10^16, there is no need to adjust decimalLength17().
          | otherwise -> Just $ FloatingDecimal (m2 `unsafeShiftR` (-e2)) 0


-- | Removes trailing (decimal) zeros for small integers in the range [1, 2^53)
unifySmallTrailing :: FloatingDecimal -> FloatingDecimal
unifySmallTrailing fd@(FloatingDecimal m e) =
  let !(q, r) = dquotRem10 m
   in if r == 0
        then unifySmallTrailing $ FloatingDecimal q (e + 1)
        else fd

-- TODO: 128-bit intrinsics
-- | Multiply a 64-bit number with a 128-bit number while keeping the upper 64
-- bits. Then shift by specified amount minus 64
mulShift64 :: Word64 -> (Word64, Word64) -> Int -> Word64
mulShift64 m (factorHi, factorLo) shift =
  let !(b0Hi, _   ) = m `timesWord2` factorLo
      !(b1Hi, b1Lo) = m `timesWord2` factorHi
      total = b0Hi + b1Lo
      high  = b1Hi + boolToWord64 (total < b0Hi)
      dist  = shift - 64
   in (high `unsafeShiftL` (64 - dist)) .|. (total `unsafeShiftR` dist)

-- | Index into the 128-bit word lookup table double_pow5_inv_split
get_double_pow5_inv_split :: Int -> (Word64, Word64)
#if !PURE_HASKELL
get_double_pow5_inv_split = getWord128At double_pow5_inv_split
#else
-- > putStr $ case128 (finv double_pow5_inv_bitcount) [0..double_max_inv_split]
get_double_pow5_inv_split i = case i of
  0 -> (0x2000000000000000, 0x1)
  1 -> (0x1999999999999999, 0x999999999999999a)
  2 -> (0x147ae147ae147ae1, 0x47ae147ae147ae15)
  3 -> (0x10624dd2f1a9fbe7, 0x6c8b4395810624de)
  4 -> (0x1a36e2eb1c432ca5, 0x7a786c226809d496)
  5 -> (0x14f8b588e368f084, 0x61f9f01b866e43ab)
  6 -> (0x10c6f7a0b5ed8d36, 0xb4c7f34938583622)
  7 -> (0x1ad7f29abcaf4857, 0x87a6520ec08d236a)
  8 -> (0x15798ee2308c39df, 0x9fb841a566d74f88)
  9 -> (0x112e0be826d694b2, 0xe62d01511f12a607)
  10 -> (0x1b7cdfd9d7bdbab7, 0xd6ae6881cb5109a4)
  11 -> (0x15fd7fe17964955f, 0xdef1ed34a2a73aea)
  12 -> (0x119799812dea1119, 0x7f27f0f6e885c8bb)
  13 -> (0x1c25c268497681c2, 0x650cb4be40d60df8)
  14 -> (0x16849b86a12b9b01, 0xea70909833de7193)
  15 -> (0x1203af9ee756159b, 0x21f3a6e0297ec143)
  16 -> (0x1cd2b297d889bc2b, 0x6985d7cd0f313537)
  17 -> (0x170ef54646d49689, 0x2137dfd73f5a90f9)
  18 -> (0x12725dd1d243aba0, 0xe75fe645cc4873fa)
  19 -> (0x1d83c94fb6d2ac34, 0xa5663d3c7a0d865d)
  20 -> (0x179ca10c9242235d, 0x511e976394d79eb1)
  21 -> (0x12e3b40a0e9b4f7d, 0xda7edf82dd794bc1)
  22 -> (0x1e392010175ee596, 0x2a6498d1625bac68)
  23 -> (0x182db34012b25144, 0xeeb6e0a781e2f053)
  24 -> (0x1357c299a88ea76a, 0x58924d52ce4f26a9)
  25 -> (0x1ef2d0f5da7dd8aa, 0x27507bb7b07ea441)
  26 -> (0x18c240c4aecb13bb, 0x52a6c95fc0655034)
  27 -> (0x13ce9a36f23c0fc9, 0xeebd44c99eaa690)
  28 -> (0x1fb0f6be50601941, 0xb17953adc3110a80)
  29 -> (0x195a5efea6b34767, 0xc12ddc8b02740867)
  30 -> (0x14484bfeebc29f86, 0x3424b06f3529a052)
  31 -> (0x1039d66589687f9e, 0x901d59f290ee19db)
  32 -> (0x19f623d5a8a73297, 0x4cfbc31db4b0295f)
  33 -> (0x14c4e977ba1f5bac, 0x3d9635b15d59bab2)
  34 -> (0x109d8792fb4c4956, 0x97ab5e277de16228)
  35 -> (0x1a95a5b7f87a0ef0, 0xf2abc9d8c9689d0d)
  36 -> (0x154484932d2e725a, 0x5bbca17a3aba173e)
  37 -> (0x11039d428a8b8eae, 0xafca1ac82efb45cb)
  38 -> (0x1b38fb9daa78e44a, 0xb2dcf7a6b1920945)
  39 -> (0x15c72fb1552d836e, 0xf57d92ebc141a104)
  40 -> (0x116c262777579c58, 0xc46475896767b403)
  41 -> (0x1be03d0bf225c6f4, 0x6d6d88dbd8a5ecd2)
  42 -> (0x164cfda3281e38c3, 0x8abe071646eb23db)
  43 -> (0x11d7314f534b609c, 0x6efe6c11d255b649)
  44 -> (0x1c8b821885456760, 0xb197134fb6ef8a0e)
  45 -> (0x16d601ad376ab91a, 0x27ac0f72f8bfa1a5)
  46 -> (0x1244ce242c5560e1, 0xb95672c260994e1e)
  47 -> (0x1d3ae36d13bbce35, 0xf5571e03cdc21695)
  48 -> (0x17624f8a762fd82b, 0x2aac18030b01abab)
  49 -> (0x12b50c6ec4f31355, 0xbbbce0026f348956)
  50 -> (0x1dee7a4ad4b81eef, 0x92c7ccd0b1eda889)
  51 -> (0x17f1fb6f10934bf2, 0xdbd30a408e57ba07)
  52 -> (0x1327fc58da0f6ff5, 0x7ca8d50071dfc806)
  53 -> (0x1ea6608e29b24cbb, 0xfaa7bb33e9660cd6)
  54 -> (0x18851a0b548ea3c9, 0x9552fc298784d711)
  55 -> (0x139dae6f76d88307, 0xaaa8c9bad2d0ac0e)
  56 -> (0x1f62b0b257c0d1a5, 0xdddadc5e1e1aace3)
  57 -> (0x191bc08eac9a4151, 0x7e48b04b4b488a4f)
  58 -> (0x141633a556e1cdda, 0xcb6d59d5d5d3a1d9)
  59 -> (0x1011c2eaabe7d7e2, 0x3c577b1177dc817b)
  60 -> (0x19b604aaaca62636, 0xc6f25e825960cf2a)
  61 -> (0x14919d5556eb51c5, 0x6bf518684780a5bb)
  62 -> (0x10747ddddf22a7d1, 0x232a79ed06008496)
  63 -> (0x1a53fc9631d10c81, 0xd1dd8fe1a3340756)
  64 -> (0x150ffd44f4a73d34, 0xa7e4731ae8f66c45)
  65 -> (0x10d9976a5d52975d, 0x531d28e253f8569e)
  66 -> (0x1af5bf109550f22e, 0xeb61db03b98d5762)
  67 -> (0x159165a6ddda5b58, 0xbc4e48cfc7a445e8)
  68 -> (0x11411e1f17e1e2ad, 0x6371d3d96c836b20)
  69 -> (0x1b9b6364f3030448, 0x9f1c8628ad9f11cd)
  70 -> (0x1615e91d8f359d06, 0xe5b06b53be18db0b)
  71 -> (0x11ab20e472914a6b, 0xeaf3890fcb4715a2)
  72 -> (0x1c45016d841baa46, 0x44b8db4c7871bc37)
  73 -> (0x169d9abe03495505, 0x3c715d6c6c1635f)
  74 -> (0x1217aefe69077737, 0x3638de456bcde919)
  75 -> (0x1cf2b1970e725858, 0x56c163a2461641c1)
  76 -> (0x17288e1271f51379, 0xdf011c81d1ab67ce)
  77 -> (0x1286d80ec190dc61, 0x7f3416ce4155eca5)
  78 -> (0x1da48ce468e7c702, 0x6520247d3556476e)
  79 -> (0x17b6d71d20b96c01, 0xea801d30f7783925)
  80 -> (0x12f8ac174d612334, 0xbb99b0f3f92cfa84)
  81 -> (0x1e5aacf215683854, 0x5f5c4e532847f739)
  82 -> (0x18488a5b44536043, 0x7f7d0b75b9d32c2e)
  83 -> (0x136d3b7c36a919cf, 0x9930d5f7c7dc2358)
  84 -> (0x1f152bf9f10e8fb2, 0x8eb4898c72f9d226)
  85 -> (0x18ddbcc7f40ba628, 0x722a07a38f2e41b8)
  86 -> (0x13e497065cd61e86, 0xc1bb394fa5be9afa)
  87 -> (0x1fd424d6faf030d7, 0x9c5ec2190930f7f6)
  88 -> (0x197683df2f268d79, 0x49e56814075a5ff8)
  89 -> (0x145ecfe5bf520ac7, 0x6e51201005e1e660)
  90 -> (0x104bd984990e6f05, 0xf1da800cd181851a)
  91 -> (0x1a12f5a0f4e3e4d6, 0x4fc400148268d4f5)
  92 -> (0x14dbf7b3f71cb711, 0xd96999aa01ed772b)
  93 -> (0x10aff95cc5b09274, 0xadee1488018ac5bc)
  94 -> (0x1ab328946f80ea54, 0x497ceda668de092c)
  95 -> (0x155c2076bf9a5510, 0x3aca57b853e4d424)
  96 -> (0x1116805effaeaa73, 0x623b7960431d7683)
  97 -> (0x1b5733cb32b110b8, 0x9d2bf566d1c8bd9e)
  98 -> (0x15df5ca28ef40d60, 0x7dbcc452416d647f)
  99 -> (0x117f7d4ed8c33de6, 0xcafd69db678ab6cc)
  100 -> (0x1bff2ee48e052fd7, 0xab2f0fc572778adf)
  101 -> (0x1665bf1d3e6a8cac, 0x88f273045b92d580)
  102 -> (0x11eaff4a98553d56, 0xd3f528d049424466)
  103 -> (0x1cab3210f3bb9557, 0xb988414d4203a0a3)
  104 -> (0x16ef5b40c2fc7779, 0x6139cdd76802e6e9)
  105 -> (0x125915cd68c9f92d, 0xe761717920025254)
  106 -> (0x1d5b561574765b7c, 0xa568b58e999d5086)
  107 -> (0x177c44ddf6c515fd, 0x5120913ee14aa6d2)
  108 -> (0x12c9d0b1923744ca, 0xa74d40ff1aa21f0e)
  109 -> (0x1e0fb44f50586e11, 0xbaece64f769cb4a)
  110 -> (0x180c903f7379f1a7, 0x3c8bd850c5ee3c3b)
  111 -> (0x133d4032c2c7f485, 0xca0979da37f1c9c9)
  112 -> (0x1ec866b79e0cba6f, 0xa9a8c2f6bfe942db)
  113 -> (0x18a0522c7e709526, 0x2153cf2bccba9be3)
  114 -> (0x13b374f06526ddb8, 0x1aa9728970954982)
  115 -> (0x1f8587e7083e2f8c, 0xf775840f1a88759d)
  116 -> (0x19379fec0698260a, 0x5f9136727ba05e17)
  117 -> (0x142c7ff0054684d5, 0x1940f85b9619e4df)
  118 -> (0x1023998cd1053710, 0xe100c6afab47ea4c)
  119 -> (0x19d28f47b4d524e7, 0xce67a44c453fdd47)
  120 -> (0x14a8729fc3ddb71f, 0xd852e9d69dccb106)
  121 -> (0x1086c219697e2c19, 0x79dbee454b0a2738)
  122 -> (0x1a71368f0f30468f, 0x295fe3a211a9d859)
  123 -> (0x15275ed8d8f36ba5, 0xbab31c81a7bb137a)
  124 -> (0x10ec4be0ad8f8951, 0x6228e39aec95a92f)
  125 -> (0x1b13ac9aaf4c0ee8, 0x9d0e38f7e0ef7517)
  126 -> (0x15a956e225d67253, 0xb0d82d931a592a79)
  127 -> (0x11544581b7dec1dc, 0x8d79be0f4847552e)
  128 -> (0x1bba08cf8c979c94, 0x158f967eda0bbb7c)
  129 -> (0x162e6d72d6dfb076, 0x77a611ff14d62f97)
  130 -> (0x11bebdf578b2f391, 0xf951a7ff43de8c79)
  131 -> (0x1c6463225ab7ec1c, 0xc21c3ffed2fdad8e)
  132 -> (0x16b6b5b5155ff017, 0x1b0333242648ad8)
  133 -> (0x122bc490dde659ac, 0x159c28e9b83a246)
  134 -> (0x1d12d41afca3c2ac, 0xcef604175f3903a3)
  135 -> (0x17424348ca1c9bbd, 0x725e69ac4c2d9c83)
  136 -> (0x129b69070816e2fd, 0xf5185489d68ae39c)
  137 -> (0x1dc574d80cf16b2f, 0xee8d540fbdab05c6)
  138 -> (0x17d12a4670c1228c, 0xbed77672fe226b05)
  139 -> (0x130dbb6b8d674ed6, 0xff12c528cb4ebc04)
  140 -> (0x1e7c5f127bd87e24, 0xcb513b74787df9a0)
  141 -> (0x18637f41fcad31b7, 0x90dc929f9fe614d)
  142 -> (0x1382cc34ca2427c5, 0xa0d7d42194cb810a)
  143 -> (0x1f37ad21436d0c6f, 0x67bfb9cf5478ce77)
  144 -> (0x18f9574dcf8a7059, 0x1fcc94a5dd2d71f9)
  145 -> (0x13faac3e3fa1f37a, 0x7fd6dd517dbdf4c7)
  146 -> (0x1ff779fd329cb8c3, 0xffbe2ee8c92fee0b)
  147 -> (0x1992c7fdc216fa36, 0x6631bf20a0f324d6)
  148 -> (0x14756ccb01abfb5e, 0xb827cc1a1a5c1d78)
  149 -> (0x105df0a267bcc918, 0x935309ae7b7ce460)
  150 -> (0x1a2fe76a3f9474f4, 0x1eeb42b0c594a099)
  151 -> (0x14f31f8832dd2a5c, 0xe58902270476e6e1)
  152 -> (0x10c27fa028b0eeb0, 0xb7a0ce859d2bebe7)
  153 -> (0x1ad0cc33744e4ab4, 0x59014a6f61dfdfd8)
  154 -> (0x1573d68f903ea229, 0xe0cdd525e7e64cad)
  155 -> (0x11297872d9cbb4ee, 0x4d7177518651d6f1)
  156 -> (0x1b758d848fac54b0, 0x7be8bee8d6e957e8)
  157 -> (0x15f7a46a0c89dd59, 0xfcba3253df211320)
  158 -> (0x1192e9ee706e4aae, 0x63c8284318e74280)
  159 -> (0x1c1e43171a4a1117, 0x60d0d3827d86a66)
  160 -> (0x167e9c127b6e7412, 0x6b3da42cecad21eb)
  161 -> (0x11fee341fc585cdb, 0x88fe1cf0bd574e56)
  162 -> (0x1ccb0536608d615f, 0x419694b462254a23)
  163 -> (0x1708d0f84d3de77f, 0x67abaa29e81dd4e9)
  164 -> (0x126d73f9d764b932, 0xb95621bb2017dd87)
  165 -> (0x1d7becc2f23ac1ea, 0xc223692b668c95a5)
  166 -> (0x179657025b6234bb, 0xce82ba891ed6de1d)
  167 -> (0x12deac01e2b4f6fc, 0xa53562074bdf1818)
  168 -> (0x1e3113363787f194, 0x3b889cd87964f359)
  169 -> (0x18274291c6065adc, 0xfc6d4a46c783f5e1)
  170 -> (0x13529ba7d19eaf17, 0x30576e9f06032b1a)
  171 -> (0x1eea92a61c311825, 0x1a257dcb3cd1de90)
  172 -> (0x18bba884e35a79b7, 0x481dfe3c30a7e540)
  173 -> (0x13c9539d82aec7c5, 0xd34b31c9c0865100)
  174 -> (0x1fa885c8d117a609, 0x5211e942cda3b4cd)
  175 -> (0x19539e3a40dfb807, 0x74db21023e1c90a4)
  176 -> (0x1442e4fb67196005, 0xf715b401cb4a0d50)
  177 -> (0x103583fc527ab337, 0xf8de299b09080aa7)
  178 -> (0x19ef3993b72ab859, 0x8e304291a80cddd7)
  179 -> (0x14bf6142f8eef9e1, 0x3e8d020e200a4b13)
  180 -> (0x10991a9bfa58c7e7, 0x653d9b3e80083c0f)
  181 -> (0x1a8e90f9908e0ca5, 0x6ec8f864000d2ce4)
  182 -> (0x153eda614071a3b7, 0x8bd3f9e999a423ea)
  183 -> (0x10ff151a99f482f9, 0x3ca994bae1501cbb)
  184 -> (0x1b31bb5dc320d18e, 0xc775bac49bb3612b)
  185 -> (0x15c162b168e70e0b, 0xd2c4956a16291a89)
  186 -> (0x11678227871f3e6f, 0xdbd0778811ba7ba1)
  187 -> (0x1bd8d03f3e9863e6, 0x2c80bf401c5d929b)
  188 -> (0x16470cff6546b651, 0xbd33cc3349e47549)
  189 -> (0x11d270cc51055ea7, 0xca8fd68f6e505dd4)
  190 -> (0x1c83e7ad4e6efdd9, 0x4419574be3b3c953)
  191 -> (0x16cfec8aa52597e1, 0x347790982f63aa9)
  192 -> (0x123ff06eea847980, 0xcf6c60d468c4fbba)
  193 -> (0x1d331a4b10d3f59a, 0xe57a34870e07f92a)
  194 -> (0x175c1508da432ae2, 0x512e906c0b399422)
  195 -> (0x12b010d3e1cf5581, 0xda8ba6bcd5c7a9b5)
  196 -> (0x1de6815302e5559c, 0x90df712e22d90f87)
  197 -> (0x17eb9aa8cf1dde16, 0xda4c5a8b4f140c6c)
  198 -> (0x1322e220a5b17e78, 0xaea37ba2a5a9a38a)
  199 -> (0x1e9e369aa2b59727, 0x7dd25f6aa2a905a9)
  200 -> (0x187e92154ef7ac1f, 0x97db7f888220d154)
  201 -> (0x139874ddd8c6234c, 0x797c6606ce80a777)
  202 -> (0x1f5a549627a36bad, 0x8f2d700ae4010bf1)
  203 -> (0x191510781fb5efbe, 0xc2459a25000d65a)
  204 -> (0x1410d9f9b2f7f2fe, 0x701d1481d99a4515)
  205 -> (0x100d7b2e28c65bfe, 0xc017439b147b6a77)
  206 -> (0x19af2b7d0e0a2cca, 0xccf205c4ed9243f2)
  207 -> (0x148c22ca71a1bd6f, 0xa5b37d0be0e9cc2)
  208 -> (0x10701bd527b4978c, 0x848f973cb3ee3ce)
  209 -> (0x1a4cf9550c5425ac, 0xda0e5bec78649fb0)
  210 -> (0x150a6110d6a9b7bd, 0x7b3eaff060507fc0)
  211 -> (0x10d51a73deee2c97, 0x95cbbff380406633)
  212 -> (0x1aee90b964b04758, 0xefac665266cd7052)
  213 -> (0x158ba6fab6f36c47, 0x2623850eb8a459db)
  214 -> (0x113c85955f29236c, 0x1e82d0d893b6ae49)
  215 -> (0x1b9408eefea838ac, 0xfd9e1af41f8ab075)
  216 -> (0x16100725988693bd, 0x97b1af29b2d559f7)
  217 -> (0x11a66c1e139edc97, 0xac8e25baf5777b2c)
  218 -> (0x1c3d79c9b8fe2dbf, 0x7a7d092b2258c513)
  219 -> (0x169794a160cb57cc, 0x61fda0ef4ead6a76)
  220 -> (0x1212dd4de7091309, 0xe7fe1a590bbdeec5)
  221 -> (0x1ceafbafd80e84dc, 0xa6635d5b45fcb13a)
  222 -> (0x172262f3133ed0b0, 0x851c4aaf6b308dc8)
  223 -> (0x1281e8c275cbda26, 0xd0e36ef2bc26d7d4)
  224 -> (0x1d9ca79d894629d7, 0xb49f17eac6a48c86)
  225 -> (0x17b08617a104ee46, 0x2a18dfef0550706b)
  226 -> (0x12f39e794d9d8b6b, 0x54e0b3259dd9f389)
  227 -> (0x1e5297287c2f4578, 0x87cdeb6f62f65274)
  228 -> (0x18421286c9bf6ac6, 0xd30b22bf825ea85d)
  229 -> (0x13680ed23aff889f, 0xf3c1bcc684bb9e4)
  230 -> (0x1f0ce4839198da98, 0x18602c7a4079296d)
  231 -> (0x18d71d360e13e213, 0x46b356c833942124)
  232 -> (0x13df4a91a4dcb4dc, 0x388f78a029434db6)
  233 -> (0x1fcbaa82a1612160, 0x5a7f2766a86baf8a)
  234 -> (0x196fbb9bb44db44d, 0x153285ebb9efbfa2)
  235 -> (0x145962e2f6a4903d, 0xaa8ed189618c994e)
  236 -> (0x1047824f2bb6d9ca, 0xeed8a7a11ad6e10c)
  237 -> (0x1a0c03b1df8af611, 0x7e27729b5e249b45)
  238 -> (0x14d6695b193bf80d, 0xfe85f549181d4904)
  239 -> (0x10ab877c142ff9a4, 0xcb9e5dd4134aa0d0)
  240 -> (0x1aac0bf9b9e65c3a, 0xdf63c9535211014d)
  241 -> (0x15566ffafb1eb02f, 0x191ca10f74da6771)
  242 -> (0x1111f32f2f4bc025, 0xadb080d92a4852c1)
  243 -> (0x1b4feb7eb212cd09, 0x15e7348eaa0d5134)
  244 -> (0x15d98932280f0a6d, 0xab1f5d3eee710dc4)
  245 -> (0x117ad428200c0857, 0xbc1917658b8da49d)
  246 -> (0x1bf7b9d9cce00d59, 0x2cf4f23c127c3a94)
  247 -> (0x165fc7e170b33de0, 0xf0c3f4fcdb969543)
  248 -> (0x11e6398126f5cb1a, 0x5a365d9716121103)
  249 -> (0x1ca38f350b22de90, 0x9056fc24f01ce804)
  250 -> (0x16e93f5da2824ba6, 0xd9df301d8ce3ecd0)
  251 -> (0x125432b14ecea2eb, 0xe17f59b13d8323da)
  252 -> (0x1d53844ee47dd179, 0x68cbc2b52f38395c)
  253 -> (0x177603725064a794, 0x53d6355dbf602de3)
  254 -> (0x12c4cf8ea6b6ec76, 0xa9782ab165e68b1c)
  255 -> (0x1e07b27dd78b13f1, 0xf26aab56fd744fa)
  256 -> (0x18062864ac6f4327, 0x3f52222abfdf6a62)
  257 -> (0x1338205089f29c1f, 0x65db4e88997f884e)
  258 -> (0x1ec033b40fea9365, 0x6fc54a7428cc0d4a)
  259 -> (0x1899c2f673220f84, 0x596aa1f68709a43b)
  260 -> (0x13ae3591f5b4d936, 0xadeee7f86c07b696)
  261 -> (0x1f7d228322baf524, 0x497e3ff3e00c5756)
  262 -> (0x1930e868e89590e9, 0xd464fff64cd6ac45)
  263 -> (0x14272053ed4473ee, 0x4383fff83d7889d1)
  264 -> (0x101f4d0ff1038ff1, 0xcf9cccc69793a174)
  265 -> (0x19cbae7fe805b31c, 0x7f6147a425b90252)
  266 -> (0x14a2f1ffecd15c16, 0xcc4dd2e9b7c7350f)
  267 -> (0x10825b3323dab012, 0x3d0b0f215fd290d9)
  268 -> (0x1a6a2b85062ab350, 0x61ab4b689950e7c1)
  269 -> (0x1521bc6a6b555c40, 0x4e22a2ba1440b967)
  270 -> (0x10e7c9eebc4449cd, 0xb4ee894dd009453)
  271 -> (0x1b0c764ac6d3a948, 0x1217da87c800ed51)
  272 -> (0x15a391d56bdc876c, 0xdb46486ca000bdda)
  273 -> (0x114fa7ddefe39f8a, 0x490506bd4ccd64af)
  274 -> (0x1bb2a62fe638ff43, 0xa8080ac87ae23ab1)
  275 -> (0x162884f31e93ff69, 0x5339a239fbe82ef4)
  276 -> (0x11ba03f5b20fff87, 0x75c7b4fb2fecf25d)
  277 -> (0x1c5cd322b67fff3f, 0x22d92191e647ea2e)
  278 -> (0x16b0a8e891ffff65, 0xb57a8141850654f2)
  279 -> (0x1226ed86db3332b7, 0xc4620101373843f5)
  280 -> (0x1d0b15a491eb8459, 0x3a366801f1f39fee)
  281 -> (0x173c115074bc69e0, 0xfb5eb99b27f6198b)
  282 -> (0x129674405d6387e7, 0x2f7efae2865e7ad6)
  283 -> (0x1dbd86cd6238d971, 0xe597f7d0d6fd9156)
  284 -> (0x17cad23de82d7ac1, 0x8479930d78cadaab)
  285 -> (0x1308a831868ac89a, 0xd06142712d6f1556)
  286 -> (0x1e74404f3daada91, 0x4d686a4eaf182222)
  287 -> (0x185d003f6488aeda, 0xa453883ef279b4e8)
  288 -> (0x137d99cc506d58ae, 0xe9dc6cff28615d87)
  289 -> (0x1f2f5c7a1a488de4, 0xa960ae650d6895a4)
  290 -> (0x18f2b061aea07183, 0xbab3beb73ded4483)
  _   -> (0x13f559e7bee6c136, 0x2ef6322c318a9d36)
#endif

-- | Index into the 128-bit word lookup table double_pow5_split
get_double_pow5_split :: Int -> (Word64, Word64)
#if !PURE_HASKELL
get_double_pow5_split = getWord128At double_pow5_split
#else
-- > putStr $ case128 (fnorm double_pow5_bitcount) [0..double_max_split]
get_double_pow5_split i = case i of
  0 -> (0x1000000000000000, 0x0)
  1 -> (0x1400000000000000, 0x0)
  2 -> (0x1900000000000000, 0x0)
  3 -> (0x1f40000000000000, 0x0)
  4 -> (0x1388000000000000, 0x0)
  5 -> (0x186a000000000000, 0x0)
  6 -> (0x1e84800000000000, 0x0)
  7 -> (0x1312d00000000000, 0x0)
  8 -> (0x17d7840000000000, 0x0)
  9 -> (0x1dcd650000000000, 0x0)
  10 -> (0x12a05f2000000000, 0x0)
  11 -> (0x174876e800000000, 0x0)
  12 -> (0x1d1a94a200000000, 0x0)
  13 -> (0x12309ce540000000, 0x0)
  14 -> (0x16bcc41e90000000, 0x0)
  15 -> (0x1c6bf52634000000, 0x0)
  16 -> (0x11c37937e0800000, 0x0)
  17 -> (0x16345785d8a00000, 0x0)
  18 -> (0x1bc16d674ec80000, 0x0)
  19 -> (0x1158e460913d0000, 0x0)
  20 -> (0x15af1d78b58c4000, 0x0)
  21 -> (0x1b1ae4d6e2ef5000, 0x0)
  22 -> (0x10f0cf064dd59200, 0x0)
  23 -> (0x152d02c7e14af680, 0x0)
  24 -> (0x1a784379d99db420, 0x0)
  25 -> (0x108b2a2c28029094, 0x0)
  26 -> (0x14adf4b7320334b9, 0x0)
  27 -> (0x19d971e4fe8401e7, 0x4000000000000000)
  28 -> (0x1027e72f1f128130, 0x8800000000000000)
  29 -> (0x1431e0fae6d7217c, 0xaa00000000000000)
  30 -> (0x193e5939a08ce9db, 0xd480000000000000)
  31 -> (0x1f8def8808b02452, 0xc9a0000000000000)
  32 -> (0x13b8b5b5056e16b3, 0xbe04000000000000)
  33 -> (0x18a6e32246c99c60, 0xad85000000000000)
  34 -> (0x1ed09bead87c0378, 0xd8e6400000000000)
  35 -> (0x13426172c74d822b, 0x878fe80000000000)
  36 -> (0x1812f9cf7920e2b6, 0x6973e20000000000)
  37 -> (0x1e17b84357691b64, 0x3d0da8000000000)
  38 -> (0x12ced32a16a1b11e, 0x8262889000000000)
  39 -> (0x178287f49c4a1d66, 0x22fb2ab400000000)
  40 -> (0x1d6329f1c35ca4bf, 0xabb9f56100000000)
  41 -> (0x125dfa371a19e6f7, 0xcb54395ca0000000)
  42 -> (0x16f578c4e0a060b5, 0xbe2947b3c8000000)
  43 -> (0x1cb2d6f618c878e3, 0x2db399a0ba000000)
  44 -> (0x11efc659cf7d4b8d, 0xfc90400474400000)
  45 -> (0x166bb7f0435c9e71, 0x7bb4500591500000)
  46 -> (0x1c06a5ec5433c60d, 0xdaa16406f5a40000)
  47 -> (0x118427b3b4a05bc8, 0xa8a4de8459868000)
  48 -> (0x15e531a0a1c872ba, 0xd2ce16256fe82000)
  49 -> (0x1b5e7e08ca3a8f69, 0x87819baecbe22800)
  50 -> (0x111b0ec57e6499a1, 0xf4b1014d3f6d5900)
  51 -> (0x1561d276ddfdc00a, 0x71dd41a08f48af40)
  52 -> (0x1aba4714957d300d, 0xe549208b31adb10)
  53 -> (0x10b46c6cdd6e3e08, 0x28f4db456ff0c8ea)
  54 -> (0x14e1878814c9cd8a, 0x33321216cbecfb24)
  55 -> (0x1a19e96a19fc40ec, 0xbffe969c7ee839ed)
  56 -> (0x105031e2503da893, 0xf7ff1e21cf512434)
  57 -> (0x14643e5ae44d12b8, 0xf5fee5aa43256d41)
  58 -> (0x197d4df19d605767, 0x337e9f14d3eec892)
  59 -> (0x1fdca16e04b86d41, 0x5e46da08ea7ab6)
  60 -> (0x13e9e4e4c2f34448, 0xa03aec4845928cb2)
  61 -> (0x18e45e1df3b0155a, 0xc849a75a56f72fde)
  62 -> (0x1f1d75a5709c1ab1, 0x7a5c1130ecb4fbd6)
  63 -> (0x13726987666190ae, 0xec798abe93f11d65)
  64 -> (0x184f03e93ff9f4da, 0xa797ed6e38ed64bf)
  65 -> (0x1e62c4e38ff87211, 0x517de8c9c728bdef)
  66 -> (0x12fdbb0e39fb474a, 0xd2eeb17e1c7976b5)
  67 -> (0x17bd29d1c87a191d, 0x87aa5ddda397d462)
  68 -> (0x1dac74463a989f64, 0xe994f5550c7dc97b)
  69 -> (0x128bc8abe49f639f, 0x11fd195527ce9ded)
  70 -> (0x172ebad6ddc73c86, 0xd67c5faa71c24568)
  71 -> (0x1cfa698c95390ba8, 0x8c1b77950e32d6c2)
  72 -> (0x121c81f7dd43a749, 0x57912abd28dfc639)
  73 -> (0x16a3a275d494911b, 0xad75756c7317b7c8)
  74 -> (0x1c4c8b1349b9b562, 0x98d2d2c78fdda5ba)
  75 -> (0x11afd6ec0e14115d, 0x9f83c3bcb9ea8794)
  76 -> (0x161bcca7119915b5, 0x764b4abe8652979)
  77 -> (0x1ba2bfd0d5ff5b22, 0x493de1d6e27e73d7)
  78 -> (0x1145b7e285bf98f5, 0x6dc6ad264d8f0866)
  79 -> (0x159725db272f7f32, 0xc938586fe0f2ca80)
  80 -> (0x1afcef51f0fb5eff, 0x7b866e8bd92f7d20)
  81 -> (0x10de1593369d1b5f, 0xad34051767bdae34)
  82 -> (0x15159af804446237, 0x9881065d41ad19c1)
  83 -> (0x1a5b01b605557ac5, 0x7ea147f492186032)
  84 -> (0x1078e111c3556cbb, 0x6f24ccf8db4f3c1f)
  85 -> (0x14971956342ac7ea, 0x4aee003712230b27)
  86 -> (0x19bcdfabc13579e4, 0xdda98044d6abcdf0)
  87 -> (0x10160bcb58c16c2f, 0xa89f02b062b60b6)
  88 -> (0x141b8ebe2ef1c73a, 0xcd2c6c35c7b638e4)
  89 -> (0x1922726dbaae3909, 0x8077874339a3c71d)
  90 -> (0x1f6b0f092959c74b, 0xe0956914080cb8e4)
  91 -> (0x13a2e965b9d81c8f, 0x6c5d61ac8507f38e)
  92 -> (0x188ba3bf284e23b3, 0x4774ba17a649f072)
  93 -> (0x1eae8caef261aca0, 0x1951e89d8fdc6c8f)
  94 -> (0x132d17ed577d0be4, 0xfd3316279e9c3d9)
  95 -> (0x17f85de8ad5c4edd, 0x13c7fdbb186434cf)
  96 -> (0x1df67562d8b36294, 0x58b9fd29de7d4203)
  97 -> (0x12ba095dc7701d9c, 0xb7743e3a2b0e4942)
  98 -> (0x17688bb5394c2503, 0xe5514dc8b5d1db92)
  99 -> (0x1d42aea2879f2e44, 0xdea5a13ae3465277)
  100 -> (0x1249ad2594c37ceb, 0xb2784c4ce0bf38a)
  101 -> (0x16dc186ef9f45c25, 0xcdf165f6018ef06d)
  102 -> (0x1c931e8ab871732f, 0x416dbf7381f2ac88)
  103 -> (0x11dbf316b346e7fd, 0x88e497a83137abd5)
  104 -> (0x1652efdc6018a1fc, 0xeb1dbd923d8596ca)
  105 -> (0x1be7abd3781eca7c, 0x25e52cf6cce6fc7d)
  106 -> (0x1170cb642b133e8d, 0x97af3c1a40105dce)
  107 -> (0x15ccfe3d35d80e30, 0xfd9b0b20d0147542)
  108 -> (0x1b403dcc834e11bd, 0x3d01cde904199292)
  109 -> (0x1108269fd210cb16, 0x462120b1a28ffb9b)
  110 -> (0x154a3047c694fddb, 0xd7a968de0b33fa82)
  111 -> (0x1a9cbc59b83a3d52, 0xcd93c3158e00f923)
  112 -> (0x10a1f5b813246653, 0xc07c59ed78c09bb6)
  113 -> (0x14ca732617ed7fe8, 0xb09b7068d6f0c2a3)
  114 -> (0x19fd0fef9de8dfe2, 0xdcc24c830cacf34c)
  115 -> (0x103e29f5c2b18bed, 0xc9f96fd1e7ec180f)
  116 -> (0x144db473335deee9, 0x3c77cbc661e71e13)
  117 -> (0x1961219000356aa3, 0x8b95beb7fa60e598)
  118 -> (0x1fb969f40042c54c, 0x6e7b2e65f8f91efe)
  119 -> (0x13d3e2388029bb4f, 0xc50cfcffbb9bb35f)
  120 -> (0x18c8dac6a0342a23, 0xb6503c3faa82a037)
  121 -> (0x1efb1178484134ac, 0xa3e44b4f95234844)
  122 -> (0x135ceaeb2d28c0eb, 0xe66eaf11bd360d2b)
  123 -> (0x183425a5f872f126, 0xe00a5ad62c839075)
  124 -> (0x1e412f0f768fad70, 0x980cf18bb7a47493)
  125 -> (0x12e8bd69aa19cc66, 0x5f0816f752c6c8dc)
  126 -> (0x17a2ecc414a03f7f, 0xf6ca1cb527787b13)
  127 -> (0x1d8ba7f519c84f5f, 0xf47ca3e2715699d7)
  128 -> (0x127748f9301d319b, 0xf8cde66d86d62026)
  129 -> (0x17151b377c247e02, 0xf7016008e88ba830)
  130 -> (0x1cda62055b2d9d83, 0xb4c1b80b22ae923c)
  131 -> (0x12087d4358fc8272, 0x50f91306f5ad1b65)
  132 -> (0x168a9c942f3ba30e, 0xe53757c8b318623f)
  133 -> (0x1c2d43b93b0a8bd2, 0x9e852dbadfde7acf)
  134 -> (0x119c4a53c4e69763, 0xa3133c94cbeb0cc1)
  135 -> (0x16035ce8b6203d3c, 0x8bd80bb9fee5cff1)
  136 -> (0x1b843422e3a84c8b, 0xaece0ea87e9f43ee)
  137 -> (0x1132a095ce492fd7, 0x4d40c9294f238a75)
  138 -> (0x157f48bb41db7bcd, 0x2090fb73a2ec6d12)
  139 -> (0x1adf1aea12525ac0, 0x68b53a508ba78856)
  140 -> (0x10cb70d24b7378b8, 0x417144725748b536)
  141 -> (0x14fe4d06de5056e6, 0x51cd958eed1ae283)
  142 -> (0x1a3de04895e46c9f, 0xe640faf2a8619b24)
  143 -> (0x1066ac2d5daec3e3, 0xefe89cd7a93d00f7)
  144 -> (0x14805738b51a74dc, 0xebe2c40d938c4134)
  145 -> (0x19a06d06e2611214, 0x26db7510f86f5181)
  146 -> (0x100444244d7cab4c, 0x9849292a9b4592f1)
  147 -> (0x1405552d60dbd61f, 0xbe5b73754216f7ad)
  148 -> (0x1906aa78b912cba7, 0xadf25052929cb598)
  149 -> (0x1f485516e7577e91, 0x996ee4673743e2ff)
  150 -> (0x138d352e5096af1a, 0xffe54ec0828a6ddf)
  151 -> (0x18708279e4bc5ae1, 0xbfdea270a32d0957)
  152 -> (0x1e8ca3185deb719a, 0x2fd64b0ccbf84bad)
  153 -> (0x1317e5ef3ab32700, 0x5de5eee7ff7b2f4c)
  154 -> (0x17dddf6b095ff0c0, 0x755f6aa1ff59fb1f)
  155 -> (0x1dd55745cbb7ecf0, 0x92b7454a7f3079e7)
  156 -> (0x12a5568b9f52f416, 0x5bb28b4e8f7e4c30)
  157 -> (0x174eac2e8727b11b, 0xf29f2e22335ddf3c)
  158 -> (0x1d22573a28f19d62, 0xef46f9aac035570b)
  159 -> (0x123576845997025d, 0xd58c5c0ab8215667)
  160 -> (0x16c2d4256ffcc2f5, 0x4aef730d6629ac01)
  161 -> (0x1c73892ecbfbf3b2, 0x9dab4fd0bfb41701)
  162 -> (0x11c835bd3f7d784f, 0xa28b11e277d08e60)
  163 -> (0x163a432c8f5cd663, 0x8b2dd65b15c4b1f9)
  164 -> (0x1bc8d3f7b3340bfc, 0x6df94bf1db35de77)
  165 -> (0x115d847ad000877d, 0xc4bbcf772901ab0a)
  166 -> (0x15b4e5998400a95d, 0x35eac354f34215cd)
  167 -> (0x1b221effe500d3b4, 0x8365742a30129b40)
  168 -> (0x10f5535fef208450, 0xd21f689a5e0ba108)
  169 -> (0x1532a837eae8a565, 0x6a742c0f58e894a)
  170 -> (0x1a7f5245e5a2cebe, 0x4851137132f22b9d)
  171 -> (0x108f936baf85c136, 0xed32ac26bfd75b42)
  172 -> (0x14b378469b673184, 0xa87f57306fcd3212)
  173 -> (0x19e056584240fde5, 0xd29f2cfc8bc07e97)
  174 -> (0x102c35f729689eaf, 0xa3a37c1dd7584f1e)
  175 -> (0x14374374f3c2c65b, 0x8c8c5b254d2e62e6)
  176 -> (0x1945145230b377f2, 0x6faf71eea079fb9f)
  177 -> (0x1f965966bce055ef, 0xb9b4e6a48987a87)
  178 -> (0x13bdf7e0360c35b5, 0x674111026d5f4c94)
  179 -> (0x18ad75d8438f4322, 0xc111554308b71fba)
  180 -> (0x1ed8d34e547313eb, 0x7155aa93cae4e7a8)
  181 -> (0x13478410f4c7ec73, 0x26d58a9c5ecf10c9)
  182 -> (0x1819651531f9e78f, 0xf08aed437682d4fb)
  183 -> (0x1e1fbe5a7e786173, 0xecada89454238a3a)
  184 -> (0x12d3d6f88f0b3ce8, 0x73ec895cb4963664)
  185 -> (0x1788ccb6b2ce0c22, 0x90e7abb3e1bbc3fd)
  186 -> (0x1d6affe45f818f2b, 0x352196a0da2ab4fd)
  187 -> (0x1262dfeebbb0f97b, 0x134fe24885ab11e)
  188 -> (0x16fb97ea6a9d37d9, 0xc1823dadaa715d65)
  189 -> (0x1cba7de5054485d0, 0x31e2cd19150db4bf)
  190 -> (0x11f48eaf234ad3a2, 0x1f2dc02fad2890f7)
  191 -> (0x1671b25aec1d888a, 0xa6f9303b9872b535)
  192 -> (0x1c0e1ef1a724eaad, 0x50b77c4a7e8f6282)
  193 -> (0x1188d357087712ac, 0x5272adae8f199d91)
  194 -> (0x15eb082cca94d757, 0x670f591a32e004f6)
  195 -> (0x1b65ca37fd3a0d2d, 0x40d32f60bf980633)
  196 -> (0x111f9e62fe44483c, 0x4883fd9c77bf03e0)
  197 -> (0x156785fbbdd55a4b, 0x5aa4fd0395aec4d8)
  198 -> (0x1ac1677aad4ab0de, 0x314e3c447b1a760e)
  199 -> (0x10b8e0acac4eae8a, 0xded0e5aaccf089c9)
  200 -> (0x14e718d7d7625a2d, 0x96851f15802cac3b)
  201 -> (0x1a20df0dcd3af0b8, 0xfc2666dae037d74a)
  202 -> (0x10548b68a044d673, 0x9d980048cc22e68e)
  203 -> (0x1469ae42c8560c10, 0x84fe005aff2ba032)
  204 -> (0x198419d37a6b8f14, 0xa63d8071bef6883e)
  205 -> (0x1fe52048590672d9, 0xcfcce08e2eb42a4e)
  206 -> (0x13ef342d37a407c8, 0x21e00c58dd309a70)
  207 -> (0x18eb0138858d09ba, 0x2a580f6f147cc10d)
  208 -> (0x1f25c186a6f04c28, 0xb4ee134ad99bf150)
  209 -> (0x137798f428562f99, 0x7114cc0ec80176d2)
  210 -> (0x18557f31326bbb7f, 0xcd59ff127a01d486)
  211 -> (0x1e6adefd7f06aa5f, 0xc0b07ed7188249a8)
  212 -> (0x1302cb5e6f642a7b, 0xd86e4f466f516e09)
  213 -> (0x17c37e360b3d351a, 0xce89e3180b25c98b)
  214 -> (0x1db45dc38e0c8261, 0x822c5bde0def3bee)
  215 -> (0x1290ba9a38c7d17c, 0xf15bb96ac8b58575)
  216 -> (0x1734e940c6f9c5dc, 0x2db2a7c57ae2e6d2)
  217 -> (0x1d022390f8b83753, 0x391f51b6d99ba086)
  218 -> (0x1221563a9b732294, 0x3b3931248014454)
  219 -> (0x16a9abc9424feb39, 0x4a077d6da019569)
  220 -> (0x1c5416bb92e3e607, 0x45c895cc9081fac3)
  221 -> (0x11b48e353bce6fc4, 0x8b9d5d9fda513cba)
  222 -> (0x1621b1c28ac20bb5, 0xae84b507d0e58be8)
  223 -> (0x1baa1e332d728ea3, 0x1a25e249c51eeee3)
  224 -> (0x114a52dffc679925, 0xf057ad6e1b33554d)
  225 -> (0x159ce797fb817f6f, 0x6c6d98c9a2002aa1)
  226 -> (0x1b04217dfa61df4b, 0x4788fefc0a803549)
  227 -> (0x10e294eebc7d2b8f, 0xcb59f5d8690214e)
  228 -> (0x151b3a2a6b9c7672, 0xcfe30734e83429a1)
  229 -> (0x1a6208b50683940f, 0x83dbc9022241340a)
  230 -> (0x107d457124123c89, 0xb2695da15568c086)
  231 -> (0x149c96cd6d16cbac, 0x1f03b509aac2f0a7)
  232 -> (0x19c3bc80c85c7e97, 0x26c4a24c1573acd1)
  233 -> (0x101a55d07d39cf1e, 0x783ae56f8d684c03)
  234 -> (0x1420eb449c8842e6, 0x16499ecb70c25f03)
  235 -> (0x19292615c3aa539f, 0x9bdc067e4cf2f6c4)
  236 -> (0x1f736f9b3494e887, 0x82d3081de02fb476)
  237 -> (0x13a825c100dd1154, 0xb1c3e512ac1dd0c9)
  238 -> (0x18922f31411455a9, 0xde34de57572544fc)
  239 -> (0x1eb6bafd91596b14, 0x55c215ed2cee963b)
  240 -> (0x133234de7ad7e2ec, 0xb5994db43c151de5)
  241 -> (0x17fec216198ddba7, 0xe2ffa1214b1a655e)
  242 -> (0x1dfe729b9ff15291, 0xdbbf89699de0feb6)
  243 -> (0x12bf07a143f6d39b, 0x2957b5e202ac9f31)
  244 -> (0x176ec98994f48881, 0xf3ada35a8357c6fe)
  245 -> (0x1d4a7bebfa31aaa2, 0x70990c31242db8bd)
  246 -> (0x124e8d737c5f0aa5, 0x865fa79eb69c9376)
  247 -> (0x16e230d05b76cd4e, 0xe7f791866443b854)
  248 -> (0x1c9abd04725480a2, 0xa1f575e7fd54a669)
  249 -> (0x11e0b622c774d065, 0xa53969b0fe54e801)
  250 -> (0x1658e3ab7952047f, 0xe87c41d3dea2202)
  251 -> (0x1bef1c9657a6859e, 0xd229b5248d64aa82)
  252 -> (0x117571ddf6c81383, 0x435a1136d85eea91)
  253 -> (0x15d2ce55747a1864, 0x143095848e76a536)
  254 -> (0x1b4781ead1989e7d, 0x193cbae5b2144e83)
  255 -> (0x110cb132c2ff630e, 0x2fc5f4cf8f4cb112)
  256 -> (0x154fdd7f73bf3bd1, 0xbbb77203731fdd56)
  257 -> (0x1aa3d4df50af0ac6, 0x2aa54e844fe7d4ac)
  258 -> (0x10a6650b926d66bb, 0xdaa75112b1f0e4eb)
  259 -> (0x14cffe4e7708c06a, 0xd15125575e6d1e26)
  260 -> (0x1a03fde214caf085, 0x85a56ead360865b0)
  261 -> (0x10427ead4cfed653, 0x7387652c41c53f8e)
  262 -> (0x14531e58a03e8be8, 0x50693e7752368f71)
  263 -> (0x1967e5eec84e2ee2, 0x64838e1526c4334e)
  264 -> (0x1fc1df6a7a61ba9a, 0xfda4719a70754022)
  265 -> (0x13d92ba28c7d14a0, 0xde86c70086494815)
  266 -> (0x18cf768b2f9c59c9, 0x162878c0a7db9a1a)
  267 -> (0x1f03542dfb83703b, 0x5bb296f0d1d280a1)
  268 -> (0x1362149cbd322625, 0x194f9e5683239064)
  269 -> (0x183a99c3ec7eafae, 0x5fa385ec23ec747e)
  270 -> (0x1e494034e79e5b99, 0xf78c67672ce7919d)
  271 -> (0x12edc82110c2f940, 0x3ab7c0a07c10bb02)
  272 -> (0x17a93a2954f3b790, 0x4965b0c89b14e9c3)
  273 -> (0x1d9388b3aa30a574, 0x5bbf1cfac1da2433)
  274 -> (0x127c35704a5e6768, 0xb957721cb92856a0)
  275 -> (0x171b42cc5cf60142, 0xe7ad4ea3e7726c48)
  276 -> (0x1ce2137f74338193, 0xa198a24ce14f075a)
  277 -> (0x120d4c2fa8a030fc, 0x44ff65700cd16498)
  278 -> (0x16909f3b92c83d3b, 0x563f3ecc1005bdbe)
  279 -> (0x1c34c70a777a4c8a, 0x2bcf0e7f14072d2e)
  280 -> (0x11a0fc668aac6fd6, 0x5b61690f6c847c3d)
  281 -> (0x16093b802d578bcb, 0xf239c35347a59b4c)
  282 -> (0x1b8b8a6038ad6ebe, 0xeec83428198f021f)
  283 -> (0x1137367c236c6537, 0x553d20990ff96153)
  284 -> (0x1585041b2c477e85, 0x2a8c68bf53f7b9a8)
  285 -> (0x1ae64521f7595e26, 0x752f82ef28f5a812)
  286 -> (0x10cfeb353a97dad8, 0x93db1d57999890b)
  287 -> (0x1503e602893dd18e, 0xb8d1e4ad7ffeb4e)
  288 -> (0x1a44df832b8d45f1, 0x8e7065dd8dffe622)
  289 -> (0x106b0bb1fb384bb6, 0xf9063faa78bfefd5)
  290 -> (0x1485ce9e7a065ea4, 0xb747cf9516efebca)
  291 -> (0x19a742461887f64d, 0xe519c37a5cabe6bd)
  292 -> (0x1008896bcf54f9f0, 0xaf301a2c79eb7036)
  293 -> (0x140aabc6c32a386c, 0xdafc20b798664c43)
  294 -> (0x190d56b873f4c688, 0x11bb28e57e7fdf54)
  295 -> (0x1f50ac6690f1f82a, 0x1629f31ede1fd72a)
  296 -> (0x13926bc01a973b1a, 0x4dda37f34ad3e67a)
  297 -> (0x187706b0213d09e0, 0xe150c5f01d88e019)
  298 -> (0x1e94c85c298c4c59, 0x19a4f76c24eb181f)
  299 -> (0x131cfd3999f7afb7, 0xb0071aa39712ef13)
  300 -> (0x17e43c8800759ba5, 0x9c08e14c7cd7aad8)
  301 -> (0x1ddd4baa0093028f, 0x30b199f9c0d958e)
  302 -> (0x12aa4f4a405be199, 0x61e6f003c1887d79)
  303 -> (0x1754e31cd072d9ff, 0xba60ac04b1ea9cd7)
  304 -> (0x1d2a1be4048f907f, 0xa8f8d705de65440d)
  305 -> (0x123a516e82d9ba4f, 0xc99b8663aaff4a88)
  306 -> (0x16c8e5ca239028e3, 0xbc0267fc95bf1d2a)
  307 -> (0x1c7b1f3cac74331c, 0xab0301fbbb2ee474)
  308 -> (0x11ccf385ebc89ff1, 0xeae1e13d54fd4ec9)
  309 -> (0x1640306766bac7ee, 0x659a598caa3ca27b)
  310 -> (0x1bd03c81406979e9, 0xff00efefd4cbcb1a)
  311 -> (0x116225d0c841ec32, 0x3f6095f5e4ff5ef0)
  312 -> (0x15baaf44fa52673e, 0xcf38bb735e3f36ac)
  313 -> (0x1b295b1638e7010e, 0x8306ea5035cf0457)
  314 -> (0x10f9d8ede39060a9, 0x11e4527221a162b6)
  315 -> (0x15384f295c7478d3, 0x565d670eaa09bb64)
  316 -> (0x1a8662f3b3919708, 0x2bf4c0d2548c2a3d)
  317 -> (0x1093fdd8503afe65, 0x1b78f88374d79a66)
  318 -> (0x14b8fd4e6449bdfe, 0x625736a4520d8100)
  319 -> (0x19e73ca1fd5c2d7d, 0xfaed044d6690e140)
  320 -> (0x103085e53e599c6e, 0xbcd422b0601a8cc8)
  321 -> (0x143ca75e8df0038a, 0x6c092b5c78212ffa)
  322 -> (0x194bd136316c046d, 0x70b763396297bf8)
  323 -> (0x1f9ec583bdc70588, 0x48ce53c07bb3daf6)
  324 -> (0x13c33b72569c6375, 0x2d80f4584d5068da)
  _   -> (0x18b40a4eec437c52, 0x78e1316e60a48310)
#endif

-- | Take the high bits of m * 5^-e2-q / 2^k / 2^q-k
mulPow5DivPow2 :: Word64 -> Int -> Int -> Word64
mulPow5DivPow2 m i j = mulShift64 m (get_double_pow5_split i) j

-- | Take the high bits of m * 2^k / 5^q / 2^-e2+q+k
mulPow5InvDivPow2 :: Word64 -> Int -> Int -> Word64
mulPow5InvDivPow2 m q j = mulShift64 m (get_double_pow5_inv_split q) j

-- | Handle case e2 >= 0
d2dGT :: Int32 -> Word64 -> Word64 -> Word64 -> (BoundsState Word64, Int32)
d2dGT e2' u v w =
  let e2 = int32ToInt e2'
      q = log10pow2 e2 - fromEnum (e2 > 3)
      -- k = B0 + log_2(5^q)
      k = double_pow5_inv_bitcount + pow5bits q - 1
      i = -e2 + q + k
      -- (u, v, w) * 2^k / 5^q / 2^-e2+q+k
      u' = mulPow5InvDivPow2 u q i
      v' = mulPow5InvDivPow2 v q i
      w' = mulPow5InvDivPow2 w q i
      !(vvTrailing, vuTrailing, vw') =
        case () of
          _ | q <= 21 && (drem5 v == 0)
                -> (multipleOfPowerOf5 v q, False, w')
            | q <= 21 && acceptBounds v
                -> (False, multipleOfPowerOf5 u q, w')
            | q <= 21
                -> (False, False, w' - boolToWord64 (multipleOfPowerOf5 w q))
            | otherwise
                -> (False, False, w')
   in (BoundsState u' v' vw' 0 vuTrailing vvTrailing, intToInt32 q)

-- | Handle case e2 < 0
d2dLT :: Int32 -> Word64 -> Word64 -> Word64 -> (BoundsState Word64, Int32)
d2dLT e2' u v w =
  let e2 = int32ToInt e2'
      q = log10pow5 (-e2) - fromEnum (-e2 > 1)
      e10 = q + e2
      i = -e2 - q
      -- k = log_2(5^-e2-q) - B1
      k = pow5bits i - double_pow5_bitcount
      j = q - k
      -- (u, v, w) * 5^-e2-q / 2^k / 2^q-k
      u' = mulPow5DivPow2 u i j
      v' = mulPow5DivPow2 v i j
      w' = mulPow5DivPow2 w i j
      !(vvTrailing, vuTrailing, vw') =
        case () of
          _ | q <= 1 && acceptBounds v
                -> (True, v - u == 2, w') -- mmShift == 1
            | q <= 1
                -> (True, False, w' - 1)
            | q < 63
                -> (multipleOfPowerOf2 v (q - 1), False, w')
            | otherwise
                -> (False, False, w')
   in (BoundsState u' v' vw' 0 vuTrailing vvTrailing, intToInt32 e10)

-- | Returns the decimal representation of the given mantissa and exponent of a
-- 64-bit Double using the ryu algorithm.
d2d :: Word64 -> Word64 -> FloatingDecimal
d2d m e =
  let !mf = if e == 0
              then m
              else (1 `unsafeShiftL` double_mantissa_bits) .|. m
      !ef = intToInt32 $ if e == 0
              then 1 - (double_bias + double_mantissa_bits)
              else word64ToInt e - (double_bias + double_mantissa_bits)
      !e2 = ef - 2
      -- Step 2. 3-tuple (u, v, w) * 2**e2
      !u = 4 * mf - 1 - boolToWord64 (m /= 0 || e <= 1)
      !v = 4 * mf
      !w = 4 * mf + 2
      -- Step 3. convert to decimal power base
      !(state, e10) =
        if e2 >= 0
           then d2dGT e2 u v w
           else d2dLT e2 u v w
      -- Step 4: Find the shortest decimal representation in the interval of
      -- valid representations.
      !(output, removed) =
        let rounded = closestCorrectlyRounded (acceptBounds v)
         in first rounded $ if vvIsTrailingZeros state || vuIsTrailingZeros state
           then trimTrailing state
           else trimNoTrailing state
      !e' = e10 + removed
   in FloatingDecimal output e'

-- | Split a Double into (sign, mantissa, exponent)
breakdown :: Double -> (Bool, Word64, Word64)
breakdown f =
  let bits = castDoubleToWord64 f
      sign = ((bits `unsafeShiftR` (double_mantissa_bits + double_exponent_bits)) .&. 1) /= 0
      mantissa = bits .&. mask double_mantissa_bits
      expo = (bits `unsafeShiftR` double_mantissa_bits) .&. mask double_exponent_bits
   in (sign, mantissa, expo)

-- | Dispatches to `d2d` or `d2dSmallInt` and applies the given formatters
{-# INLINE d2s' #-}
d2s' :: (Bool -> Word64 -> Int32 -> a) -> (NonNumbersAndZero -> a) -> Double -> a
d2s' formatter specialFormatter d =
  let (sign, mantissa, expo) = breakdown d
   in if (expo == mask double_exponent_bits) || (expo == 0 && mantissa == 0)
         then specialFormatter NonNumbersAndZero
                  { negative=sign
                  , exponent_all_one=expo > 0
                  , mantissa_non_zero=mantissa > 0 }
         else let v = unifySmallTrailing <$> d2dSmallInt mantissa expo
                  FloatingDecimal m e = fromMaybe (d2d mantissa expo) v
               in formatter sign m e

-- | Render a Double in scientific notation
d2s :: Double -> Builder
d2s d = primBounded (d2s' toCharsScientific toCharsNonNumbersAndZero d) ()

-- | Returns the decimal representation of a Double. NaN and Infinity will
-- return `FloatingDecimal 0 0`
d2Intermediate :: Double -> FloatingDecimal
d2Intermediate = d2s' (const FloatingDecimal) (const $ FloatingDecimal 0 0)
