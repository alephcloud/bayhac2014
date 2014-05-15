-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module BayHac2014.Cryptmail.PasswordEncryption
( Code64(..)
, Password(..)
, PlainText(..)
, CipherText(..)
, encryptWithPwd
, decryptWithPwd

#ifndef __HASTE__
-- * Test Properties
, prop_code640
, prop_code641
, prop_code642
, prop_randomBytes0
, prop_randomBytes1
, prop_padPkcs70
, prop_pbkdf20
, prop_pbkdf21
, prop_aesCbc0
, prop_encrypt0
-- ** Command line interface
, runTests
-- ** Cabal interface
, code64Tests
, cryptoTests
, passwordEncryptionTests
#endif
) where

import qualified BayHac2014.Cryptmail.Text as T
import qualified BayHac2014.Cryptmail.ByteString as B

import Control.Applicative

import Data.Monoid.Unicode

import Prelude.Unicode

-- -------------------------------------------------------------------------- --
-- Backend specific imports

#ifdef __HASTE__

import Haste
import Haste.Prim

import System.IO.Unsafe (unsafePerformIO)

#else

import Crypto.Cipher.AES
import Crypto.PBKDF.ByteString (sha512PBKDF2)
import Crypto.Random

import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T

import Distribution.TestSuite.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances ()

#endif

fmapL ∷ (α → γ) → Either α β → Either γ β
fmapL f (Left a) = Left (f a)
fmapL _ (Right a) = Right a

-- -------------------------------------------------------------------------- --
-- Base64 Serialization

class Code64 α where
    to64 ∷ α → T.Text
    from64 ∷ T.Text → Either String α

#ifdef __HASTE__

foreign import ccall "base64toBits" j_toBytes64 ∷ JSString → IO (Ptr (Either JSString B.ByteString))
foreign import ccall "base64fromBits" j_fromBytes64 ∷ B.ByteString → IO JSString
foreign import ccall "utf8toBits" j_toBytesUtf8 ∷ JSString → IO B.ByteString
foreign import ccall "utf8fromBits" j_fromBytesUtf8 ∷ B.ByteString → IO (Ptr (Either JSString JSString))

instance Code64 B.ByteString where
    to64 = unsafePerformIO ∘ j_fromBytes64
    from64 = fmapL fromJSStr ∘ fromPtr ∘ unsafePerformIO ∘ j_toBytes64

    {-# INLINE to64 #-}
    {-# INLINE from64 #-}

#else

instance Code64 B.ByteString where
    to64 = T.decodeUtf8 ∘ B64.encode
    from64 = B64.decode ∘ T.encodeUtf8

    {-# INLINE to64 #-}
    {-# INLINE from64 #-}

#endif

-- -------------------------------------------------------------------------- --
-- Cryptographically strong Randomness

#ifdef __HASTE__

paranoia ∷ Int
paranoia = 10

type Paranoia = Int

foreign import ccall "randomWords" j_randomWords ∷ Int → Paranoia → IO B.ByteString

randomBytes ∷ Int → IO B.ByteString
randomBytes i = B.take i <$> j_randomWords ((i + 4) `div` 4) paranoia
{-# INLINE randomBytes #-}

#else

randomBytes ∷ Int → IO B.ByteString
randomBytes i = do
    rng ∷ SystemRNG ← cprgCreate <$> createEntropyPool
    return $ fst ∘ cprgGenerate i $ rng

#endif

-- -------------------------------------------------------------------------- --
-- Crypto

pbkdf2Rounds ∷ Int
pbkdf2Rounds = 10000

pbkdf2SaltLength ∷ Int
pbkdf2SaltLength = 32

aesKeyLength ∷ Int
aesKeyLength = 32

aesBlockLength ∷ Int
aesBlockLength = 16

padPKCS7 ∷ Int → B.ByteString → B.ByteString
padPKCS7 blockLength a = case blockLength - (B.length a `rem` blockLength) of
    0 → a ⊕ B.replicate blockLength (fromIntegral blockLength)
    i → a ⊕ B.replicate i (fromIntegral i)

-- | precondition: @a ≠ ""@
--
unpadPKCS7 ∷ B.ByteString → B.ByteString
unpadPKCS7 a = B.take (B.length a - fromIntegral (B.last a)) a

#ifdef __HASTE__

foreign import ccall "pbkdf2" j_pbkdf2 ∷ B.ByteString → B.ByteString → Int → Int → IO B.ByteString

pbkdf2 ∷ B.ByteString → B.ByteString → B.ByteString
pbkdf2 pwd salt = unsafePerformIO $
    j_pbkdf2 pwd salt pbkdf2Rounds (aesKeyLength * 8)

-- | SJCL AES-CBC encryption
--
-- %1: Key, %2: data, %3: IV
--
foreign import ccall "aesEncrypt" j_aesCbcEncrypt ∷ B.ByteString → B.ByteString → B.ByteString → IO B.ByteString
foreign import ccall "aesDecrypt" j_aesCbcDecrypt ∷ B.ByteString → B.ByteString → B.ByteString → IO (Ptr (Either JSString B.ByteString))

-- | AES-256 encryption with CBC mode and PKCS#5 padding
--
aes256CbcEncrypt ∷ B.ByteString → B.ByteString → B.ByteString → B.ByteString
aes256CbcEncrypt k iv d = unsafePerformIO $ j_aesCbcEncrypt k d iv

-- | AES-256 decryption with CBC mode and PKCS#5 padding
--
-- Returns 'Left' if the padding is invalid
--
aes256CbcDecrypt ∷ B.ByteString → B.ByteString → B.ByteString → Either String B.ByteString
aes256CbcDecrypt k iv d = fmapL fromJSStr ∘ fromPtr ∘ unsafePerformIO $ j_aesCbcDecrypt k d iv

#else

pbkdf2 ∷ B.ByteString → B.ByteString → B.ByteString
pbkdf2 pwd salt = sha512PBKDF2 pwd salt pbkdf2Rounds aesKeyLength

-- | AES-256 encryption with CBC mode and PKCS#5 padding
--
aes256CbcEncrypt ∷ B.ByteString → B.ByteString → B.ByteString → B.ByteString
aes256CbcEncrypt k iv d =
    encryptCBC (initAES k) iv $ padPKCS7 aesBlockLength d

-- | AES-256 decryption with CBC mode and PKCS#5 padding
--
-- This implementation does not check validity of the padding.
--
aes256CbcDecrypt ∷ B.ByteString → B.ByteString → B.ByteString → Either String B.ByteString
aes256CbcDecrypt k iv d =
    Right ∘ unpadPKCS7 $ decryptCBC (initAES k) iv d

#endif

-- -------------------------------------------------------------------------- --
-- Password Encryption Library

newtype Password = Password { unPassword ∷ B.ByteString }
    deriving (Eq, Code64)

newtype CipherText = CipherText { unCipherText ∷ B.ByteString }
    deriving (Eq, Code64)

newtype PlainText = PlainText { unPlainText ∷ B.ByteString }
    deriving (Eq, Code64)

encryptWithPwd ∷ Password → PlainText → IO CipherText
encryptWithPwd (Password pwdBytes) (PlainText plainTextBytes) = do
    salt ← randomBytes pbkdf2SaltLength
    iv ← randomBytes aesBlockLength
    return $
        let key = pbkdf2 pwdBytes salt
            enc = aes256CbcEncrypt key iv plainTextBytes
        in CipherText $ salt ⊕ iv ⊕ enc

decryptWithPwd ∷ Password → CipherText → Either String PlainText
decryptWithPwd (Password pwdBytes) (CipherText cipherTextBytes) =
    PlainText <$> dec
  where
    (salt, aesBytes) = B.splitAt pbkdf2SaltLength cipherTextBytes
    (iv, enc) = B.splitAt aesBlockLength aesBytes
    key = pbkdf2 pwdBytes salt
    dec = aes256CbcDecrypt key iv enc

-- -------------------------------------------------------------------------- --
-- QuickCheck Properties

#ifndef __HASTE__

deriving instance Arbitrary Password
deriving instance Arbitrary PlainText
deriving instance Arbitrary CipherText

deriving instance Show Password
deriving instance Show PlainText
deriving instance Show CipherText

prop_code640 ∷ B.ByteString → Property
prop_code640 = property ∘ isRight ∘ (from64 ∷ T.Text → Either String B.ByteString) ∘ to64

prop_code641 ∷ B.ByteString → Property
prop_code641 b = property $ Right b ≡ (from64 ∘ to64) b

prop_code642 ∷ B.ByteString → B.ByteString → Property
prop_code642 b1 b2 = b1 ≠ b2 ==> property (isRight a1 && isRight a2 && a1 ≠ a2)
  where
    a1 ∷ Either String B.ByteString
    a1 = from64 $ to64 b1
    a2 ∷ Either String B.ByteString
    a2 = from64 $ to64 b2

prop_randomBytes0 ∷ NonNegative Int → Property
prop_randomBytes0 (NonNegative i) = i < 1024 * 4096 ==> monadicIO $ do
    b ← run $ randomBytes i
    assert $ B.length b ≡ i

prop_randomBytes1 ∷ Property
prop_randomBytes1 = monadicIO $ do
    i ← pick $ choose (0, 1024 * 4096)
    b1 ← run $ randomBytes i
    b2 ← run $ randomBytes i
    assert $ b1 ≠ b2

prop_padPkcs70 ∷ B.ByteString → Property
prop_padPkcs70 b = ∀ (choose (1, 255)) $ \i →
    property $ (unpadPKCS7 ∘ padPKCS7 i) b ≡ b

-- This failed for "a\0", "a", "" (or any case where only one of the first two parameters ends with '\0')
-- This looks like a bug in pbkdf2 to me.
--
prop_pbkdf20 ∷ B.ByteString → B.ByteString → B.ByteString → Property
prop_pbkdf20 pwd0 pwd1 salt = pwd0 ≠ pwd1 && pwd0 `B.snoc` 0 ≠ pwd1 && pwd0 ≠ pwd1 `B.snoc` 0 ==>
    property $ pbkdf2 pwd0 salt ≠ pbkdf2 pwd1 salt

prop_pbkdf21 ∷ B.ByteString → B.ByteString → B.ByteString → Property
prop_pbkdf21 pwd salt0 salt1 = salt0 ≠ salt1 ==> property $ pbkdf2 pwd salt0 ≠ pbkdf2 pwd salt1

prop_aesCbc0 ∷ B.ByteString → Property
prop_aesCbc0 d = monadicIO $ do
    k ← run $ randomBytes aesKeyLength
    iv ← run $ randomBytes aesBlockLength
    assert $ (aes256CbcDecrypt k iv ∘ aes256CbcEncrypt k iv) d ≡ Right d

prop_encrypt0 ∷ Password → PlainText → Property
prop_encrypt0 pwd d = monadicIO $ do
    enc ← run $ encryptWithPwd pwd d
    assert $ decryptWithPwd pwd enc ≡ Right d

-- interfae for command line

runTests ∷ IO Bool
runTests = $quickCheckAll

-- Interface for Cabal

code64Tests ∷ [Test]
code64Tests =
    [ testProperty "code64 0" prop_code640
    , testProperty "code64 1" prop_code641
    , testProperty "code64 2" prop_code642
    ]

cryptoTests ∷ [Test]
cryptoTests =
    [ testProperty "random bytes 0" prop_randomBytes0
    , testProperty "random bytes 1" prop_randomBytes1
    , testProperty "PKCS#7 padding" prop_padPkcs70
    , testProperty "PBKDF2 0" prop_pbkdf20
    , testProperty "PBKDF2 1" prop_pbkdf21
    , testProperty "AES CBC" prop_aesCbc0
    , testProperty "encryption with password" prop_encrypt0
    ]

passwordEncryptionTests ∷ [Test]
passwordEncryptionTests =
    [ testGroup "code64 tests" code64Tests
    , testGroup "crypto tests" cryptoTests
    ]

#endif
