-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BayHac2014.Cryptmail.ServiceApi
( Password(..)
, CipherText(..)
, PlainText(..)
, Request(..)
, EncryptWithPwd(..)
, EncryptWithPwdR(..)
, DecryptWithPwd(..)
, DecryptWithPwdR(..)
) where

import BayHac2014.Cryptmail.PasswordEncryption

import Control.Applicative

import Prelude.Unicode

#ifdef __HASTE__
import BayHac2014.Cryptmail.Json
#else
import Data.Aeson
#endif

-- -------------------------------------------------------------------------- --
-- Types

instance ToJSON Password where
    toJSON = String ∘ to64
instance FromJSON Password where
    parseJSON = withText "Password" $ either fail return ∘ from64

instance ToJSON CipherText where
    toJSON = String ∘ to64
instance FromJSON CipherText where
    parseJSON = withText "Password" $ either fail return ∘ from64

instance ToJSON PlainText where
    toJSON = String ∘ to64
instance FromJSON PlainText where
    parseJSON = withText "Password" $ either fail return ∘ from64

class (FromJSON α, ToJSON (Response α)) ⇒ Request α where
    type Response α ∷ *
    answerRequest ∷ α → IO (Either String (Response α))

-- -------------------------------------------------------------------------- --
-- Encryption

data EncryptWithPwd = EncryptWithPwd
    { ePassword ∷ !Password
    , ePlainText ∷ !PlainText
    }

instance FromJSON EncryptWithPwd where
    parseJSON = withObject "EncryptWithPwd" $ \o → EncryptWithPwd
        <$> o .: "password"
        <*> o .: "plain_text"

data EncryptWithPwdR = EncryptWithPwdR
    { erCipherText ∷ !CipherText
    }

instance ToJSON EncryptWithPwdR where
    toJSON EncryptWithPwdR{..} = object
        [ "cipher_text" .= erCipherText
        ]

instance Request EncryptWithPwd where
    type Response EncryptWithPwd = EncryptWithPwdR
    answerRequest EncryptWithPwd{..} =
        Right ∘ EncryptWithPwdR <$> encryptWithPwd ePassword ePlainText

-- -------------------------------------------------------------------------- --
-- Decryption

data DecryptWithPwd = DecryptWithPwd
    { dPassword ∷ !Password
    , dCipherText ∷ !CipherText
    }

instance FromJSON DecryptWithPwd where
    parseJSON = withObject "DecryptWithPwd" $ \o → DecryptWithPwd
        <$> o .: "password"
        <*> o .: "cipher_text"

data DecryptWithPwdR = DecryptWithPwdR
    { drPlainText ∷ !PlainText
    }

instance ToJSON DecryptWithPwdR where
    toJSON DecryptWithPwdR{..} = object
        [ "plain_text" .= drPlainText
        ]

instance Request DecryptWithPwd where
    type Response DecryptWithPwd = DecryptWithPwdR
    answerRequest DecryptWithPwd{..} = return ∘ fmap DecryptWithPwdR $
        decryptWithPwd dPassword dCipherText
