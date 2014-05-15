-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BayHac2014.Cryptmail.Json.Instances ()
where

import Data.Monoid.Unicode

import Control.Applicative

import Data.Word
import qualified Data.Set as S

import Prelude.Unicode

import qualified BayHac2014.Cryptmail.Text as T
import BayHac2014.Cryptmail.Json.Types

-- ---------------------------------- --
-- Basic Values

instance ToJSON Value where
    toJSON = id
    {-# INLINE toJSON #-}

instance FromJSON Value where
    parseJSON = pure
    {-# INLINE parseJSON #-}

instance ToJSON () where
    toJSON _ = Array []
    {-# INLINE toJSON #-}

instance FromJSON () where
    parseJSON = withArray "()" $ \case
        [] → pure ()
        _ → fail "Expected an empty array"
    {-# INLINE parseJSON #-}

instance ToJSON String where
    toJSON = String ∘ T.pack
    {-# INLINE toJSON #-}

instance FromJSON String where
    parseJSON = withString "String" pure
    {-# INLINE parseJSON #-}

instance ToJSON T.Text where
    toJSON = String
    {-# INLINE toJSON #-}

instance FromJSON T.Text where
    parseJSON = withText "Text" pure
    {-# INLINE parseJSON #-}

instance ToJSON Bool where
    toJSON = Bool
    {-# INLINE toJSON #-}

instance FromJSON Bool where
    parseJSON = withBool "Bool" pure
    {-# INLINE parseJSON #-}

-- ---------------------------------- --
-- Functors (and alike)

instance ToJSON α ⇒ ToJSON (Maybe α) where
    toJSON Nothing = Null
    toJSON (Just a) = toJSON a
    {-# INLINE toJSON #-}

instance FromJSON α ⇒ FromJSON (Maybe α) where
    parseJSON Null = pure Nothing
    parseJSON a = Just <$> parseJSON a
    {-# INLINE parseJSON #-}

instance ToJSON α ⇒ ToJSON [α] where
    toJSON = Array ∘ map toJSON
    {-# INLINE toJSON #-}

instance FromJSON α ⇒ FromJSON [α] where
    parseJSON = withArray "[α]" $ mapM parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON α, ToJSON β) ⇒ ToJSON (α, β) where
    toJSON (a, b) = Array $ [toJSON a, toJSON b]
    {-# INLINE toJSON #-}

instance (FromJSON α, FromJSON β) ⇒ FromJSON (α, β) where
    parseJSON = withArray "(a,b)" $ \case
        [a,b] → (,) <$> parseJSON a <*> parseJSON b
        l → fail $ "cannot unpack array of length " ⊕ show (length l) ⊕ " into a tupple"
    {-# INLINE parseJSON #-}

instance (ToJSON α, ToJSON β, ToJSON γ) ⇒ ToJSON (α, β, γ) where
    toJSON (a, b, c) = Array $ [toJSON a, toJSON b, toJSON c]
    {-# INLINE toJSON #-}

instance (FromJSON α, FromJSON β, FromJSON γ) ⇒ FromJSON (α, β, γ) where
    parseJSON = withArray "(α, β, γ)" $ \case
        [a,b,c] → (,,) <$> parseJSON a <*> parseJSON b <*> parseJSON c
        l → fail $ "cannot unpack array of length " ⊕ show (length l) ⊕ " into a tripple"
    {-# INLINE parseJSON #-}

instance ToJSON α ⇒ ToJSON (S.Set α) where
    toJSON = toJSON ∘ S.toList
    {-# INLINE toJSON #-}

instance (Ord α, FromJSON α) ⇒ FromJSON (S.Set α) where
    parseJSON = fmap S.fromList ∘ parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON α, ToJSON β) ⇒ ToJSON (Either α β) where
    toJSON (Left a)  = object [jsleft .= a]
    toJSON (Right b) = object [jsright .= b]
    {-# INLINE toJSON #-}

instance (FromJSON α, FromJSON β) ⇒ FromJSON (Either α β) where
    parseJSON (Object [(key, value)])
        | key ≡ jsleft  = Left  <$> parseJSON value
        | key ≡ jsright = Right <$> parseJSON value
    parseJSON _ = fail ""
    {-# INLINE parseJSON #-}

jsleft, jsright ∷ T.Text
jsleft = "Left"
jsright = "Right"
{-# INLINE jsleft #-}
{-# INLINE jsright #-}

-- ---------------------------------- --
-- Numbers

numToJson ∷ Integral α ⇒ α → Value
numToJson = Number ∘ fromInteger ∘ toInteger
{-# INLINE numToJson #-}

instance ToJSON Number where
    toJSON = Number
    {-# INLINE toJSON #-}

instance FromJSON Number where
    parseJSON (Number n) = pure n
    parseJSON Null = pure (D (0/0))
    parseJSON v = typeMismatch "Number" v
    {-# INLINE parseJSON #-}

instance ToJSON Integer where
    toJSON = Number ∘ fromInteger
    {-# INLINE toJSON #-}

instance FromJSON Integer where
    parseJSON = withNumber "Integer" $ return ∘ floor
    {-# INLINE parseJSON #-}

instance ToJSON Int where
    toJSON = numToJson
    {-# INLINE toJSON #-}

instance FromJSON Int where
    parseJSON = withNumber "Integral" $ return ∘ floor
    {-# INLINE parseJSON #-}

instance ToJSON Word8 where
    toJSON = numToJson
    {-# INLINE toJSON #-}

instance FromJSON Word8 where
    parseJSON = withNumber "Word8" $ return ∘ floor
    {-# INLINE parseJSON #-}

instance ToJSON Word16 where
    toJSON = numToJson
    {-# INLINE toJSON #-}

instance FromJSON Word16 where
    parseJSON = withNumber "Word16" $ return ∘ floor
    {-# INLINE parseJSON #-}

instance ToJSON Word32 where
    toJSON = numToJson
    {-# INLINE toJSON #-}

instance FromJSON Word32 where
    parseJSON = withNumber "Word32" $ return ∘ floor
    {-# INLINE parseJSON #-}

instance ToJSON Word64 where
    toJSON = numToJson
    {-# INLINE toJSON #-}

instance FromJSON Word64 where
    parseJSON = withNumber "Word64" $ return ∘ floor
    {-# INLINE parseJSON #-}

instance ToJSON Double where
    toJSON = Number ∘ D
    {-# INLINE toJSON #-}

instance FromJSON Double where
    parseJSON (Number n) = case n of
        D d → pure d
        I i → pure (fromIntegral i)
    parseJSON Null = pure (0/0)
    parseJSON v = typeMismatch "Double" v
    {-# INLINE parseJSON #-}

