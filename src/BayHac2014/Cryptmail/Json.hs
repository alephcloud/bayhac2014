{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module BayHac2014.Cryptmail.Json
(
-- * Types
  ToJSON(..)
, FromJSON(..)
, Value(..)
, Object
, Array
, Number(..)

, JSONParser
, Parser
, Result(..)
, parseMaybe
, parseEither
, parseIO

-- * Value Builders
, (.=)
, object

-- * Value Parsers
, jsfail
, (.:)
, (.:?)
, (.!=)
, withArray
, withObject
, withText
, withString
, withNumber
, withDouble
, withInteger
, modifyFailure

-- * Serialization
, encode
, encodeValue

-- * Deserialization
, fromJSON
, eitherFromJSON
, ioFromJSON
, eitherDecode
, eitherDecodeValue
, ioDecode
, json
, value

) where

import Data.Monoid.Unicode

import Control.Applicative
import Control.Monad

import Data.Monoid

import Prelude.Unicode

import qualified BayHac2014.Cryptmail.Text as T
import BayHac2014.Cryptmail.Json.Instances ()
import BayHac2014.Cryptmail.Json.Decode
import BayHac2014.Cryptmail.Json.Encode
import BayHac2014.Cryptmail.Json.Types

-- | Failure continuation.
type Failure μ ρ = String → μ ρ

-- | Success continuation.
type Success α μ ρ = α → μ ρ

-- | A continuation-based parser type.
newtype Parser α = Parser { runParser ∷ ∀ μ ρ.  Failure μ ρ → Success α μ ρ → μ ρ }

instance JSONParser Parser

instance Monad Parser where
    m >>= g = Parser $ \kf ks →
        let ks' a = runParser (g a) kf ks
        in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks → ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks → kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks →
        let ks' a = ks (f a)
        in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks →
        let kf' _ = runParser b kf ks
        in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP ∷ Parser (α → β) → Parser α → Parser β
apP d e = do
  b ← d
  a ← e
  return (b a)
{-# INLINE apP #-}

-- | Run a 'Parser'.
parse ∷ (α → Parser β) → α → Result β
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe ∷ (α → Parser β) → α → Maybe β
parseMaybe m v = runParser (m v) (const Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.
parseEither ∷ (α → Parser β) → α → Either String β
parseEither m v = runParser (m v) Left Right
{-# INLINE parseEither #-}

parseIO ∷ (α → Parser β) → α → (String → IO ()) → (β → IO ()) → IO ()
parseIO m v failCallback successCallback =
    runParser (m v) failCallback successCallback
{-# INLINE parseIO #-}

-- | If the inner @Parser@ failed, modify the failure message using the
-- provided function. This allows you to create more descriptive error messages.
-- For example:
--
-- > parseJSON (Object o) = modifyFailure
-- >     ("Parsing of the Foo value failed: " ++)
-- >     (Foo <$> o .: "someField")
--
-- Since 0.6.2.0
modifyFailure ∷ (String → String) → Parser a → Parser a
modifyFailure f (Parser p) = Parser $ \kf → p (kf . f)

-- -------------------------------------------------------------------------- --
-- * Decoding

-- | Value → α
--
fromJSON
    ∷ FromJSON α
    ⇒ Value
    → Result α
fromJSON = parse parseJSON
{-# INLINE fromJSON #-}

-- | Value → α
--
eitherFromJSON
    ∷ FromJSON α
    ⇒ Value
    → Either String α
eitherFromJSON = parseEither parseJSON
{-# INLINE eitherFromJSON #-}

ioFromJSON
    ∷ FromJSON α
    ⇒ Value
    → (String → IO ())
    → (α → IO ())
    → IO ()
ioFromJSON = parseIO parseJSON
{-# INLINE ioFromJSON #-}

eitherDecode
    ∷ FromJSON α
    ⇒ T.Text
    → Either String α
eitherDecode s = eitherFromJSON <=< maybe (Left $ "failed to decode JSON: " ⊕ T.unpack s) return ∘ json $ s
{-# INLINE eitherDecode #-}

eitherDecodeValue
    ∷ FromJSON α
    ⇒ T.Text
    → Either String α
eitherDecodeValue s = eitherFromJSON <=< maybe (Left $ "failed to decode JSON: " ⊕ T.unpack s) return ∘ value $ s
{-# INLINE eitherDecodeValue #-}

ioDecode
    ∷ FromJSON α
    ⇒ T.Text
    → (String → IO ())
    → (α → IO ())
    → IO ()
ioDecode s err suc = do
    case value s of
        Nothing → err $ "failed to decode JSON: " ⊕ T.unpack s
        Just v → ioFromJSON v err suc
{-# INLINE ioDecode #-}

