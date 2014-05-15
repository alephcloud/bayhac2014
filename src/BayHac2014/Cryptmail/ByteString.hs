-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Dispatch on the ByteString implemtation
--
-- For native builds "Data.ByteString" is re-exported.
-- For haste builds a minimal subset of the API from
-- "Data.ByteString" is implemented base on SJCL bit arrays.
--
module BayHac2014.Cryptmail.ByteString
(
#ifdef __HASTE__

-- The construtor is export because it is needed for
-- FFI calls in other modules.
  ByteString(..)
, take
, drop
, splitAt
, replicate
, length
, last
#else
  module Data.ByteString
#endif
) where

-- -------------------------------------------------------------------------- --
-- Imports

import Data.Monoid
import Data.Monoid.Unicode
import Data.Word

import Prelude.Unicode
import Prelude hiding (take, drop, length, splitAt, replicate, last)

#ifdef __HASTE__

import Data.String

import Haste
import Haste.Prim

import System.IO.Unsafe (unsafePerformIO)

#else

import Data.ByteString

#endif

-- -------------------------------------------------------------------------- --
-- Haste ByteString

#ifdef __HASTE__

newtype ByteString = ByteString JSAny

foreign import ccall "bsEmpty" j_bytesEmpty ∷ IO ByteString
foreign import ccall "bsConcat" j_bytesConcat ∷ ByteString → ByteString → IO ByteString
foreign import ccall "bsEqual" j_bytesEqual ∷ ByteString → ByteString → Bool
foreign import ccall "bsSlice" j_bytesSlice ∷ ByteString → Int → Int → IO ByteString
foreign import ccall "bsLength" j_bytesLength ∷ ByteString → Int
foreign import ccall "bsReplicate" j_bytesReplicate ∷ Int → Word8 → IO ByteString
foreign import ccall "bsExtract" j_bytesExtract ∷ ByteString → Int → Int → Word8

instance Monoid ByteString where
    mempty = unsafePerformIO $ j_bytesEmpty
    mappend a b = unsafePerformIO $ j_bytesConcat a b

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance Eq ByteString where
    (==) a b = j_bytesEqual a b

    {-# INLINE (==) #-}

length ∷ ByteString → Int
length = j_bytesLength

take ∷ Int → ByteString → ByteString
take i b = unsafePerformIO $ j_bytesSlice b 0 i
{-# INLINE take #-}

drop ∷ Int → ByteString → ByteString
drop i b = unsafePerformIO $ j_bytesSlice b i (j_bytesLength b)
{-# INLINE drop #-}

splitAt ∷ Int → ByteString → (ByteString, ByteString)
splitAt i a = (take i a, drop i a)
{-# INLINE splitAt #-}

replicate ∷ Int → Word8 → ByteString
replicate i a = unsafePerformIO $ j_bytesReplicate i a
{-# INLINE replicate #-}

last ∷ ByteString → Word8
last a = j_bytesExtract a (length a) 1
{-# INLINE last #-}

-- String

foreign import ccall "utf8toBits" j_toBytesUtf8 ∷ JSString → IO ByteString

instance IsString ByteString where
    fromString = unsafePerformIO ∘ j_toBytesUtf8 ∘ toJSStr

#endif

prop_empty ∷ Bool
prop_empty = mempty ≡ ("" ∷ ByteString)

prop_lengthEmpty ∷ Bool
prop_lengthEmpty = length mempty ≡ 0

prop_lengthReplicate ∷ Int → Word8 → Bool
prop_lengthReplicate i c = i < 0 || length (replicate i c) ≡ i

prop_lengthConcat ∷ ByteString → ByteString → Bool
prop_lengthConcat a b = length (a ⊕ b) ≡ length a + length b

prop_concatSplit ∷ ByteString → ByteString → Bool
prop_concatSplit a b = splitAt (length a) (a ⊕ b) ≡ (a,b)

prop_lengthTake ∷ Int → ByteString → Bool
prop_lengthTake i a = length (take i a) ≡ min (length a) i

prop_replicateLast ∷ Int → Word8 → Bool
prop_replicateLast i c = i ≤ 0 || last (replicate i c) ≡ c

