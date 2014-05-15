-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module BayHac2014.Cryptmail.Json.Decode
( json
, value
) where

import qualified BayHac2014.Cryptmail.Text as T
import BayHac2014.Cryptmail.Json.Types

import Prelude.Unicode

#ifdef __HASTE__
import Haste.Prim
#else
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as A
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
#endif

#ifdef __HASTE__

foreign import ccall "parseJson" parseJson ∷ T.Text → Ptr (Maybe Value)

--- | Parses only Object or Array values
---
json ∷ T.Text → Maybe Value
json s = case value s of
    Just o@Object{} → Just o
    Just a@Array{} → Just a
    _ → Nothing

--- | Parses any JSON value
---
value ∷ T.Text → Maybe Value
value = fromPtr ∘ parseJson

#else

json ∷ T.Text → Maybe Value
json = fmap value2value ∘ P.maybeResult ∘ P.parse A.json ∘ T.encodeUtf8

value ∷ T.Text → Maybe Value
value = fmap value2value ∘ P.maybeResult ∘ P.parse A.value ∘ T.encodeUtf8

value2value ∷ A.Value → Value
value2value (A.Object o) = Object ∘ HM.toList ∘ HM.map value2value $ o
value2value (A.Array a) = Array ∘ V.toList ∘ V.map value2value $ a
value2value (A.String s) = String s
value2value (A.Number i) = Number ∘ D ∘ fromRational ∘ toRational $ i
value2value (A.Bool b) = Bool b
value2value A.Null = Null

#endif
