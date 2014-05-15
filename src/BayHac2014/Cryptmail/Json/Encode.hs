-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module BayHac2014.Cryptmail.Json.Encode
( encodeValue
, encode
) where

import qualified BayHac2014.Cryptmail.Text as T
import BayHac2014.Cryptmail.Json.Types

import Prelude.Unicode

#ifdef __HASTE__

import Data.Function (on)
import Data.List (sortBy)

import Haste
import Haste.Prim

#else

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

#endif

-- -------------------------------------------------------------------------- --

#ifdef __HASTE__

foreign import ccall "jsShowI" jsShowD ∷ Double → JSString
foreign import ccall "jsUnquote" jsUnquote ∷ JSString → JSString
foreign import ccall "jsonStringify" jsStringify ∷ JSString → JSString

-- -------------------------------------------------------------------------- --
-- * Encoding (Value → JSString)

-- | Mostly copy and pasted from Haste.JSON
--
-- This function always normalizes the JSON string
--
encodeValue ∷ Value → T.Text
encodeValue = catJSStr "" . enc []
  where
    comma = ","
    openbr = "["
    closebr = "]"
    opencu = "{"
    closecu = "}"
    colon = ":"
    quote = "\""
    true = "true"
    false = "false"
    nul = "null"
    enc acc (String s) = jsStringify s : acc
    -- enc acc (Number (I i)) = jsShowI i : acc            -- FIXME use this if number is small enough
    enc acc (Number (I i)) = toJSStr (show i) : acc
    enc acc (Number (D d)) = jsShowD d : acc
    enc acc (Bool True) = true : acc
    enc acc (Bool False) = false : acc
    enc acc Null = nul : acc
    enc acc (Array elems)
      | (x:xs) ← elems =
        openbr : enc (foldr (\s a → comma:enc a s) (closebr:acc) xs) x
      | otherwise =
        openbr : closebr : acc
    enc acc (Object elems)
      | ((key,val):xs) ← sortKeys elems =
        let encElem (k, v) a = comma : quote : k : quote : colon : enc a v
            encAll = opencu : quote : jsUnquote key : quote : colon : encRest
            encRest  = enc (foldr encElem (closecu:acc) xs) val
        in encAll
      | otherwise =
        opencu : closecu : acc
    sortKeys = sortBy (compare `on` fst)

#else

encodeValue ∷ Value → T.Text
encodeValue = T.decodeUtf8 ∘ BL.toStrict ∘ A.encode ∘ value2value
  where
    value2value ∷ Value → A.Value
    value2value (Object o) = A.Object ∘ HM.map value2value ∘ HM.fromList $ o
    value2value (Array a) = A.Array ∘ V.fromList ∘ map value2value $ a
    value2value (String s) = A.String s
    value2value (Number (D i)) = A.Number ∘ fromRational ∘ toRational $ i
    value2value (Number (I i)) = A.Number ∘ fromRational ∘ toRational $ i
    value2value (Bool b) = A.Bool b
    value2value Null = A.Null


#endif

encode
    ∷ ToJSON α
    ⇒ α
    → T.Text
encode = encodeValue ∘ toJSON

