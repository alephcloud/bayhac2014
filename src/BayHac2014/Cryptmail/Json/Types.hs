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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BayHac2014.Cryptmail.Json.Types
(

-- * External

-- ** Value
  Value(..)
, Object
, Array
, Number(..)

-- ** FromJSON and ToJSON classes
, ToJSON(..)
, FromJSON(..)

-- ** Value Builders
, (.=)
, object

-- ** Value Parsers
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
, withBool
, typeMismatch

-- * Internal

, JSONParser
, Result(..)
, emptyArray
, emptyObject
, isEmptyArray

) where

import Data.Monoid.Unicode

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.Monoid
import Data.String
import Data.Typeable
import Data.Function (on)

import Prelude.Unicode

import qualified BayHac2014.Cryptmail.Text as T

#ifdef __HASTE__

import Haste hiding (fromString)

#endif

#ifdef TRACE
import System.IO.Unsafe

trace ∷ String → α → α
trace s a = t `seq` a
  where
    t = unsafePerformIO $ writeLog s
#endif

-- -------------------------------------------------------------------------- --

data Value
    = Number !Number
    | String !T.Text
    | Bool !Bool
    | Null
    | Array !Array
    | Object !Object
    deriving (Eq, Ord, Show, Typeable)

instance IsString Value where
    fromString = String ∘ fromString
    {-# INLINE fromString #-}

type Array = [Value]

-- | Hashable does currently not compile with Haste. So we
-- use Map instead.
--
type Object = [Pair]

-- | A key\/value pair for an 'Object'.
type Pair = (T.Text, Value)

{-
instance NFData Value where
    rnf (Object o) = rnf o
    rnf (Array a)  = V.foldl' (\x y → rnf y `seq` x) () a
    rnf (String s) = rnf s
    rnf (Number n) = case n of I i → rnf i; D d → rnf d
    rnf (Bool b)   = rnf b
    rnf Null       = ()
-}

-- -------------------------------------------------------------------------- --
-- The Aeson Parser adapted to our needs
--

-- | The result of running a 'Parser'.
data Result α
    = Error !String
    | Success !α
    deriving (Eq, Show, Typeable)

{-
instance (NFData a) ⇒ NFData (Result a) where
    rnf (Success a) = rnf a
    rnf (Error err) = rnf err
-}

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad Result where
    return = Success
    {-# INLINE return #-}
    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

-- -------------------------------------------------------------------------- --

class (Monad π, Applicative π, Functor π, Alternative π, MonadPlus π) ⇒ JSONParser π

-- | The empty array.
emptyArray ∷ Value
emptyArray = Array []
{-# INLINE emptyArray #-}

-- | Determines if the 'Value' is an empty 'Array'.
-- Note that: @isEmptyArray 'emptyArray'@.
isEmptyArray ∷ Value → Bool
isEmptyArray (Array arr) = null arr
isEmptyArray _ = False
{-# INLINE isEmptyArray #-}

-- | The empty object.
emptyObject ∷ Value
emptyObject = Object []
{-# INLINE emptyObject #-}

jsfail ∷ JSONParser π ⇒ String → π α
jsfail = fail
{-# INLINE jsfail #-}

(.=) ∷ (ToJSON α) ⇒ T.Text → α → (T.Text, Value)
(.=) p a = (p, toJSON a)
{-# INLINE (.=) #-}

-- We may consider integrating this into the JSParser typeclass
#ifdef TRACE

(.:) ∷ (JSONParser π, FromJSON α) ⇒ Object → T.Text → π α
(.:) o s = maybe (fail $ "missing JSON property: " ⊕ T.unpack s) parser $ lookup s o
  where
    parser !v = trace ("parseJSON: " ⊕ show s) $ do
        x ← parseJSON v
        return $ trace ("DONE parseJSON: " ⊕ show s) x
{-# INLINE (.:) #-}

#else

(.:) ∷ (JSONParser π, FromJSON α) ⇒ Object → T.Text → π α
(.:) o s = maybe (fail $ "missing JSON property: " ⊕ T.unpack s) parseJSON $ lookup s o
{-# INLINE (.:) #-}

#endif

(.:?) ∷ (FromJSON (Maybe α), JSONParser π, FromJSON α) ⇒ Object → T.Text → π (Maybe α)
(.:?) o s = maybe (return Nothing) parseJSON $ lookup s o
{-# INLINE (.:?) #-}

(.!=) ∷ JSONParser π ⇒ π (Maybe α) → α → π α
(.!=) a b = fromMaybe b <$> a
{-# INLINE (.!=) #-}

withArray ∷ JSONParser π ⇒ String → (Array → π α) → Value → π α
withArray _ p (Array a) = p a
withArray s _ a = typeMismatch s a
{-# INLINE withArray #-}

withObject ∷ JSONParser π ⇒ String → (Object → π α) → Value → π α
withObject _ p (Object a) = p a
withObject s _ a = typeMismatch s a
{-# INLINE withObject #-}

withText ∷ JSONParser π ⇒ String → (T.Text → π α) → Value → π α
withText _ p (String a) = p a
withText s _ a = typeMismatch s a
{-# INLINE withText #-}

withString ∷ JSONParser π ⇒ String → (String → π α) → Value → π α
withString _ p (String a) = p (T.unpack a)
withString s _ a = typeMismatch s a
{-# INLINE withString #-}

withBool ∷ JSONParser π ⇒ String → (Bool → π α) → Value → π α
withBool _ p (Bool a) = p a
withBool s _ a = typeMismatch s a
{-# INLINE withBool #-}

withNumber ∷ JSONParser π ⇒ String → (Number → π α) → Value → π α
withNumber _ p (Number a) = p a
withNumber s _ a = typeMismatch s a
{-# INLINE withNumber #-}

withDouble ∷ JSONParser π ⇒ String → (Double → π α) → Value → π α
withDouble _ p (Number (D a)) = p a
withDouble s _ a = typeMismatch s a
{-# INLINE withDouble #-}

withInteger ∷ JSONParser π ⇒ String → (Integer → π α) → Value → π α
withInteger _ p (Number (I a)) = p a
withInteger s _ a = typeMismatch s a
{-# INLINE withInteger #-}

object ∷ [Pair] → Value
object = Object
{-# INLINE object #-}

-- | Fail parsing due to a type mismatch, with a descriptive message.
--
-- copied from "Data.Aeson.Types.Class"
--
typeMismatch
    ∷ JSONParser π
    ⇒ String -- ^ The name of the type you are trying to parse.
    → Value  -- ^ The actual value encountered.
    → π a
typeMismatch expected actual =
    fail $ "when expecting a " ⊕ expected ⊕ ", encountered " ⊕ name ⊕ " instead"
    where
    name = case actual of
             Object _ → "Object"
             Array _  → "Array"
             String _ → "String"
             Number (I _) → "Integer"
             Number (D _) → "Double"
             Bool _   → "Boolean"
             Null     → "Null"

-- -------------------------------------------------------------------------- --

class ToJSON α where
    toJSON ∷ α → Value

class FromJSON α where
    parseJSON ∷ (∀ π . JSONParser π ⇒ Value → π α)

-- -------------------------------------------------------------------------- --

-- | Number data-type from "Data.Attoparsec"
--
-- Mostly copy and pasted.
--
-- Copyright   :  Bryan O'Sullivan 2011
-- License     :  BSD3
--
data Number
    = I !Integer
    | D !Double

instance Show Number where
    show (I a) = show a
    show (D a) = show a

binop
    ∷ (Integer → Integer → α)
    → (Double → Double → α)
    → Number
    → Number
    → α
binop _ d (D a) (D b) = d a b
binop i _ (I a) (I b) = i a b
binop _ d (D a) (I b) = d a (fromIntegral b)
binop _ d (I a) (D b) = d (fromIntegral a) b
{-# INLINE binop #-}

instance Eq Number where
    (==) = binop (==) (==)
    {-# INLINE (==) #-}

    (/=) = binop (/=) (/=)
    {-# INLINE (/=) #-}

instance Ord Number where
    (<) = binop (<) (<)
    {-# INLINE (<) #-}

    (<=) = binop (<=) (<=)
    {-# INLINE (<=) #-}

    (>) = binop (>) (>)
    {-# INLINE (>) #-}

    (>=) = binop (>=) (>=)
    {-# INLINE (>=) #-}

    compare = binop compare compare
    {-# INLINE compare #-}

instance Num Number where
    (+) = binop (((I$!).) . (+)) (((D$!).) . (+))
    {-# INLINE (+) #-}

    (-) = binop (((I$!).) . (-)) (((D$!).) . (-))
    {-# INLINE (-) #-}

    (*) = binop (((I$!).) . (*)) (((D$!).) . (*))
    {-# INLINE (*) #-}

    abs (I a) = I $! abs a
    abs (D a) = D $! abs a
    {-# INLINE abs #-}

    negate (I a) = I $! negate a
    negate (D a) = D $! negate a
    {-# INLINE negate #-}

    signum (I a) = I $! signum a
    signum (D a) = D $! signum a
    {-# INLINE signum #-}

    fromInteger = I
    {-# INLINE fromInteger #-}

instance Real Number where
    toRational (I a) = fromIntegral a
    toRational (D a) = toRational a
    {-# INLINE toRational #-}

instance Fractional Number where
    fromRational = (D$!) . fromRational
    {-# INLINE fromRational #-}

    (/) = binop (((D$!).) . (/) `on` fromIntegral)
                (((D$!).) . (/))
    {-# INLINE (/) #-}

    recip (I a) = D $! recip (fromIntegral a)
    recip (D a) = D $! recip a
    {-# INLINE recip #-}

instance RealFrac Number where
    properFraction (I a) = (fromIntegral a,0)
    properFraction (D a) = case properFraction a of
                             (i,d) → (i,D d)
    {-# INLINE properFraction #-}
    truncate (I a) = fromIntegral a
    truncate (D a) = truncate a
    {-# INLINE truncate #-}
    round (I a) = fromIntegral a
    round (D a) = round a
    {-# INLINE round #-}
    ceiling (I a) = fromIntegral a
    ceiling (D a) = ceiling a
    {-# INLINE ceiling #-}
    floor (I a) = fromIntegral a
    floor (D a) = floor a
    {-# INLINE floor #-}

