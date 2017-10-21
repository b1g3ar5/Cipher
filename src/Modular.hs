{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Types for working with integers modulo some constant.
module Modular (
  -- $doc

  -- * Preliminaries
  -- $setup

  -- * Modular arithmetic
  MMod(..)
  , unMod
  , toMod
  , toMod'
  , inv
  , type (/)()
  , ℤ
  , modVal
  , SomeMod
  , someModVal
) where

import Control.Arrow (first)
import Data.Proxy    (Proxy (..))
import Data.Ratio    ((%))
import GHC.TypeLits
import Numeric.LinearAlgebra hiding (Mod(..), inv)
import Foreign.Storable

-- $setup
--
-- To use type level numeric literals you need to enable
-- the @DataKinds@ extension:
--
-- >>> :set -XDataKinds
--
-- To use infix syntax for @'Mod'@ or the @/@ synonym,
-- enable @TypeOperators@:
--
-- >>> :set -XTypeOperators

-- $doc
--
-- @'Mod'@ and its synonym @/@ let you wrap arbitrary numeric types
-- in a modulus. To work with integers (mod 7) backed by @'Integer'@,
-- you could use one of the following equivalent types:
--
-- > Mod Integer 7
-- > Integer `Mod` 7
-- > Integer/7
-- > ℤ/7
--
-- (@'ℤ'@ is a synonym for @'Integer'@ provided by this library. In
-- Emacs, you can use the TeX input mode to type it with @\\Bbb{Z}@.)
--
-- The usual numeric typeclasses are defined for these types. You can
-- always extract the underlying value with @'unMod'@.
--
-- Here is a quick example:
--
-- >>> 10 * 11 :: ℤ/7
-- 5
--
-- It also works correctly with negative numeric literals:
--
-- >>> (-10) * 11 :: ℤ/7
-- 2
--
-- Modular division is an inverse of modular multiplication.
-- It is defined when divisor is coprime to modulus:
--
-- >>> 7 `div` 3 :: ℤ/16
-- 13
-- >>> 3 * 13 :: ℤ/16
-- 7
--
-- To use type level numeric literals you need to enable the
-- @DataKinds@ extension and to use infix syntax for @Mod@ or the @/@
-- synonym, you need @TypeOperators@.

-- | Wraps an underlying @Integeral@ type @i@ in a newtype annotated
-- with the bound @n@.
newtype i `MMod` (n :: Nat) = MMod i deriving (Eq, Ord, Storable)

instance (Integral i, KnownNat n) => Num (i `MMod` n) where
  fromInteger = toMod . fromInteger
  MMod i₁ + MMod i₂ = toMod $ i₁ + i₂
  MMod i₁ * MMod i₂ = toMod $ i₁ * i₂
  abs    (MMod i) = toMod $ abs i
  signum (MMod i) = toMod $ signum i
  negate (MMod i) = toMod $ negate i

instance (Integral i, KnownNat n) => Enum (i `MMod` n) where
  toEnum = fromInteger . toInteger
  fromEnum = fromInteger . toInteger . unMod
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise               = minBound

instance (Integral i, KnownNat n) => Bounded (i `MMod` n) where
  maxBound = pred _bound
  minBound = 0

instance (Integral i, KnownNat n) => Real (i `MMod` n) where
  toRational (MMod i) = toInteger i % 1

-- | Integer division uses modular inverse @'inv'@, so it is possible
-- to divide only by numbers coprime to @n@ and the remainder is
-- always @0@.
instance (Integral i, KnownNat n) => Integral (MMod i n) where
  toInteger (MMod i) = toInteger i
  i₁ `quotRem` i₂ = (i₁ * inv i₂, 0)

instance (KnownNat n, Integral i, Storable i) => Element (MMod i n)

instance (Fractional i, Integral i, KnownNat n) => Fractional (MMod i n) where
  n / m = headOrError [x | x <- map toMod [0..25], (x*m) == n]
  fromRational x = toMod $ fromRational x

-- Container Vector t, Container Matrix t, Konst t Int Vector, Konst t (Int, Int) Matrix, Product t
--instance (KnownNat n, Numeric i, Integral i) => Numeric (Mod i n)

instance (KnownNat n, Integral i, Storable i, Numeric i) => Container Vector (MMod i n)
instance (KnownNat n, Integral i, Storable i) => Product (MMod i n)

--instance (KnownNat n) => CTrans (Mod i n) where
--  ctrans m = toMod n $ trans $ unMod m

headOrError :: [a] -> a
headOrError [] = error "No inverse"
headOrError (x:xs) = x

-- | Extract the underlying integral value from a modular type.
unMod :: i `MMod` n -> i
unMod (MMod i) = i

-- | A synonym for @Mod@, inspired by the ℤ/n syntax from mathematics.
type (/) = MMod

-- | A synonym for Integer, also inspired by the ℤ/n syntax.
type ℤ   = Integer

-- | Returns the bound of the modular type in the type itself. This
-- breaks the invariant of the type, so it shouldn't be used outside
-- this module.
_bound :: forall n i. (Integral i, KnownNat n) => i `MMod` n
_bound = MMod . fromInteger $ natVal (Proxy :: Proxy n)

-- | Injects a value of the underlying type into the modulus type,
-- wrapping as appropriate.
toMod :: forall n i. (Integral i, KnownNat n) => i -> i `MMod` n
toMod i = MMod $ i `mod` unMod (_bound :: i `MMod` n)

-- | Wraps an integral number, converting between integral types.
toMod' :: forall n i j. (Integral i, Integral j, KnownNat n) => i -> j `MMod` n
toMod' i = toMod . fromIntegral $ i `mod` (fromInteger $ natVal (Proxy :: Proxy n))

instance Show i => Show (i `MMod` n) where show (MMod i) = show i
instance (Read i, Integral i, KnownNat n) => Read (i `MMod` n)
  where readsPrec prec = map (first toMod) . readsPrec prec


-- | The modular inverse.
--
-- >>> inv 3 :: ℤ/7
-- 5
-- >>> 3 * 5 :: ℤ/7
-- 1
--
-- Note that only numbers coprime to @n@ have an inverse modulo @n@:
--
-- >>> inv 6 :: ℤ/15
-- *** Exception: divide by 6 (mod 15), non-coprime to modulus
--
inv :: forall n i. (KnownNat n, Integral i) => MMod i n -> MMod i n
inv k = toMod . snd . inv' (fromInteger (natVal (Proxy :: Proxy n))) . unMod $ k
  where
    -- these are only used for error message
    modulus = show $ natVal (Proxy :: Proxy n)
    divisor = show (toInteger k)

    -- backwards Euclidean algorithm
    inv' _ 0 = error ("divide by " ++ divisor ++ " (mod " ++ modulus ++ "), non-coprime to modulus")
    inv' _ 1 = (0, 1)
    inv' n x = (r', q' - r' * q)
      where
        (q,  r)  = n `quotRem` x
        (q', r') = inv' x r

-- | A modular number with an unknown bound.
data SomeMod i where
  SomeMod :: forall i (n :: Nat). KnownNat n => MMod i n -> SomeMod i

instance Show i => Show (SomeMod i) where
  showsPrec p (SomeMod x) = showsPrec p x

-- | Convert an integral number @i@ into a @'Mod'@ value given modular
-- bound @n@ at type level.
modVal :: forall i proxy n. (Integral i, KnownNat n) => i -> proxy n -> MMod i n
modVal i _ = toMod i

-- | Convert an integral number @i@ into a @'Mod'@ value with an
-- unknown modulus.
someModVal :: Integral i => i -> Integer -> Maybe (SomeMod i)
someModVal i n =
  case someNatVal n of
    Nothing -> Nothing
    Just (SomeNat proxy) -> Just (SomeMod (modVal i proxy))
