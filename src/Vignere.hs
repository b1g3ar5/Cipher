module Vignere
(
  ShiftCipher(..)
  , VigCipher(..)
  , solveVig
) where

import Data.List
import Data.Char (ord, chr)
import Cipher
import Analysis (corr, cShift, nchr, nord, nAlphabet, countChars, splitIC, ixOfMin, splitText, count2freq)

data ShiftCipher = ShiftCipher Char deriving (Show)

instance Cipher ShiftCipher where
    cipher (ShiftCipher a) = map (cShift a)
    decipher (ShiftCipher a) = map (cShift $ nchr $ nAlphabet- nord a)

-- 'D' is equivalent to 3
caesarCipher = ShiftCipher 'D'

data VigCipher = VigCipher String deriving (Show)

instance Cipher VigCipher where
    cipher (VigCipher key) = zipWith cShift (cycle key)
    decipher (VigCipher key) = zipWith (\k c -> cShift (minus k) c) (cycle key)
        where
            minus k = nchr $ nAlphabet - nord k

-- Solves Vignere returning the key and the plain text
solveVig::String->(String, String)
solveVig cipherText = (codeKey, plain)
    where
    cipherCount = countChars cipherText
    cipherFreq = count2freq cipherCount
    keyICs = map (\n-> splitIC n cipherText) [1..20]
    bestKeySize = (ixOfMin $ map (\d->(d-1.73)**2.0) keyICs) +1
    bestSplit = splitText bestKeySize cipherText -- an Array of Strings of size bestKeySize
    letterCorrelations = map (\s-> ixOfMin $ (map (\n ->(1.0-(Analysis.corr n s))**2.0) ("ABCDEFGHIJKLMNOPQRSTUVWXYZ"++[chr 95]))) bestSplit
    decodeKey = map (\i->chr $ (i + 65 )) letterCorrelations
    codeKey = fmap (\k-> nchr $ nAlphabet - nord k) decodeKey
    plain = cipher (VigCipher decodeKey) cipherText
