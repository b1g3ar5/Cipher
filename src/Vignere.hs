module Vignere
(
  ShiftCipher(..)
  , VigCipher(..)
  , BeaufortCipher(..)
  , solveShift
  , solveVig
  , solveBeaufort
) where

import Data.List
import Data.Char (ord, chr)
import Cipher
import Utils
import Analysis (corr, cShift, nchr, nord, nAlphabet, countChars, splitIC, ixOfMin, splitText, count2freq, freqDist)

newtype ShiftCipher = ShiftCipher Char deriving (Show)

instance Cipher ShiftCipher where
    cipher (ShiftCipher a) = map (cShift a)
    decipher (ShiftCipher a) = map (cShift $ nchr $ nAlphabet -  nord a)

-- 'D' is equivalent to 3
caesarCipher = ShiftCipher 'D'

solveShift :: String -> (Char, String)
solveShift ct = (c, decipher (ShiftCipher c) ct)
  where
    unshifts = fmap (\c ->  decipher (ShiftCipher c) ct) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    fds = fmap freqDist unshifts
    c = nchr $ ixOfMin fds



newtype VigCipher = VigCipher String deriving (Show)
newtype BeaufortCipher = BeaufortCipher String deriving (Show)

instance Cipher VigCipher where
    cipher   (VigCipher key) = zipWith cShift (cycle key)
    decipher (VigCipher key) = zipWith (\k c -> cShift (minus k) c) (cycle key)
      where
        minus k = nchr $ nAlphabet - nord k

instance Cipher BeaufortCipher where
    cipher   (BeaufortCipher key) = cipher (VigCipher key) . reflectTxt
    decipher (BeaufortCipher key) = cipher (VigCipher key) . reflectTxt


-- Solves Vignere returning the key and the plain text
solveVig::String->(String, String)
solveVig cipherText = (codeKey, concat $ transpose pts)
    where
    cipherCount = countChars cipherText
    cipherFreq = count2freq cipherCount
    keyICs = map (`splitIC` cipherText) [1..20]
    bestKeySize = ixOfMin (map (\d->(d-65)**2.0) keyICs) +1
    bestSplits = splitText bestKeySize cipherText -- an Array of Strings of size bestKeySize
    (codeKey, pts) = unzip $ map solveShift bestSplits

solveBeaufort :: String -> (String, String)
solveBeaufort = solveVig . reflectTxt
