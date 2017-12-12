module Vignere
(
  ShiftCipher(..)
  , VigCipher(..)
  , AffineCipher(..)
  , BeaufortCipher(..)
  , solveShift
  , solveVig
  , solveBeaufort
  , solveAffine
) where

import Data.List
import Data.Char (ord, chr)
import Cipher
import Utils
import Analysis (corr, cShift, nchr, nord, nAlphabet, countChars, ixOfMin, splitText, count2freq, freqDist)
import Stats (splitIC)


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
data AffineCipher = AffineCipher Int Int deriving (Show)


instance Cipher VigCipher where
    cipher   (VigCipher key) = zipWith cShift (cycle key)
    decipher (VigCipher key) = zipWith (\k c -> cShift (minus k) c) (cycle key)
      where
        minus k = nchr $ nAlphabet - nord k


instance Cipher BeaufortCipher where
    cipher   (BeaufortCipher key) = cipher (VigCipher key) . reflectTxt
    decipher (BeaufortCipher key) = cipher (VigCipher key) . reflectTxt


instance Cipher AffineCipher where
    cipher   (AffineCipher a b) = fmap (\c -> nchr $ a * nord c + b `mod` nAlphabet)
    decipher (AffineCipher a b) = fmap (\c -> nchr $ a' * ( nord c - b) `mod` nAlphabet)
      where
        a' = inv a nAlphabet


inv x n = foldl (\acc y -> if acc == 0 then (if x * y `mod` n == 1 then y else 0) else acc) 0 [1..(n-1)]


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


solveAffine::String->((Int, Int), String)
solveAffine ct = ((as!!a, b), pts!!n)
  where
    as = [1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25]
    bs = [0..25]
    pts = concatMap (\a -> fmap (\b -> decipher (AffineCipher a b) ct) bs) as
    fds = fmap freqDist pts
    n = ixOfMin fds
    (a, b) = n `divMod` nAlphabet
