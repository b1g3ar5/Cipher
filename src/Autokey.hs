module Autokey
(
  AutokeyCipher(..)
  , solveAutokey
  , topChars
  , keyLength
) where

import Data.List
import Data.Char (ord, chr)
import Cipher
import Utils
import Analysis
import Vignere
import Debug.Trace


newtype AutokeyCipher = AutokeyCipher String deriving (Show)


instance Cipher AutokeyCipher where
  cipher   (AutokeyCipher key) pt = zipWith cShift (key ++ pt) pt
  decipher k@(AutokeyCipher key) ct = zipWith (\k c -> cShift (minus k) c) (key ++ decipher k ct) ct
    where
      minus k = nchr $ nAlphabet - nord k


solveAutokey::String->(String, String)
solveAutokey ct = snd $ head $ (sortOn fst $ zip fss $ zip pwds pts)
  where
    -- Work out the key length
    lens = keyLength ct
    -- Work out the top 3 for each letterFreq
    tops = fmap  (\ix -> topChars ct (lens!!1) ix 3) [0..((lens!!1)-1)]
    -- Try each password from these
    pwds = makePwds tops
    pts = fmap (\pwd -> decipher (AutokeyCipher pwd)  ct) pwds
    fss = fmap quadgramScore pts


combine :: [[a]] -> [[a]]
combine = sequence


makePwds :: [String] -> [String]
makePwds topCharss = combine topCharss


topChars :: String -> Int -> Int -> Int -> String
topChars ct keyLen ix n = fmap snd $ take n $ sortOn fst $ zip fss "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  where
    pts = fmap (\c -> decipher (AutokeyCipher $ keyGen c)  ct) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    fss = fmap quadgramScore pts
    keyGen c = replicate (ix-1) 'A' ++ [c] ++ replicate (keyLen-ix) 'A'


keyLength :: String -> [Int]
keyLength ct = fmap snd $ take 3 $ sortOn fst $ zip fss [1..10]
  where
    pts :: [String]
    pts = fmap (\i -> decipher (AutokeyCipher $ replicate i 'A')  ct) [1..10]
    fss :: [Double]
    fss = fmap quadgramScore pts
