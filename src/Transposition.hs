module Transposition
(
  TranspositionCipher(..)
  , solveTransposition
  , solveTranspositionKnownLength
  , scytaleCipher
) where

import GHC.Exts (sortWith)
import Data.List as L
import Data.Char (ord, chr)
import Cipher hiding (keySort, unKeySort)
import Utils
import Analysis (corr, cShift, nchr, nord, nAlphabet, countChars, ixOfMin, splitText, count2freq, freqDist)
import Stats (splitIC)


-- This is a transpoisiotn cipher where the key is given by [Int]
newtype TranspositionCipher = TranspositionCipher [Int]

scytaleCipher n = TranspositionCipher [1..n]

instance Cipher TranspositionCipher where
    cipher (TranspositionCipher key) pt = L.concatMap fst $ sortWith snd $ zip (L.transpose ws) key
        where
            m = length key
            ws = chunksOf m pt
    decipher (TranspositionCipher key) ct = L.concat $ L.transpose  $ L.map fst $ sortWith snd $ zip ws unKey
        where
          unKey = L.map fst $ sortWith snd $ zip [0..] key
          m = length key
          n = length ct
          (q, r) = quotRem n m
          cols = replicate r (q+1) ++ replicate (m-r) q
          ws = chunkUsing (unKeySort key cols) ct
          -- putStrLn $ concat $ L.transpose ws

-- Sort the list and key into ascending order and just return the list
keySort :: Ord s => [s] -> [a] -> [a]
keySort ky xs = fmap snd $ sortWith fst $ zip ky xs


-- Not very efficient...
unKeySort :: Ord s => [s] -> [a] -> [a]
unKeySort ky = keySort (keySort ky [1..])


-- This works by trying all the permutations for key up to 9 in length
solveTransposition :: String -> (String, String)
solveTransposition ct = ts!!ix
  where
    ts = fmap (`solveTranspositionKnownLength` ct) [1..6]
    fds = fmap (mightBeEnglish . snd) ts
    ix = ixOfMax fds


solveTranspositionKnownLength :: Int -> String -> (String, String)
solveTranspositionKnownLength n ct = (fmap nchr $ keys!!ix, pts!!ix)
  where
    keys = permutations [0..(n-1)]
    pts = fmap (\k -> decipher (TranspositionCipher k) ct) keys
    fds = fmap mightBeEnglish pts
    ix = ixOfMax fds
