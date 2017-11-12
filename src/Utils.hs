{-# LANGUAGE TypeOperators #-}

module Utils
(
  chunksOf
  , chunkUsing
  , splitText
  , clean
  , isUpperChar
  , nAlphabet
  , alphabet
  , nord
  , nchr
  , cShift
  , reverseTxt
  , ixOfMin
  , ixOfMax
  , incidences
  , myAbs
  , countFrequencies
  , charDiffs
  , charFreqs
  , unC
  , reflect
  , reflectTxt
{-
  , z2i
  , i2z
  , z2int
  , int2z
  , i2int
  , int2i
  , mod2z
  , z2mod
  , int2mod
  , mod2int
-}
) where

import Data.Char
import Data.List as L
import Data.Array as A
import Data.String.Utils (split)
import Data.Maybe
--import Numeric.LinearAlgebra hiding (accum)
import GHC.TypeLits
import Control.Arrow ((&&&))

{-
z2i :: Z -> I
z2i = fromIntegral
i2z :: I -> Z
i2z = fromIntegral

z2int :: Z -> Int
z2int = fromIntegral
int2z :: Int -> Z
int2z = fromIntegral1

i2int :: I -> Int
i2int = fromIntegral
int2i :: Int -> I
int2i = fromIntegral

mod2z :: KnownNat n => (Z ./. n) -> Z
mod2z = fromIntegral

z2mod :: KnownNat n => Z -> (Z ./. n)
z2mod = fromIntegral

mod2int :: KnownNat n => (Z ./. n) -> Int
mod2int = fromIntegral

int2mod :: KnownNat n => Int -> (Z ./. n)
int2mod = fromIntegral

-}

chunksOf::Int->[a]->[[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


chunkUsing::[Int]->[a]->[[a]]
chunkUsing [] xs = [xs]
chunkUsing (n:ns) xs = take n xs : chunkUsing ns (drop n xs)


split'::Int->Int->Array Int [a]->[a]->Array Int [a]
split' i n acc [] = acc
split' i n acc (x:xs) = split' (mod (i+1) n) n (accum (++) acc [(i, [x])]) xs


-- split a list into n bits alternating which bit to put the next item in
splitText::Int->[a]->[[a]]
splitText n xs = L.transpose $ chunksOf n xs


clean::(Char->Bool)->String->String
clean _ [] = []
clean f (x:xs) = if f x then x : clean f xs else clean f xs


isUpperChar::Char->Bool
isUpperChar x = (nord x >= 0) && (nord x <26)


-- The number of letters in the alphabet
nAlphabet = 26::Int
alphabet = "ABCDEFGHIJLMNOPQRSTUVWXYZ"


-- Calculates ord where ord 'A'=0 etc.
nord::Char->Int
nord c = ord c - ord 'A'


-- Calculates the letter where 0 gives 'A'
nchr::Int->Char
nchr i = chr $ ord 'A' + i

reflect :: Char -> Char
reflect c = nchr $ 25 - nord c


-- unC is minus c, ie. to work out what you came from with a shift of c
unC :: Char -> Char
unC c = nchr $ nAlphabet - nord c


-- Shifts right according to an alphabet with nAlphabet Chars
cShift::Char->Char->Char
cShift k c = nchr $ mod (nord k + nord c) nAlphabet


-- Switches a->z, b->y etc (when n=26)
reverseTxt::String->String
reverseTxt = L.map (\c-> chr $ nAlphabet - 1 + 2*65 - ord c)

reflectTxt :: String -> String
reflectTxt = fmap reflect


-- Index of the minimum starting with 0
ixOfMin::Ord a=>[a]->Int
ixOfMin xs = fromJust $ elemIndex (minimum xs) xs


-- Index of the minimum starting with 0
ixOfMax::Ord a=>[a]->Int
ixOfMax xs = fromJust $ elemIndex (maximum xs) xs


incidences :: Char -> String -> [Int]
incidences = elemIndices


myAbs:: Int -> Int
myAbs x = if x>0 then x else -x


countFrequencies:: Ord a => [a]->[(a, Int)]
countFrequencies = L.map (head &&& length) . L.group . L.sort


charDiffs :: Char -> Char ->String->[Int]
charDiffs a b ct = L.concatMap (\hpos -> L.map (\tpos -> gcd (length ct) $ myAbs (hpos - tpos)) $ elemIndices a ct) $ elemIndices b ct


charFreqs:: Char -> Char -> String -> [(Int, Int)]
charFreqs a b ct = countFrequencies $ charDiffs a b ct
