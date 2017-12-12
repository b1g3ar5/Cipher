module Stats
(
    incidenceOfCoincidence
  , normalisedIncidenceOfCoincidence
  , englishness
  , splitIC
  , ic
  , mic
  , mka
  , dic
  , edi
  , lr
  , ldi
  , sdd
) where

import Data.List as L
import Data.Char (ord, chr)
import Analysis
import Data.Map as M

{-

IC is the Index of Coincidence multiplied by 1000.
 MIC is the maximum Index of Coincidence for periods 1-15, multiplied by 1000.
 MKA is the maximum kappa value for periods 1-15 , multiplied by 1000.
 DIC is the Digraphic Index of Coincidence, multiplied by 10,000.
 EDI is the Digraphic IC for even-numbered pairs, multiplied by 10,000.
 LR is the square root of the percentage of 3 symbol repeats, multiplied by 1000.
 ROD is the percentage of odd-spaced repeats to all repeats.LDI is AAHJU’s average log-digraph score.
 SDD is the average Single letter-Digraph Discrepancy score.

-}


-- Measures the difference of the correlation from 65 the expected correlation for English
-- The smaller the better. This is usually for cipher text and the frequencies of cipher
-- letter are compared to frequencies of letters in English
englishness :: String -> Double
englishness pt = (lic-65.0)**2.0
  where
    lic = incidenceOfCoincidence $ countChars pt


normalisedIncidenceOfCoincidence::Map Char Int -> Double
normalisedIncidenceOfCoincidence fs = fromIntegral nAlphabet * incidenceOfCoincidence fs


-- This is the IC from https://bionsgadgets.appspot.com/gadget_forms/acarefstats.html
-- it's expected to be 63 +- 5 for plain text (ie. 1.73/26*1000)
-- IC = SUM(freq(x)*(freq(x)-1)) / (L*(L-1))
incidenceOfCoincidence::Map Char Int -> Double
incidenceOfCoincidence fs =  M.foldl (\a n -> fromIntegral (n*(n-1))+a) 0.0 fs / fromIntegral (totN * (totN - 1))
  where
    totN = M.foldl' (+) 0 fs


ic :: String -> Double
ic pt = 1000.0 * incidenceOfCoincidence (countChars pt)

-- works out the average ic over the pieces of a string split into n pieces
-- So, the chance that the pieces could each be english
splitIC::Int->String->Double
splitIC n txt =sum (fmap (incidenceOfCoincidence . countChars) spl) / fromIntegral n * 1000.0
        where
            spl = splitText n txt


mic :: String -> Double
mic ct = maximum sics
  where
    sics :: [Double]
    sics = fmap (`splitIC` ct) [1..15]


kappa :: Int -> String -> Double
kappa ix ct = L.foldl (\acc (a, b) -> if a == b then acc + 1 else acc) 0.0 (zip ct shifted) / n
  where
    shifted :: String
    shifted = drop ix ct ++ take ix ct
    n :: Double
    n = fromIntegral $ length ct


mka :: String -> Double
mka ct = 1000 * maximum kas
  where
    kas :: [Double]
    kas = fmap (`kappa` ct) [1..15]

dic :: String -> Double
dic ct = 10000 * M.foldl (\a n -> fromIntegral (n*(n-1))+a) 0.0 bs / fromIntegral (totN * (totN - 1))
  where
    bs :: Map Bigram Int
    bs = countBigrams 1 ct
    totN = M.foldl' (+) 0 bs

edi :: String -> Double
edi ct = 10000 * M.foldl (\a n -> fromIntegral (n*(n-1))+a) 0.0 bs / fromIntegral (totN * (totN - 1))
  where
    bs :: Map Bigram Int
    bs = countEvenBigrams ct
    totN = M.foldl' (+) 0 bs

lr :: String -> (Double, Double)
lr ct = (1000.0 / fromIntegral n * sqrt r3, 100.0 * so / sa)
  where
    n :: Int
    n = length ct
    -- The list is [sum_all, sum_odd, R3]
    ixs = [(ix, jx) | ix<-[0..(n-2)], jx<-[ix+1..(n-1)]]
    (sa, so, r3) = L.foldl go (0.0,0.0,0.0) ixs

    go :: (Double, Double, Double) -> (Int, Int) -> (Double, Double, Double)
    go (sumAll, sumOdd, r3) (ix, jx) = ( sumAll + if nc > 1 then 1 else 0
                                       , sumOdd + if (nc > 1) && odd (ix - jx) then 1 else 0
                                       , r3 + if nc==3 then 1 else 0
                                       )
      where
        nc = countLength ix jx

    countLength :: Int -> Int -> Int
    countLength ix jx = if ct!!ix == ct!!jx then 1 + next else 0
      where
        next :: Int
        next = if jx >= n-1 then 0 else countLength (ix+1) (jx+1)

--    Initialize variables R3, sum_all, and sum_odd to zero.
--      For each character c in the ciphertext:
--      For each character d to the right of c in the ciphertext:
--        Let n be the number of characters for which strings starting at c and d are identical.
--        If n is more than one, increment sum_all.
--        If c and d are also an odd number of characters apart, increment sum_odd
--        If n is equal to 3, increment R3
--    ROD is equal to 100 * sum_odd / sum_all
--    LR is equal to 1000 * (square root of R3) / length of ciphertext


logdi :: [[Int]]
logdi =[ [4,7,8,7,4,6,7,5,7,3,6,8,7,9,3,7,3,9,8,9,6,7,6,5,7,4],
         [7,4,2,0,8,1,1,1,6,3,0,7,2,1,7,1,0,6,5,3,7,1,2,0,6,0],
         [8,2,5,2,7,3,2,8,7,2,7,6,2,1,8,2,2,6,4,7,6,1,3,0,4,0],
         [7,6,5,6,8,6,5,5,8,4,3,6,6,5,7,5,3,6,7,7,6,5,6,0,6,2],
         [9,7,8,8,8,7,6,6,7,4,5,8,7,9,7,7,5,9,9,8,5,7,7,6,7,3],
         [7,4,5,3,7,6,4,4,7,2,2,6,5,3,8,4,0,7,5,7,6,2,4,0,5,0],
         [7,5,5,4,7,5,5,7,7,3,2,6,5,5,7,5,2,7,6,6,6,3,5,0,5,1],
         [8,5,4,4,9,4,3,4,8,3,1,5,5,4,8,4,2,6,5,7,6,2,5,0,5,0],
         [7,5,8,7,7,7,7,4,4,2,5,8,7,9,7,6,4,7,8,8,4,7,3,5,0,5],
         [5,0,0,0,4,0,0,0,3,0,0,0,0,0,5,0,0,0,0,0,6,0,0,0,0,0],
         [5,4,3,2,7,4,2,4,6,2,2,4,3,6,5,3,1,3,6,5,3,0,4,0,5,0],
         [8,5,5,7,8,5,4,4,8,2,5,8,5,4,8,5,2,4,6,6,6,5,5,0,7,1],
         [8,6,4,3,8,4,2,4,7,1,0,4,6,4,7,6,1,3,6,5,6,1,4,0,6,0],
         [8,6,7,8,8,6,9,6,8,4,6,6,5,6,8,5,3,5,8,9,6,5,6,3,6,2],
         [6,6,7,7,6,8,6,6,6,3,6,7,8,9,7,7,3,9,7,8,9,6,8,4,5,3],
         [7,3,3,3,7,3,2,6,7,2,1,7,3,2,7,6,0,7,6,6,6,0,3,0,4,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0],
         [8,6,6,7,9,6,6,5,8,3,6,6,6,6,8,6,3,6,8,8,6,5,6,0,7,1],
         [8,6,7,6,8,6,5,7,8,4,6,6,6,6,8,7,4,5,8,9,7,4,7,0,6,2],
         [8,6,6,5,8,6,5,9,8,3,3,6,6,5,9,6,2,7,8,8,7,4,7,0,7,2],
         [6,6,7,6,6,4,6,4,6,2,3,7,7,8,5,6,0,8,8,8,3,3,4,3,4,3],
         [6,1,0,0,8,0,0,0,7,0,0,0,0,0,5,0,0,0,1,0,2,1,0,0,3,0],
         [7,3,3,4,7,3,2,8,7,2,2,4,4,6,7,3,0,5,5,5,2,1,4,0,3,1],
         [4,1,4,2,4,2,0,3,5,1,0,1,1,0,3,5,0,1,2,5,2,0,2,2,3,0],
         [6,6,6,6,6,6,5,5,6,3,3,5,6,5,8,6,3,5,7,6,4,3,6,2,4,2],
         [4,0,0,0,5,0,0,0,3,0,0,2,0,0,3,0,0,0,1,0,2,0,0,0,4,4]
       ]


sddDat :: [[Int]]
sddDat = [
           [0,3,4,2,0,0,1,0,0,0,4,5,2,6,0,2,0,4,4,3,0,6,0,0,3,5],
           [0,0,0,0,6,0,0,0,0,9,0,7,0,0,0,0,0,0,0,0,7,0,0,0,7,0],
           [3,0,0,0,2,0,0,6,0,0,8,0,0,0,6,0,5,0,0,0,3,0,0,0,0,0],
           [1,6,0,0,1,0,0,0,4,4,0,0,0,0,0,0,0,0,0,1,0,0,4,0,1,0],
           [0,0,4,5,0,0,0,0,0,3,0,0,3,2,0,3,6,5,4,0,0,4,3,8,0,0],
           [3,0,0,0,0,5,0,0,2,1,0,0,0,0,5,0,0,2,0,4,1,0,0,0,0,0],
           [2,0,0,0,1,0,0,6,1,0,0,0,0,0,2,0,0,1,0,0,2,0,0,0,0,0],
           [5,0,0,0,7,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
           [0,0,5,0,0,0,4,0,0,0,1,1,3,7,0,0,0,0,5,3,0,5,0,0,0,8],
           [0,0,0,0,6,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,9,0,0,0,0,0],
           [0,0,0,0,6,0,0,0,5,0,0,0,0,4,0,0,0,0,0,0,0,0,1,0,0,0],
           [2,0,0,4,2,0,0,0,3,0,0,7,0,0,0,0,0,0,0,0,0,0,0,0,7,0],
           [5,5,0,0,5,0,0,0,2,0,0,0,0,0,2,6,0,0,0,0,2,0,0,0,6,0],
           [0,0,4,7,0,0,8,0,0,2,2,0,0,0,0,0,3,0,0,4,0,0,0,0,0,0],
           [0,2,0,0,0,8,0,0,0,0,4,0,5,5,0,2,0,4,0,0,7,4,5,0,0,0],
           [3,0,0,0,3,0,0,0,0,0,0,5,0,0,5,7,0,6,0,0,3,0,0,0,0,0],
           [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,0,0,0,0,0],
           [1,0,0,0,4,0,0,0,2,0,4,0,0,0,2,0,0,0,0,0,0,0,0,0,5,0],
           [1,1,0,0,0,0,0,1,2,0,0,0,0,0,1,4,4,0,1,4,2,0,4,0,0,0],
           [0,0,0,0,0,0,0,8,3,0,0,0,0,0,3,0,0,0,0,0,0,0,2,0,0,0],
           [0,4,3,0,0,0,5,0,0,0,0,6,2,3,0,6,0,6,5,3,0,0,0,0,0,6],
           [0,0,0,0,8,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
           [6,0,0,0,2,0,0,6,6,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0],
           [3,0,7,0,1,0,0,0,2,0,0,0,0,0,0,9,0,0,0,5,0,0,0,6,0,0],
           [1,6,2,0,0,2,0,0,0,6,0,0,2,0,6,2,1,0,2,1,0,0,6,0,0,0],
           [2,0,0,0,8,0,0,0,0,6,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,9]
        ]




ldi :: String -> Double
ldi ct = fromIntegral score * 100.0 / fromIntegral n
  where
    n = length ct
    score = L.foldl (\acc ix -> acc + (logdi!!nord (ct!!ix))!!nord (ct!!(ix+1))) 0 [0..(n-2)]


sdd :: String -> Double
sdd ct = fromIntegral score * 100.0 / fromIntegral n
  where
    n = length ct
    score = L.foldl (\acc ix -> acc + (sddDat!!nord (ct!!ix))!!nord (ct!!(ix+1))) 0 [0..(n-2)]


{-
function get_sdd(dat) {
	var score,i,l;

	l=dat.length-1;
	score = 0;
	for (i=0;i<l;i++) {
		if (dat[i]>25 || dat[i+1]>25)
			continue;
		score += sdd[dat[i]][dat[i+1]]
	}
	score *= 100;
	score /= l;
	return score
}
-}
