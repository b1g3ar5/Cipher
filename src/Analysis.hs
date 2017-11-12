{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Analysis
    (
    cShift
    , nchr
    , nord
    , nAlphabet
    , alphabet
    , clean
    , count2freq
    , countChars
    , splitIC
    , ixOfMin
    , ixOfMax
    , loseZeros
    , splitText
    , incidenceOfCoincidence
    , normalisedIncidenceOfCoincidence
    , var
    , mean
    , corr
    , freqDist
    , isUpperChar
    , countBigrams
    , countTrigrams
    , chunksOf
    , chunkUsing
    , eqBigramCount
    , Trigram
    , charFreqs
    , englishness
    , wordsOn
    ) where

import Prelude hiding (filter)
import System.IO
import Control.Monad
import Data.Array as A
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Map as M hiding (foldr)
import Data.List as L hiding (foldr, filter)
import Text.Printf
import Data.Tuple (swap)
import Data.String.Utils
import Utils
import Mod
import Quadgram (qscore)


wordsOn :: (Char -> Bool) -> String -> [String]
wordsOn p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsOn p s''
                            where (w, s'') = break p s'


-- Measures the difference of the correlation from 65 the expected correlation for English
-- The smaller the better. This is usually for cipher text and the frequencies of cipher
-- letter are compared to frequencies of letters in English
englishness :: String -> Double
englishness pt = (lic-65.0)**2.0
  where
    lic = incidenceOfCoincidence $ countChars pt


type Bigram = (Char, Char)

type Trigram = (Char, Char, Char)


--type Ngram = String


loseZeros::Map a Int -> Map a Int
loseZeros = M.filter (>0)


var :: Map k Int -> Double
var xs = fromIntegral ssq / fromIntegral n - (fromIntegral s / fromIntegral n)**2.0
    where
        (n, (s, ssq)) = M.foldl' (\acc x -> ( 1 + fst acc, (x + fst (snd acc), x*x + snd (snd acc)))) (0, (0, 0)) xs


mean :: Map k Int -> Double
mean xs = fromIntegral s / fromIntegral n
    where
        (n, s) = M.foldl' (\acc x -> (fst acc + 1, x + snd acc)) (0, 0) xs


zeroCount::Map Char Int
zeroCount = fromList $ L.map (\x->(nchr x,0)) [0..25]


zeroBigramCount::Map Bigram Int
zeroBigramCount = fromList [((nchr x, nchr y), 0)| x<-[1..25], y<-[1..25]]

zeroTrigramCount :: Map Trigram Int
zeroTrigramCount = fromList [((nchr x, nchr y, nchr z), 0)| x<-[1..25], y<-[1..25], z<-[1..25]]

mInsert :: (Ord b) => Map b Int -> b -> Map b Int
mInsert xs x = insertWith (+) x 1 xs


countChars::(Ord a) =>[a]->Map a Int
countChars = L.foldl' mInsert empty

-- counts the occurences of each bigram in a string
-- where n is the distance between the letters
countBigrams::Int->String->Map Bigram Int
countBigrams n txt = L.foldl' mInsert zeroBigramCount $ bigrams n txt


-- counts the occurences of each bigram in a string
-- where n is the distance between the letters
countTrigrams::Int->String->Map Trigram Int
countTrigrams n txt = L.foldl' mInsert zeroTrigramCount $ trigrams n txt


eqBigramCount::Map Bigram Int->Int
eqBigramCount = M.foldrWithKey (\k x acc -> acc + (if fst k == snd k then x else 0)) 0


-- Here the p is the distance between the letters of the bigram
bigrams::Int->String->[Bigram]
bigrams p txt = [(txt!!ix, txt!!(ix+p)) | ix<-[0..(n-p-1)]]
    where
        n = length txt

-- Here the p is the number of letters in the ngram - which must be consecutive...
trigrams::Int->String->[Trigram]
trigrams p txt = [(txt!!ix, txt!!(ix+p), txt!!(ix+p+p)) | ix<-[0..(n-p-p-1)]]
    where
        n = length txt


count2freq::Map a Int->Map a Double
count2freq c = M.map (\v-> fromIntegral v / fromIntegral tot ) c
        where
            tot = M.foldl (+) 0 c


normalisedIncidenceOfCoincidence::Map Char Int -> Double
normalisedIncidenceOfCoincidence fs = (fromIntegral nAlphabet) * incidenceOfCoincidence fs

-- This is the IC from https://bionsgadgets.appspot.com/gadget_forms/acarefstats.html
-- it's expected to be 63 +- 5 for plain text (ie. 1.73/26*1000)
-- IC = SUM(freq(x)*(freq(x)-1)) / (L*(L-1))
incidenceOfCoincidence::Map Char Int -> Double
incidenceOfCoincidence fs =  M.foldl (\a n -> fromIntegral (n*(n-1))+a) 0.0 fs / fromIntegral (totN * (totN - 1))
  where
    totN = M.foldl' (+) 0 fs


-- works out the average ic over the pieces of a string split into n pieces
-- So, the chance that the pieces could each be english
splitIC::Int->String->Double
splitIC n txt =sum (L.map (incidenceOfCoincidence . countChars) spl) / fromIntegral n * 1000.0
        where
            spl = splitText n txt


-- Works out the correlation of an offset of n of a string with standard
-- english text frequencies and compares with the correlation of the frequencies themselves
-- Close to zero is good.
corr::String->Double
corr txt = sum $ zipWith (\a b -> (snd a - snd b)**2 / snd b) cntTxt letterFreq
    where
        fs :: Map Char Double
        fs = count2freq $ countChars txt
        cntTxt = toAscList fs


freqDist :: String -> Double
freqDist ct = dist freqs $ fromList letterFreq
  where
    freqs = count2freq $ countChars ct


dist :: Map Char Double -> Map Char Double -> Double
dist m1 m2 = sqrt $ sum dists
  where
    dists = mapWithKey (\k x -> (x - m2 M.! k)**2) m1


rms_letterFreq = sqrt $ sum $ fmap (\x -> x*x) [0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150, 0.01974, 0.00074]


-- These are the frequencies of English text by letter
letterFreq::[(Char, Double)]
letterFreq = [ ('A', 0.08167)
        ,('B',0.01492)
        ,('C',0.02782)
        ,('D',0.04253)
        ,('E',0.12702)
        ,('F',0.02228)
        ,('G',0.02015)
        ,('H',0.06094)
        ,('I',0.06966)
        ,('J',0.00153)
        ,('K',0.00772)
        ,('L',0.04025)
        ,('M',0.02406)
        ,('N',0.06749)
        ,('O',0.07507)
        ,('P',0.01929)
        ,('Q',0.00095)
        ,('R',0.05987)
        ,('S',0.06327)
        ,('T',0.09056)
        ,('U',0.02758)
        ,('V',0.00978)
        ,('W',0.02360)
        ,('X',0.00150)
        ,('Y',0.01974)
        ,('Z',0.00074)]

bigramFreq = [("TH",0.03880),
    ("HE",0.03680),
    ("IN",0.02280),
    ("ER",0.02180),
    ("AN",0.02140),
    ("RE",0.01750),
    ("ND",0.01570),
    ("AT",0.01420),
    ("ON",0.01380),
    ("NT",0.01340),
    ("HA",0.01290),
    ("ES",0.01280),
    ("ST",0.01270),
    ("EN",0.01170),
    ("ED",0.01150),
    ("TO",0.01135),
    ("IT",0.01110),
    ("OU",0.01090),
    ("EA",0.01090),
    ("HI",0.01050)]

trigramFreq = [("THE",0.03508232)
    , ("AND",0.01593878)
    , ("ING",0.01147042)
    , ("HER",0.00822444)
    , ("HAT",0.00650715)
    , ("HIS",0.00596748)
    , ("THA",0.00593593)
    , ("ERE",0.00560594)
    , ("FOR",0.00555372)
    , ("ENT",0.00530771)
    , ("ION",0.00506454)
    , ("TER",0.00461099)
    , ("WAS",0.00460487)
    , ("YOU",0.00437213)
    , ("ITH",0.00431250)
    , ("VER",0.00430732)
    , ("ALL",0.00422758)
    , ("WIT",0.00397290)
    , ("THI",0.00394796)
    , ("TIO",0.00378058)]


quadrigramFreq = [("THAT",0.00761242)
    ,("THER",0.00604501)
    ,("WITH",0.00573866)
    ,("TION",0.00551919)
    ,("HERE",0.00374549)
    ,("OULD",0.00369920)
    ,("IGHT",0.00309440)
    ,("HAVE",0.00290544)
    ,("HICH",0.00284292)
    ,("WHIC",0.00283826)
    ,("THIS",0.00276333)
    ,("THIN",0.00270413)
    ,("THEY",0.00262421)
    ,("ATIO",0.00262386)
    ,("EVER",0.00260695)
    ,("FROM",0.00258580)
    ,("OUGH",0.00253447)
    ,("WERE",0.00231089)
    ,("HING",0.00229944)
    ,("MENT",0.00223347)]
