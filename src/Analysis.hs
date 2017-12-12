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
    , ixOfMin
    , ixOfMax
    , loseZeros
    , splitText
    , var
    , mean
    , corr
    , freqDist
    , isUpperChar
    , countBigrams
    , countEvenBigrams
    , countTrigrams
    , countQuadgrams
    , chunksOf
    , chunkUsing
    , eqBigramCount
    , Bigram(..)
    , Trigram(..)
    , Quadgram(..)
    , charFreqs
    , wordsOn
    , quadgramScore
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
import Data.ByteString.Lazy.Char8 (pack)

import Utils
import Mod
import Quadgram (qscore)


quadgramScore :: String -> Double
quadgramScore pt = qscore $ pack pt



wordsOn :: (Char -> Bool) -> String -> [String]
wordsOn p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsOn p s''
                            where (w, s'') = break p s'


type Bigram = (Char, Char)


type Trigram = (Char, Char, Char)


type Quadgram = (Char, Char, Char, Char)


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


zeroQuadgramCount :: Map Quadgram Int
zeroQuadgramCount = fromList [((nchr w, nchr x, nchr y, nchr z), 0)| w<-[1..25], x<-[1..25], y<-[1..25], z<-[1..25]]


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
countEvenBigrams::String->Map Bigram Int
countEvenBigrams txt = L.foldl' mInsert zeroBigramCount $ evenBigrams txt


-- counts the occurences of each bigram in a string
-- where n is the distance between the letters
countTrigrams::Int->String->Map Trigram Int
countTrigrams n txt = L.foldl' mInsert zeroTrigramCount $ trigrams n txt


countQuadgrams::Int->String->Map Quadgram Int
countQuadgrams n txt = L.foldl' mInsert zeroQuadgramCount $ quadgrams n txt


eqBigramCount::Map Bigram Int->Int
eqBigramCount = M.foldrWithKey (\k x acc -> acc + (if fst k == snd k then x else 0)) 0


-- Here the p is the distance between the letters of the bigram
bigrams::Int->String->[Bigram]
bigrams p txt = [(txt!!ix, txt!!(ix+p)) | ix<-[0..(n-p-1)]]
    where
        n = length txt


-- Here the p is the distance between the letters of the bigram
evenBigrams::String->[Bigram]
evenBigrams txt = [(txt!!ix, txt!!(ix+1)) | ix<-[0, 2..lst]]
    where
        n = length txt
        lst = if even n then n - 2 else n - 3




-- Here the p is the number of letters in the ngram - which must be consecutive...
trigrams::Int->String->[Trigram]
trigrams p txt = [(txt!!ix, txt!!(ix+p), txt!!(ix+p+p)) | ix<-[0..(n-p-p-1)]]
    where
        n = length txt


-- Here the p is the number of letters in the ngram - which must be consecutive...
quadgrams::Int->String->[Quadgram]
quadgrams p txt = [(txt!!ix, txt!!(ix+p), txt!!(ix+p+p), txt!!(ix+p+p+p)) | ix<-[0..(n-p-p-p-1)]]
    where
        n = length txt


count2freq::Map a Int->Map a Double
count2freq c = M.map (\v-> fromIntegral v / fromIntegral tot ) c
        where
            tot = M.foldl (+) 0 c


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

firstLeterOfWordFreq = [
   ('a',  0.11682)
  ,('b', 0.04434)
  ,('c', 0.05238)
  ,('d', 0.03174)
  ,('e', 0.02799)
  ,('g', 0.01642)
  ,('f', 0.04027)
  ,('h', 0.04200)
  ,('i', 0.07294)
  ,('j', 0.00511)
  ,('k', 0.00456)
  ,('l', 0.02415)
  ,('n', 0.02284)
  ,('m', 0.03826)
  ,('o', 0.07631)
  ,('p', 0.04319)
  ,('q', 0.00222)
  ,('r', 0.02826)
  ,('s', 0.06686)
  ,('t', 0.15978)
  ,('u', 0.01183)
  ,('v', 0.00824)
  ,('w', 0.05497)
  ,('x', 0.00045)
  ,('y', 0.00763)
  ,('z', 0.00045)]


wordLengthDist = [
  ( 1, 0.02998)
  , ( 2, 0.17651)
  , ( 3, 0.20511)
  , ( 4, 0.14787)
  , ( 5, 0.10700)
  , ( 6, 0.08388)
  , ( 7, 0.07939)
  , ( 8, 0.05943)
  , ( 9, 0.04437)
  , (10, 0.03076)
  , (11, 0.01761)
  , (12, 0.00958)
  , (13, 0.00518)
  , (14, 0.00222)
  , (15, 0.00076)
  , (16, 0.00020)
  , (17, 0.00010)
  , (18, 0.00004)
  , (19, 0.00001)
  , (20, 0.00001)]

wordFreq = [
  ("the",    0.0714)
  , ("of",   0.0416)
  , ("and",  0.0304)
  , ("to",   0.0260)
  , ("in",   0.0227)
  , ("a",    0.0206)
  , ("is",   0.0113)
  , ("that", 0.0108)
  , ("it",   0.0077)
  , ("for",  0.0088)
  , ("as",   0.0077)
  , ("with", 0.0070)
  , ("was",  0.0074)
  , ("be",   0.0065)
  , ("by",   0.0063)
  , ("on",   0.0062)
  , ("not",  0.0061)
  , ("he",   0.0055)
  , ("this", 0.0051)
  , ("i",    0.0052)
  , ("are",  0.0050)
  , ("or",   0.0049)
  , ("his",  0.0049)
  , ("from", 0.0047)
  , ("at",   0.0046)
  , ("which",0.0042)
  , ("but",  0.0038)
  , ("an",   0.0037)
  , ("have", 0.0037)
  , ("had",  0.0035)
  , ("they", 0.0033)
  , ("you",  0.0031)
  , ("were", 0.0031)
  , ("their",0.0029)
  , ("one",  0.0029)
  , ("all",  0.0028)
  , ("we",   0.0028)
  , ("can",  0.0022)
  , ("her",  0.0022)
  , ("has",  0.0022)
  , ("there",0.0022)
  , ("been", 0.0022)
  , ("if",   0.0021)
  , ("more", 0.0021)
  , ("when", 0.0020)
  , ("will", 0.0020)
  , ("would",0.0020)
  , ("who",  0.0020)
  , ("so",   0.0019)
  , ("no",   0.0019)]
