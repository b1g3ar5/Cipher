module Analysis
    (
	cShift
	, nchr
	, nord
	, nAlphabet
	, alphabet
	, clean
	, count2freq
	, bcount2freq
	, txt2count
	, splitIC
	, ixOfMin
	, ixOfMax
	, loseZeros
	, splitText
    , ic
	, var
	, mean
	, num
	, corr
	, isUpperChar
	, affineShift
	, affineDeshift
	, txtBigramCount
	, modinv
	, chunksOf
    , chunkUsing
	, eqBigramCount
	, ICount(..)
	, DCount(..)
	, BICount(..)
	, BDCount(..)
    ) where

import System.IO
import Control.Monad
import Data.Array as A
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Map as M
import Data.List as L
import Text.Printf
import Data.Tuple (swap)
import Data.String.Utils
import Number.ResidueClass
import Mod

type Bigram = (Char, Char)

eqbg::Bigram->Bool
eqbg (x,y) = (x==y)

-- Counts of the letters in a text
newtype ICount = ICount {itoMap::Map Char Int} deriving (Show)
newtype DCount = DCount {dtoMap::Map Char Double} deriving (Show)

-- Counts of bigrams in a text
newtype BICount = BICount {bitoMap::Map Bigram Int} deriving (Show)
newtype BDCount = BDCount {bdtoMap::Map Bigram Double} deriving (Show)

loseZeros::BICount->BICount
loseZeros (BICount m) = BICount $ M.filter (\x-> x>0) m

var::BICount->Double
var (BICount m) = (fromIntegral ssq)/n - ((fromIntegral s)/n)**2.0
    where
        (s, ssq) = M.foldr (\x acc -> ( x + fst acc, x*x + snd acc)) (0, 0) m
        n = fromIntegral $ M.size m

mean::BICount->Double
mean (BICount m) = (fromIntegral s)/n
    where
        s = M.foldr (\x acc -> x + acc) 0 m
        n = fromIntegral $ M.size m

num::BICount->Int
num (BICount m) = s
    where
        s = M.foldr (\x acc -> x + acc) 0 m

zeroCount::ICount
zeroCount = ICount $ fromList $ L.map (\x->(nchr x,0)) [0..25]

zeroBigramCount::BICount
zeroBigramCount = BICount $ fromList $ [((nchr x, nchr y), 0)| x<-[1..25], y<-[1..25]] 

-- A monoid instance where zero is alphabet with 0 frequencies
instance Monoid ICount where
    mempty = zeroCount
    mappend (ICount c) (ICount d) = ICount $ M.union c d

-- A monoid instance where zero is alphabet with 0 frequencies
instance Monoid BICount where
    mempty = zeroBigramCount
    mappend (BICount c) (BICount d) = BICount $ M.union c d

-- insert a character into a map of characters
-- Increment the count by 1
cInsert::ICount->Char->ICount
cInsert (ICount m) c = ICount $ insertWith (+) c 1 m

-- insert a bigram into a map of bigrams
-- Increment the count by 1
bigramInsert::BICount->Bigram->BICount
bigramInsert (BICount m) bg = BICount $ insertWith (+) bg 1 m

-- counts the occurences of each character in a string
txt2count::String->ICount
txt2count txt = L.foldl cInsert zeroCount txt

-- counts the occurences of each bigram in a string
-- where n is the distance between the letters
txtBigramCount::Int->String->BICount
txtBigramCount n txt = L.foldl bigramInsert zeroBigramCount $ bigrams n txt

eqBigramCount::BICount->Int
eqBigramCount (BICount m) = M.foldrWithKey (\k x acc -> acc + (if fst k == snd k then x else 0)) 0 m


bigrams::Int->String->[Bigram]
bigrams p txt = [(txt!!ix, txt!!(ix+p)) | ix<-[0..(n-p-1)]]
    where
        n = length txt
        

-- converts the count into a frequency
count2freq::ICount->DCount
count2freq (ICount c) = DCount $ M.map (\v-> fromIntegral v / fromIntegral tot ) c
        where
            tot = M.foldl (+) 0 c

-- converts the count into a frequency
bcount2freq::BICount->BDCount
bcount2freq (BICount c) = BDCount $ M.map (\v-> fromIntegral v / fromIntegral tot ) c
        where
            tot = M.foldl (+) 0 c

-- Works out the incidence of coincidence
-- IC = sum{ f_i*(f_i-1)}/N/(N-1)*n
-- where N = length of the text (= sum of the f_i)
-- n = the size of the alphabet
-- f_i = the frequency of letter i
ic::ICount->Double
ic (ICount fs) = (M.foldl (\a f -> (fromIntegral $ f*(f-1))+a) (0.0) fs) / (fromIntegral $ tot * (tot - 1)) * fromIntegral nAlphabet
        where
            tot = M.foldl (+) 0 fs


-- Cleans cipher text of everything except alphabetic characters
clean::(Char->Bool)->String->String
clean _ [] = []
clean f (x:xs) = case f x of
			True -> x:(clean f xs)
			False-> clean f xs

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

-- Shifts right according to an alphabet with nAlphabet Chars
cShift::Char->Char->Char
cShift k c = nchr $ mod (nord k + nord c) nAlphabet

  
-- Switches a->z, b->y etc (when n=26)
reverseTxt::String->String       
-- reverseTxt txt = L.map (\c-> chr $ nAlphabet - ord c + 65) txt     
reverseTxt txt = L.map (\c-> chr $ nAlphabet - 1 + 2*65 - ord c) txt     
            
-- works out the ic of a string split into n pieces (ie. autocorrelation)
splitIC::Int->String->Double
splitIC n txt = (sum $ L.map (ic.txt2count) spl)/(fromIntegral n)
        where
            spl = splitText n txt
            
chunksOf::Int->[a]->[[a]]
chunksOf n [] = []
chunksOf n xs = (take n xs):(chunksOf n $ drop n xs)
     
chunkUsing::[Int]->[a]->[[a]]
chunkUsing [] xs = [xs]
chunkUsing (n:ns) xs = [take n xs] ++ (chunkUsing ns $ drop n xs)
       
-- split a list into n bits alternating which bit to put the next item in
splitText::Int->[a]->[[a]]
splitText _ [] = []
splitText 1 xs = [xs]
splitText n xs = A.elems $ split' 0 n init xs
            where
                init::Array Int [a]
                init = listArray (0,n-1) $ replicate n ([])

split'::Int->Int->Array Int [a]->[a]->Array Int [a]
split' i n acc [] = acc
split' i n acc (x:xs) = split' (mod (i+1) n) n (accum (++) acc [(i, [x])]) xs

-- Index of the minimum starting with 0
ixOfMin::Ord a=>[a]->Int
ixOfMin xs = fromJust $ elemIndex (minimum xs) xs

-- Index of the minimum starting with 0
ixOfMax::Ord a=>[a]->Int
ixOfMax xs = fromJust $ elemIndex (maximum xs) xs

-- Shift a character by an int
cshift::Char->Char->Char
cshift k ' ' = ' '
cshift k c = chr $ (ord 'A') + (mod (ord k + ord c - ord 'A' - ord 'A') nAlphabet)

-- Shift a char x to a*x+b mod 26
affineShift::Int->Int->Char->Char
affineShift a b x = nchr $ mod ((a*nord x) + b) nAlphabet

-- Shift a char x to a*x+b mod 26
affineDeshift::Int->Int->Char->Char
affineDeshift a b x = nchr $ mod (inva*(nord x - b)) nAlphabet
	where
		inva = head $ modinv nAlphabet a

-- Works out the correlation of an offset of n of a string with standard
-- english text frequencies
corr::Char->String->Double
corr c txt = sum $ zipWith (\a b -> (snd a)*(snd b)) cntTxt letterFreq
    where
        shiftedTxt = L.map (cshift c) txt
        fs = count2freq $ txt2count shiftedTxt
        al (DCount m) = toAscList m
        cntTxt = al fs
            

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



