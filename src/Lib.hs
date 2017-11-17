module Lib
    ( someFunc
    ) where

import System.IO
import GHC.Exts hiding (toList)
import Control.Monad
import Data.Char
import Data.Int
import Data.Maybe
import Data.Map as M
import Data.List as L
import Data.Monoid
import Text.Printf
import Data.Tuple (swap)
import Data.Set as S (size)
import qualified Data.ByteString.Lazy as BL  hiding (split)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base32.Hex as B32
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Internal as BSI (c2w, w2c)

import Analysis
import Cribs
import Cipher
import Bifid
import CTexts

import Solve2013
import Solve2014
import Solve2015
import Solve2016
import Solve2017
import Test


someFunc :: IO ()
someFunc = do
  test
  --main_2017
  --putStrLn "someFunc"


-- shows n decimal places for a double
dshow::Int->Double->String
dshow n = printf ("%."++ show n ++"f")

-- Switches a->z, b->y etc (when n=26)
--reverseTxt::String->String
--reverseTxt txt = L.map (\c-> chr $ nAlphabet - 1 + 2*65 - ord c) txt

-- just split into bits of size n
mySplit n = unfoldr $ \xs -> case xs of
     [] -> Nothing
     _  -> Just (splitAt n xs)

fromCols::[[a]]->[a]
fromCols = fromCols' []

fromCols'::[a]->[[a]]->[a]
fromCols' acc [] = acc
fromCols' acc xss = fromCols' (acc ++ L.map head xss) (L.map tail xss)

go nChunks isTail ct crib = L.map (  L.filter (\t->snd t==crib)
                                    .zip [0..]
                                    .chunksOf 2
                                    .(if isTail then tail else id)
                                  )  $ chunksOf (length ct `div` nChunks) ct

get nChunks isTail ct col offSet = concat $ drop offSet $ chunksOf 2 $ (if isTail then tail else id) $ chunksOf (length ct `div` nChunks) ct !! col

ccrib ct isTail i = L.filter (not . L.null . L.map snd . snd) $ zip [0..] $ Lib.go 5 isTail ct $ L.map (!! i) ["PHASE","EIGHT"]


ts ct = zip (L.map (ccrib ct True) [0..4]) $ repeat True
fs ct = zip (L.map (ccrib ct False) [0..4]) $ repeat False


ftable = [[(4, 45, False)], [(3, 14, False)], [(0, 118, False),(3, 15, False),(3, 52, False)], [(0, 108, False)], [(0,39, False),(1,19,False),(1,50,False),(2,36,False),(2,128,False),(3,43,False)]]

ttable = [[],[(1,30,True),(2,33,True),(2,49,True),(2,97,True)],[(1,121,True),(2,14,True)],[(0,12,True),(4,116,True)],[(0,8,True),(1,73,True),(1,132,True),(2,81,True),(2,93,True)]]

alltable = L.zipWith (++) ftable ttable

try1 ct alltable coords= L.transpose $ L.zipWith (flip (!!)) coords $ L.map (L.map (\(c,o,b)-> get 5 b ct c o)) alltable

allCoords = L.map (\x-> [0,0,0,1,x]) [0..10]

tries ct = L.map (try1 ct alltable) allCoords


-- Turns a string 0f 0's ans 1's into an integer
bstringTo32 :: BL.ByteString -> Int
bstringTo32 = go 0
    where
        go n bs
              | BL.null bs = n
              | BL.length bs > 1 =
                go (2 * n + if BL.head bs == BSI.c2w '1' then 1 else 0) $
                  BL.tail bs
              | otherwise = 2 * n + if BL.head bs == BSI.c2w '1' then 1 else 0


bchunksOf::Int64 -> BL.ByteString->[BL.ByteString]
bchunksOf n xs = if BL.null xs then [] else BL.take n xs : bchunksOf n (BL.drop n xs)


testPeriodBifid::IO ()
testPeriodBifid = do
    let inCipherText  = b7_2013
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let bs = "THEORYABCDFGIKLMNPQSUVWXZ"
    let pt = decipher (BifidCipher bs) cipherText
    putStrLn $ "pt = " ++ show pt
    let ct = cipher (BifidPeriodCipher 14 bs) pt
    putStrLn $ "Encoded with PeriodBifid= " ++ show ct
    let bgs = L.map (\i-> (i, var $ loseZeros $ countBigrams i ct)) [1..10]
    putStrLn $ "bigrams = " ++ show bgs

testBifidCrib:: IO ()
testBifidCrib = do
    let inCipherText  = b8_2013
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let n = length cipherText
    --let r1 = applyCrib (Crib (Just 0) (Just $ n `quot` 2) "HERRGOERING") cipherText
    --let r2 = applyCrib (Crib (Just $ (n `quot` 2)-5) (Just $ n-5) "HEILHITLER") cipherText
    let r1 = applyCrib (Crib (Just 0) Nothing "HERRGOERING") cipherText
    let r2 = applyCrib (Crib Nothing (Just $ n-5) "HEILHITLER") cipherText
    let rs = L.reverse $ sortWith S.size $ consolidate $ r1 ++ r2
    putStrLn $ "Rules are: " ++ show rs
    let gs = grids rs
    putStrLn $ "Grids are: " ++ show (take 3 gs)


zeroDic = [ ('A',0),('B',0),('C',0),('D',0),('E',0),('F',0),('G',0),('H',0),('I',0),('J',0),('K',0),('L',0),('M',0),('N',0),('O',0),('P',0),('Q',0),('R',0),('S',0),('T',0),('U',0),('V',0),('W',0),('X',0),('Y',0),('Z',0), (chr 95, 0) ]

trifid = [   ('A',"AAA")
            ,('B',"AAB")
            ,('C',"AAC")
            ,('D',"ABA")
            ,('E',"ABB")
            ,('F',"ABC")
            ,('G',"ACA")
            ,('H',"ACB")
            ,('I',"ACC")
            ,('J',"BAA")
            ,('K',"BAB")
            ,('L',"BAC")
            ,('M',"BBA")
            ,('N',"BBB")
            ,('O',"BBC")
            ,('P',"BCA")
            ,('Q',"BCB")
            ,('R',"BCC")
            ,('S',"CAA")
            ,('T',"CAB")
            ,('U',"CAC")
            ,('V',"CBA")
            ,('W',"CBB")
            ,('X',"CBC")
            ,('Y',"CCA")
            ,('Z',"CCB")
            ,('_',"CCC")
         ]

trifid2 = [   ('A',"aA0")
            ,('B',"bB1")
            ,('C',"cC2")
            ,('D',"dD3")
            ,('E',"eE4")
            ,('F',"fF5")
            ,('G',"gG6")
            ,('H',"hH7")
            ,('I',"iI8")
            ,('J',"jJ9")
            ,('K',"kK[")
            ,('L',"lL]")
            ,('M',"mM{")
            ,('N',"nN}")
            ,('O',"oO:")
            ,('P',"pP;")
            ,('Q',"qQ'")
            ,('R',"Rr@")
            ,('S',"sS~")
            ,('T',"Tt#")
            ,('U',"uU<")
            ,('V',"vV,")
            ,('W',"wW>")
            ,('X',"xX?")
            ,('Y',"yY/")
            ,('Z',"zZ+")
            ,('_',"-_=")
         ]

-- Codes up plain text to 3* length of the trifid cipher
-- Just the fractionation bit
trifidFractionation::String->String
trifidFractionation = concatMap (\x -> fromJust $ M.lookup x $ M.fromList trifid2)

trifidDefractionation::String->String
trifidDefractionation ct = L.map (\x -> fromJust $ M.lookup x $ M.fromList $ L.map swap trifid2) $ mySplit 3 ct


-- This does the columnar transopition
-- Splits into pieces of size n and reads off in column order
-- No transposition yet
trifidTransposition::Int->String->String
trifidTransposition n ft = fromCols $ mySplit n ft

-- Splits the data into n pieces, reads off in piece order
trifidDetransposition::Int->String->String
trifidDetransposition m tt = trifidTransposition n tt
    where
        n = length tt `div` m

trifidCipher::Int->String->String
trifidCipher n pt = trifidDefractionation $ trifidTransposition n $ trifidFractionation pt

trifidDecipher::Int->String->String
trifidDecipher n pt = trifidDefractionation $ trifidDetransposition n $ trifidFractionation pt

-- Detransposes in column order
trifidDetranspose::Int->String->String
trifidDetranspose n ct = undefined
