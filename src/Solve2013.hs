module Solve2013
    (
        main_2013
        , solve3A_2013
        , solve6A_2013
        , solve6B_2013
        , solve7A_2013
        , solve7B_2013
        , solve8A_2013
        , solve8B_2013
    ) where

import System.IO
import GHC.Exts hiding (fromList, toList)
import Control.Monad
import Data.Char (isAlpha, chr, ord)
import Data.Maybe
import Data.Monoid
import Data.Map as M
import Data.List as L
import Text.Printf
import Data.Tuple (swap)
import Data.Set as S (size)

import Analysis
import Cribs
import Cipher
import Vignere
import Bifid
import Tree
import CTexts


-- shows n decimal places for a double
dshow::Int->Double->String
dshow n = printf ("%."++ show n ++"f")

-- Switches a->z, b->y etc (when n=26)
reverseTxt::String->String
reverseTxt = L.map (\c-> chr $ nAlphabet - 1 + 2*65 - ord c)

-- just split into bits of size n
mySplit n = unfoldr $ \xs -> case xs of
     [] -> Nothing
     _  -> Just (splitAt n xs)

fromCols::[[a]]->[a]
fromCols = fromCols' []

fromCols'::[a]->[[a]]->[a]
fromCols' acc ([]:xs) = acc
fromCols' acc xss = fromCols' (acc ++ L.map head xss) $ L.map tail xss

solve3A_2013::IO ()
solve3A_2013 = do
    inCipherText <- readFile "./src/2013/3A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = toDescList $ fromList $ L.map swap $ toList $ count2freq $ countChars cipherText
    --putStrLn $ "Frequencies are: " ++ (show fs)
    --putStrLn $ "length of cipherText = " ++ (show $ length cipherText)
    let ptCrib = "HARRYTHISSTRICTLYEYESONLYWHENIHAVETIMEPHILENOUGHHEADACHE"
    let ctCrib = "LODDARLSKKRDSCRNAQAQKIBNAMLQBSLOFQRSUQPLSNQBIYELLQOJOCLQ"
    let km = cribMap ptCrib ctCrib
    let km1 = mappend km $ cribMap "ET" $ L.map snd $ take 2 fs
    --let km2 = mappend km1 $ cribMap "ILPXCWNSOUGFVMDBKJ" "SNPTCMBKIYEXFUJVGZ"
    let km2 = mappend km1 $ cribMap "ILP" "SNPZ"
    let pt = apply km2 cipherText
    --putStrLn $ "cpherText = " ++ (show $ cipherText)
    putStrLn $ "3A = " ++ show pt
    dd<-getDict
    -- let bs = L.map (\l-> isWord dd $ toWord l ) $ getWords pt [5,4,2,8]
    return ()

solve5B_2013::IO ()
solve5B_2013 = do
    inCipherText <- readFile "./src/2013/5B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = sortBy (flip compare) $ L.map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ show fs
    let pt = decipher (RailRoadCipher 9) cipherText
    -- use get word until true the use it again on the drop etc.
    putStrLn $ "5B = " ++ show pt

solve6A_2013::IO ()
solve6A_2013 = do
    inCipherText <- readFile "./src/2013/6A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = toDescList $ fromList $ L.map swap $ toList $ count2freq $ countChars cipherText
    --putStrLn $ "Frequencies are: " ++ (show fs)
    --putStrLn $ "length of cipherText = " ++ (show $ length cipherText)
    let km = cribMap "" ""
    let km1 = mappend km $ cribMap "ETA" $ L.map snd $ take 3 fs
    let km2 = mappend km1 $ cribMap "HARRYCDSIPNOGFLMKWBUVZJ" "CPYYLRIZOWUVGHDTNJAEFQM"
    let pt = apply km2 cipherText
    putStrLn $ "6A = " ++ show pt
    return ()


cleanup x = clean isAlpha $ concat $ lines x

solve7A_2013::IO ()
solve7A_2013 = do
    let inCipherText = a7_2013
    --let cipherText  = cleanup inCipherText
    --let fs = toDescList $ fromList $ L.map swap $ toList $ count2freq $ countChars cipherText
    let kp = solveVig $ cleanup inCipherText
    putStrLn $ "7A (key, plainText) = " ++ show kp

solve8A_2013::IO ()
solve8A_2013 = do
    let inCipherText = a8_2013
    --let cipherText  = cleanup inCipherText
    -- let fs = toDescList $ fromList $ L.map swap $ toList $ count2freq $ countChars cipherText
    let kp = solveVig $ cleanup inCipherText
    putStrLn $ "8A (key, plainText) = " ++ show kp

solve6B_2013::IO ()
solve6B_2013 = do
    inCipherText <- readFile "./src/2013/6B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = sortBy (flip compare) $ L.map swap $ toList $ count2freq $ countChars cipherText
    --putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = decipher (RailRoadCipher 9) cipherText
    putStrLn $ "6B = " ++ show pt

solve7B_2013::IO ()
solve7B_2013 = do
    let inCipherText  = b7_2013
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = sortBy (flip compare) $ L.map swap $ toList $ count2freq $ countChars cipherText
    --putStrLn $ "Frequencies are: " ++ (show fs)
    --putStrLn $ "ct = " ++ (show cipherText)
    --putStrLn $ "length of ct = " ++ (show $ length cipherText)
    let bgs = L.map (\i-> (i, var $ loseZeros $ countBigrams i cipherText)) [645..655]
    --putStrLn $ "bigrams = " ++ (show bgs)
    let bc = BifidCipher "THEORYABCDFGIKLMNPQSUVWXZ"
    let pt = decipher bc cipherText
    putStrLn $ "7B = " ++ show pt

solve8B_2013::IO ()
solve8B_2013 = do
    let inCipherText  = b8_2013
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = sortBy (flip compare) $ L.map swap $ toList $ count2freq $ countChars cipherText
    --putStrLn $ "Frequencies are: " ++ (show fs)
    --putStrLn $ "ct = " ++ (show cipherText)
    --putStrLn $ "length of ct = " ++ (show $ length cipherText)
    let pt = decipher (PlayfairCipher $ key "ADLERBCFGHIKMNOPQSTUVWXYZ") cipherText
    putStrLn $ "8B = " ++ show pt

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
    let r1 = applyCrib (Crib (Just 0) Nothing "HERRGOERING") cipherText
    let r2 = applyCrib (Crib Nothing (Just $ n-5) "HEILHITLER") cipherText
    let rs = L.reverse $ sortWith S.size $ consolidate $ r1 ++ r2
    putStrLn $ "Rules are: " ++ show rs
    let gs = grids rs
    putStrLn $ "Grids are: " ++ show (take 3 gs)


main_2013 :: IO ()
main_2013 = do
  solve3A_2013
  solve5B_2013
  solve6A_2013
  solve6B_2013
  solve7A_2013
  solve7B_2013
  solve8A_2013
  solve8B_2013



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
trifidFractionation = concatMap (\x -> fromJust $ M.lookup x $ fromList trifid2)

trifidDefractionation::String->String
trifidDefractionation ct = L.map (\x -> fromJust $ M.lookup x $ fromList $ L.map swap trifid2) $ mySplit 3 ct


-- This does the columnar transopition
-- Splits into pieces of size n and reads off in column order
-- No transposition yet
trifidTransposition::Int->String->String
trifidTransposition n ft = fromCols $ mySplit n ft

-- Splits the data into n pieces, reads off in piece order
trifidDetransposition::Int->String->String
trifidDetransposition m tt = trifidTransposition n tt
    where
        n = floor $ fromIntegral (length tt) / fromIntegral m

trifidCipher::Int->String->String
trifidCipher n pt = trifidDefractionation $ trifidTransposition n $ trifidFractionation pt

trifidDecipher::Int->String->String
trifidDecipher n pt = trifidDefractionation $ trifidDetransposition n $ trifidFractionation pt

-- Detransposes in column order
trifidDetranspose::Int->String->String
trifidDetranspose n ct = undefined
