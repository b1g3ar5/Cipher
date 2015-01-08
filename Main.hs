{-# LANGUAGE NoImplicitPrelude #-}

module Main
    (
        main   
        , solve1A 	
        , solve1B
        , solve2A
        , solve2B
        , solve3A
        , solve3B
        , solve4A
        , solve4B
        , solve5A
        , solve5B
        , solve6A
        , solve6B
        , solve7A
        , solve7B
        , solve8A
        , solve8B
    ) where

import System.IO
import GHC.Exts hiding (toList)
import Control.Monad
--import Data.Array as A
import Data.Char
import Data.Int
import Data.Maybe
import Data.Map as M
import Data.List as L
import Data.Monoid
--import Data.List.Split
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
--import Tree
--import Dict
import CTexts
import Mod hiding (toList)
--import Words
--import Text.Regex

import NumericPrelude
import MathObj.Matrix as MM hiding (zipWith)
--import Algebra.Vector hiding (Eq(..))


-- shows n decimal places for a double
dshow::Int->Double->String
dshow n d = printf ("%."++ show n ++"f") d
          
-- Switches a->z, b->y etc (when n=26)
--reverseTxt::String->String       
--reverseTxt txt = L.map (\c-> chr $ nAlphabet - 1 + 2*65 - ord c) txt                 

-- just split into bits of size n
mySplit n = unfoldr $ \xs -> case xs of
     [] -> Nothing
     _  -> Just (splitAt n xs)
    
fromCols::[[a]]->[a]
fromCols cols = fromCols' [] cols

fromCols'::[a]->[[a]]->[a]
fromCols' acc [] = acc
fromCols' acc xss = fromCols' (acc ++ (L.map head xss)) (L.map tail xss)

solve1A::IO ()
solve1A = do
    inCipherText <- readFile "./data/1A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let c = ShiftCipher 'E'
    let pt = decipher c cipherText    
    putStrLn $ "plainText = " ++ (show $ pt)
    return ()
    
solve1B::IO ()
solve1B = do
    inCipherText <- readFile "./data/1B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ M.toList $ dtoMap $ count2freq $ txt2count cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'W'
    let pt = decipher c cipherText    
    putStrLn $ "plainText = " ++ (show $ pt)
    return ()
    
solve2A::IO ()
solve2A = do
    inCipherText <- readFile "./data/2A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ M.toList $ dtoMap $ count2freq $ txt2count cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c cipherText    
    putStrLn $ "plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 3 fs)
    --let km2 = mappend km1 $ cribMap "HRY" "MKT"
    let km2 =  km1 `mappend` (cribMap "DRMKHYNSFOPIWCLBGVU" "SKLBMTQPCVARJNGIHEZ")
    let pt2 = apply km2 cipherText    

    putStrLn $ "plainText = " ++ (show $ pt2)
    return ()
    
solve2B::IO ()
solve2B = do    
    inCipherText <- readFile "./data/2B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ cipherText
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c cipherText    
    putStrLn $ "plainText = " ++ (show $ pt)

    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 6 fs)
    let km2 = km1 `mappend` (cribMap "ETAWHRGNOSIPJYLDUCMVBKQF" "MCFHPZOVWBQXRJTGDAUELSYN")
    -- let km2 = mappend km1 $ cribMap "ABCDEFGHIJKLMNOPQRSTUVWY" "FLAGMNOPQRSTUVWXYZBCDEHJ"
    let pt3 = apply km2 cipherText  
    let cc = take 50 pt3  

    putStrLn $ "plainText = " ++ (show $ pt3)
    return ()

solve3A::IO ()
solve3A = do
    inCipherText <- readFile "./data/3A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c cipherText    
    putStrLn $ "plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 3 fs)
    let km2 = km1 `mappend` (cribMap "ETAHRYOUNSILMBKDPCVWFGJX" "DMLKQPJXYBVCNWRSUHITOZGE")
    let pt2 = apply km2 cipherText    
    putStrLn $ "plainText = " ++ (show $ pt2)
    return ()
    
solve3B::IO ()
solve3B = do    
    inCipherText <- readFile "./data/3B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c ct    
    putStrLn $ "plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 6 fs)
    let km2 = km1 `mappend` (cribMap "ETAHSPROGLWKCUDIMNFBVYJ" "OJSUIDGCTYMXAKHVZBRELPW")
    --let km2 = mappend km1 $ cribMap "ABCDEFGHIJKLMNOPRSTUVWY" "SEAHORTUVWXYZBCDGIJKLMP"
    let pt3 = apply km2 ct  
    let cc = take 50 pt3  
    putStrLn $ "plainText = " ++ (show $ pt3)
    return ()

solve4A::IO ()
solve4A = do
    inCipherText <- readFile "./data/4A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c ct    
    putStrLn $ "plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 3 fs)
    let km2 = km1 `mappend` (cribMap "ETAHRYDOCPULWMSVNGIBFKQX" "NJSWHPRDEFKAMBILCVXTUZGO")
    --let km2 = km1 `mappend` (cribMap "ABCDEFGHIJKLMNOPRSTUVWXY" "STERNUVWXZABCDFGHIJKLMOP")
    let pt2 = apply km2 ct    
    putStrLn $ "plainText = " ++ (show $ pt2)
    return ()
    
solve4B::IO ()
solve4B = do    
    inCipherText <- readFile "./data/4B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = decipherRailRoad 5 ct
    putStrLn $ "plainText = " ++ pt
    return ()

solve5A::IO ()
solve5A = do    
    inCipherText <- readFile "./data/5A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 3 fs)
    let km2 = km1 `mappend` (cribMap "HRYVPCDKINOSMGLXWUBFZQJ" "THPMFABWUZCJYDXONLERQGV")
    let pt2 = apply km2 ct    
    putStrLn $ "plainText = " ++ (show $ pt2)
    return ()

solve5B::IO ()
solve5B = do    
    inCipherText <- readFile "./data/5B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let key = [5,3,1,0,2,4]::[Int]
    let pt = decipher (TranspositionCipher key) ct   
    putStrLn $ "plainText = " ++ pt
    return ()

solve6A::IO ()
solve6A = do    
    inCipherText <- readFile "./data/6A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = decipherRailRoad 3 ct
    putStrLn $ "plainText = " ++ pt
    return ()

solve6B::IO ()
solve6B = do    
    inCipherText <- readFile "./data/6B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let m1 = hillCrib (take 4 ct) "PHAS"
    let m2 = hillCrib (take 4 $ drop 2 ct) "ASES"
    let m3 = hillCrib (take 4 $ drop 2 ct) "ESIX"
    let m = MM.fromList 2 2 ([25,1,1,0]::[Int]) -- Inverse of [0,1,1,1]
    let pt = decipher (HillCipher m) ct


    putStrLn $ "plainText = " ++ pt
    return ()

solve7A::IO ()
solve7A = do    
    inCipherText <- readFile "./data/7A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig ct
    putStrLn $ "plainText = " ++ (snd pt)
    return ()

solve7B::IO ()
solve7B = do   
    -- This is an AMSCO cipher 
    inCipherText <- readFile "./data/7B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let code = [2,0,1,4,3]::[Int] -- column order
    let recode = L.map snd $ sortWith fst $ zip code [0..]::[Int]
    let pt = decipher (AmscoCipher code) ct
    putStrLn $ "plainText = " ++ pt
    
solve8A::IO ()
solve8A = do    
    inCipherText <- readFile "./data/8A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig ct
    putStrLn $ "plainText = " ++ (snd pt)
    return ()

solve8B::IO ()
solve8B = do    
    inCipherText <- readFile "./data/8B.txt"
    let bt  = take 7000 inCipherText
    let ct = L.map (nchr.stringTo32) $ chunksOf 5 $ bt
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count ct
    return ()


go nChunks isTail ct crib = L.map (  (L.filter (\t->(snd t)==crib))
                                    .(zip [0..])
                                    .(chunksOf 2)
                                    .(if isTail then tail else id)
                                  )  $ chunksOf (length ct `div` nChunks) ct

get nChunks isTail ct col offSet = concat $ drop offSet $ ((chunksOf 2) $ (if isTail then tail else id) $ (chunksOf (length ct `div` nChunks) $ ct)!!col)

ccrib ct isTail i = L.filter (\t-> length (L.map snd $ snd t)>0) $ zip [0..] $ Main.go 5 isTail ct $ L.map (\w->w!!i) ["PHASE","EIGHT"]


ts ct = zip (L.map (ccrib ct True) [0..4]) $ repeat True
fs ct = zip (L.map (ccrib ct False) [0..4]) $ repeat False


ftable = [[(4, 45, False)], [(3, 14, False)], [(0, 118, False),(3, 15, False),(3, 52, False)], [(0, 108, False)], [(0,39, False),(1,19,False),(1,50,False),(2,36,False),(2,128,False),(3,43,False)]]

ttable = [[],[(1,30,True),(2,33,True),(2,49,True),(2,97,True)],[(1,121,True),(2,14,True)],[(0,12,True),(4,116,True)],[(0,8,True),(1,73,True),(1,132,True),(2,81,True),(2,93,True)]]

alltable = L.zipWith (\a b -> concat [a,b]) ftable ttable

try1 ct alltable coords= L.transpose $ L.zipWith (\ix cs -> cs!!ix) coords $ L.map (\ps -> L.map (\(c,o,b)-> get 5 b ct c o) ps) alltable

allCoords = L.map (\x-> [0,0,0,1,x]) [0..10]

tries ct = L.map (\c -> try1 ct alltable c) allCoords



stringTo32 :: String -> Int
stringTo32 s = go 0 s
    where
        go n [] = n
        go n (x:[]) = 2*n + if x == '1' then 1 else 0 
        go n (x:xs) = go (2*n + if x == '1' then 1 else 0) xs

-- Turns a string 0f 0's ans 1's into an integer
bstringTo32 :: BL.ByteString -> Int
bstringTo32 s = go 0 s
    where
        go n bs = if (BL.null bs) then 
                        n
                  else
                      if BL.length bs > 1 then 
                            go (2*n + if (BL.head bs) == BSI.c2w '1' then 1 else 0) $ BL.tail bs
                      else
                            2*n + if (BL.head bs) == BSI.c2w '1' then 1 else 0

bchunksOf::Int64 -> BL.ByteString->[BL.ByteString]
bchunksOf n xs = if (BL.null xs) then [] else (BL.take n xs):(bchunksOf n $ BL.drop n xs)


testPeriodBifid::IO ()
testPeriodBifid = do
    let inCipherText  = b7_2013
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let bs = "THEORYABCDFGIKLMNPQSUVWXZ"
    let pt = decipher (BifidCipher bs) cipherText
    putStrLn $ "pt = " ++ (show pt)
    let ct = cipher (BifidPeriodCipher 14 bs) pt
    putStrLn $ "Encoded with PeriodBifid= " ++ (show ct)
    let bgs = L.map (\i-> (i, var $ loseZeros $ txtBigramCount i ct)) [1..10]
    putStrLn $ "bigrams = " ++ (show bgs)

testBifidCrib:: IO () 
testBifidCrib = do 
    let inCipherText  = b8_2013
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let n = length cipherText
    --let r1 = applyCrib (Crib (Just 0) (Just $ n `quot` 2) "HERRGOERING") cipherText
    --let r2 = applyCrib (Crib (Just $ (n `quot` 2)-5) (Just $ n-5) "HEILHITLER") cipherText
    let r1 = applyCrib (Crib (Just 0) (Nothing) "HERRGOERING") cipherText
    let r2 = applyCrib (Crib (Nothing) (Just $ n-5) "HEILHITLER") cipherText
    let rs = L.reverse $ sortWith (S.size) $ consolidate $ r1 ++ r2
    putStrLn $ "Rules are: " ++ (show rs)
    let gs = grids rs
    putStrLn $ "Grids are: " ++ (show $ take 3 gs)
    
    
main :: IO ()
main = do
    let inCipherText  = b8_2013
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ toList $ dtoMap $ count2freq $ txt2count cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    putStrLn $ "ct = " ++ (show cipherText)
    let bc = BifidCipher "THEORYABCDFGIKLMNPQSUVWXZ"
    --let pt = bifidDecipher bs cipherText
    let pt = decipher bc cipherText
    putStrLn $ "pt = " ++ (show pt)
  
    
    
{-    
-- Index of the minimum starting with 0
ixOfMin::Ord a=>[a]->Int
ixOfMin xs = fromJust $ elemIndex (minimum xs) xs

-- Index of the minimum starting with 0
ixOfMax::Ord a=>[a]->Int
ixOfMax xs = fromJust $ elemIndex (maximum xs) xs
-}

-- Shift a character by an int
--cshift::Char->Char->Char
--cshift k ' ' = ' '
--cshift k c = chr $ (ord 'A') + (mod (ord k + ord c - ord 'A' - ord 'A') nAlphabet)

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
trifidFractionation pt = concatMap (\x -> fromJust $ M.lookup x $ M.fromList trifid2) pt          
        
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
        n = (length tt) `div` m

trifidCipher::Int->String->String
trifidCipher n pt = trifidDefractionation $ trifidTransposition n $ trifidFractionation pt
   
trifidDecipher::Int->String->String
trifidDecipher n pt = trifidDefractionation $ trifidDetransposition n $ trifidFractionation pt
   
-- Detransposes in column order   
trifidDetranspose::Int->String->String   
trifidDetranspose n ct = undefined
   
{-
-- Works out the correlation of an offset of n of a string with standard
-- english text frequencies
corr::Char->String->Double
corr c txt = sum $ zipWith (\a b -> (snd a)*(snd b)) cntTxt letterFreq
    where
        shiftedTxt = L.map (cshift c) txt
        cntTxt = toAscList $ count2freq $ txt2count shiftedTxt
-}
        
