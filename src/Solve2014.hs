-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

module Solve2014
    (
        stringTo32
        , main_2014
        , solve1A_2014
        , solve1B_2014
        , solve2A_2014
        , solve2B_2014
        , solve3A_2014
        , solve3B_2014
        , solve4A_2014
        , solve4B_2014
        , solve5A_2014
        , solve5B_2014
        , solve6A_2014
        --, solve6B_2014
        , solve7A_2014
        , solve7B_2014
        , solve8A_2014
        , solve8B_2014
    ) where

import GHC.TypeLits
import System.IO
import GHC.Exts (sortWith)
import Data.Char (isAlpha)
import Data.Map as M (toList)
import Data.List as L
import Data.Monoid (mappend)
import Data.Tuple (swap)
--import Numeric.LinearAlgebra
-- import NumericPrelude
-- import MathObj.Matrix (fromList)

import Analysis
import Cribs
import Cipher
import Vignere
--import Mod (matFromList, Mat(..), Mod(..), type (/)())


main_2014 :: IO ()
main_2014 = do
        solve1A_2014
        solve1B_2014
        solve2A_2014
        solve2B_2014
        solve3A_2014
        solve3B_2014
        solve4A_2014
        solve4B_2014
        solve5A_2014
        solve5B_2014
        solve6A_2014
        --solve6B_2014
        solve7A_2014
        solve7B_2014
        solve8A_2014
        solve8B_2014

stringTo32 :: String -> Int
stringTo32 s = go 0 s
    where
        go n [] = n
        go n (x:[]) = 2*n + if x == '1' then 1 else 0
        go n (x:xs) = go (2*n + if x == '1' then 1 else 0) xs


solve1A_2014::IO ()
solve1A_2014 = do
    inCipherText <- readFile "./src/2014/1A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let c = ShiftCipher 'E'
    let pt = decipher c cipherText
    putStrLn $ "1A: plainText = " ++ (show $ pt)
    return ()

solve1B_2014::IO ()
solve1B_2014 = do
    inCipherText <- readFile "./src/2014/1B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'W'
    let pt = decipher c cipherText
    putStrLn $ "1B: plainText = " ++ (show $ pt)
    return ()

solve2A_2014::IO ()
solve2A_2014 = do
    inCipherText <- readFile "./src/2014/2A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c cipherText
    putStrLn $ "2A: plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 3 fs)
    --let km2 = mappend km1 $ cribMap "HRY" "MKT"
    let km2 =  km1 `mappend` (cribMap "DRMKHYNSFOPIWCLBGVU" "SKLBMTQPCVARJNGIHEZ")
    let pt2 = apply km2 cipherText

    putStrLn $ "2A: plainText = " ++ (show $ pt2)
    return ()

solve2B_2014::IO ()
solve2B_2014 = do
    inCipherText <- readFile "./src/2014/2B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ cipherText
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c cipherText
    -- putStrLn $ "2B: plainText = " ++ (show $ pt)

    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 6 fs)
    let km2 = km1 `mappend` (cribMap "ETAWHRGNOSIPJYLDUCMVBKQF" "MCFHPZOVWBQXRJTGDAUELSYN")
    -- let km2 = mappend km1 $ cribMap "ABCDEFGHIJKLMNOPQRSTUVWY" "FLAGMNOPQRSTUVWXYZBCDEHJ"
    let pt3 = apply km2 cipherText
    let cc = take 50 pt3

    putStrLn $ "2B: plainText = " ++ (show $ pt3)
    return ()

solve3A_2014::IO ()
solve3A_2014 = do
    inCipherText <- readFile "./src/2014/3A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c cipherText
    -- putStrLn $ "3A: plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 3 fs)
    let km2 = km1 `mappend` (cribMap "ETAHRYOUNSILMBKDPCVWFGJX" "DMLKQPJXYBVCNWRSUHITOZGE")
    let pt2 = apply km2 cipherText
    putStrLn $ "3A: plainText = " ++ (show $ pt2)
    return ()

solve3B_2014::IO ()
solve3B_2014 = do
    inCipherText <- readFile "./src/2014/3B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c ct
    -- putStrLn $ "3B: plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 6 fs)
    let km2 = km1 `mappend` (cribMap "ETAHSPROGLWKCUDIMNFBVYJ" "OJSUIDGCTYMXAKHVZBRELPW")
    --let km2 = mappend km1 $ cribMap "ABCDEFGHIJKLMNOPRSTUVWY" "SEAHORTUVWXYZBCDGIJKLMP"
    let pt3 = apply km2 ct
    let cc = take 50 pt3
    putStrLn $ "3B: plainText = " ++ (show $ pt3)
    return ()

solve4A_2014::IO ()
solve4A_2014 = do
    inCipherText <- readFile "./src/2014/4A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'A'
    let pt = decipher c ct
    -- putStrLn $ "4A: plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 3 fs)
    let km2 = km1 `mappend` (cribMap "ETAHRYDOCPULWMSVNGIBFKQX" "NJSWHPRDEFKAMBILCVXTUZGO")
    --let km2 = km1 `mappend` (cribMap "ABCDEFGHIJKLMNOPRSTUVWXY" "STERNUVWXZABCDFGHIJKLMOP")
    let pt2 = apply km2 ct
    putStrLn $ "4A: plainText = " ++ (show $ pt2)
    return ()

solve4B_2014::IO ()
solve4B_2014 = do
    inCipherText <- readFile "./src/2014/4B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = decipherRailRoad 5 ct
    putStrLn $ "4B: plainText = " ++ pt
    return ()

solve5A_2014::IO ()
solve5A_2014 = do
    inCipherText <- readFile "./src/2014/5A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETA" $ L.map snd $ take 3 fs)
    let km2 = km1 `mappend` (cribMap "HRYVPCDKINOSMGLXWUBFZQJ" "THPMFABWUZCJYDXONLERQGV")
    let pt2 = apply km2 ct
    putStrLn $ "5A: plainText = " ++ (show $ pt2)
    return ()

solve5B_2014::IO ()
solve5B_2014 = do
    inCipherText <- readFile "./src/2014/5B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let key = [5,3,1,0,2,4]::[Int]
    let pt = decipher (TranspositionCipher key) ct
    putStrLn $ "5B: plainText = " ++ pt
    return ()

solve6A_2014::IO ()
solve6A_2014 = do
    inCipherText <- readFile "./src/2014/6A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = decipherRailRoad 3 ct
    putStrLn $ "6A: plainText = " ++ pt
    return ()

{-
solve6B_2014::IO ()
solve6B_2014 = do
    inCipherText <- readFile "./src/2014/6B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let m1 = hillCrib (take 4 ct) "PHAS"
    let m2 = hillCrib (take 4 $ drop 2 ct) "ASES"
    let m3 = hillCrib (take 4 $ drop 2 ct) "ESIX"
    -- :set -XDataKinds -XTypeOperators
    let m = (2><2) [25::Z ./. 26, 1::Z ./. 26, 1::Z ./. 26, 0::Z ./. 26] -- Inverse of [0,1,1,1]
    let hc = HillCipher m
    let pt = decipher (HillCipher m) ct

    -- Try to work out the m from a crib
    let c = (2><2) [7,22,18,18] :: Matrix (Z ./. 26) --HWSS, from ct
    let p = (2><2) [15,7,0,18] :: Matrix (Z ./. 26) -- PHAS from pt
    let m = (2><2) [25,1,1,0] :: Matrix (Z ./. 26) -- PHAS from pt
    -- so now c<>m == p
    -- luSolve' (luPacked' c) p should solve c * m = p for m?
    -- But this doesn't work. It should do because there are no coprime issues
    -- But c has no inverse because 18 has no inverse wrt 26, so it doesn't
    putStrLn $ "6B: plainText = " ++ pt
    return ()
-}

solve7A_2014::IO ()
solve7A_2014 = do
    inCipherText <- readFile "./src/2014/7A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig ct
    putStrLn $ "7A: plainText = " ++ (snd pt)
    return ()

solve7B_2014::IO ()
solve7B_2014 = do
    -- This is an AMSCO cipher
    inCipherText <- readFile "./src/2014/7B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let code = [2,0,1,4,3]::[Int] -- column order
    let recode = L.map snd $ sortWith fst $ zip code [0..]::[Int]
    let pt = decipherOld (AmscoCipher False code) ct
    putStrLn $ "7B: plainText = " ++ pt

solve8A_2014::IO ()
solve8A_2014 = do
    inCipherText <- readFile "./src/2014/8A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ M.toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig ct
    putStrLn $ "8A: plainText = " ++ (snd pt)
    return ()

solve8B_2014::IO ()
solve8B_2014 = do
    inCipherText <- readFile "./src/2014/8B.txt"
    let bt  = take 7000 inCipherText
    let ct = L.map (nchr.stringTo32) $ chunksOf 5 $ bt
    -- Length of ct is 1400 = 56*25, so potential keys are 2,4,7,8...
    -- Guess 7
    let m = 7::Int
    -- Then blokcs will be of 7*25=175
    let blocks = chunksOf (m*25) ct
    -- BABALMANDABSTRAIT is probably a crib so where are the B's
    let bcount = fmap (\blk -> length $ filter (\t-> fst t =='B') $ zip blk [1..]) blocks
    -- Mostly in the 4th block, so let's look at that
    let blk = blocks!!3
    let cols = L.transpose $ chunksOf m blk
    -- Now we need to find BNA, BAT, AB and ADI
    -- BNA is in col!!3 at 4
    -- BAT is in col!!6 at 16
    -- AB  is in col!!1 at 24
    -- ADI is in col!!5 at 12
    -- Now we're looking for LS, MT and AR
    -- These give us the following
    let tryKey = [19,24,2,4,7,12,16]::[Int]
    -- Now there's no need for BABLMANDAB to be at the top of the column, so we can add [1..24] to this
    -- It would be good if they were in order, so try adding 6 to get 19 to zero
    -- Doing the spin with these keys gives:
    let xs = L.map (\t -> spin (fst t) $ snd t) $ zip tryKey cols
    -- Which gives as the translation:
    putStrLn $ show $ L.transpose [xs!!3,xs!!5,xs!!6,xs!!1,xs!!2,xs!!4,xs!!0]
    -- Notice that the first 6 sets work OK then the 7th doesn't work - this means we need to add 6 to tryKey
    let tryKey1 = fmap (\x -> (6+x) `mod` 25) tryKey
    let xs1 = L.map (\t -> spin (fst t) $ snd t) $ zip tryKey1 cols
    -- Which gives as the translation:
    putStrLn $ show $ L.transpose [xs1!!3,xs1!!5,xs1!!6,xs1!!1,xs1!!2,xs1!!4,xs1!!0]


    let ky = L.map nord "FINALTX"
    let cp = CadenusCipher ky
    let pt = decipher cp ct
    putStrLn $ "8B: plainText = " ++ pt
    return ()
