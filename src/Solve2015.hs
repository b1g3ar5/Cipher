-- {-# LANGUAGE NoImplicitPrelude #-}

module Solve2015
    (
        stringTo32
        , main_2015
        , solve1A_2015
        , solve1B_2015
        , solve2A_2015
        , solve2B_2015
        , solve3A_2015
        , solve3B_2015
        , solve4A_2015
        , solve4B_2015
        , solve5A_2015
        , solve5B_2015
        , solve6A_2015
        , solve6B_2015
        , solve7A_2015
        , solve7B_2015
        , solve8A_2015
        , solve8B_2015
    ) where

import System.IO
import GHC.Exts (sortWith)
import Data.Char (isAlpha)
import Data.Map (toList)
import Data.List as L
import Data.Monoid (mappend)
import Data.Tuple (swap)
-- import NumericPrelude
-- import MathObj.Matrix (fromList)

import Analysis
import Cribs
import Cipher
import Vignere


main_2015 :: IO ()
main_2015 = do
        solve1A_2015
        solve1B_2015
        solve2A_2015
        solve2B_2015
        solve3A_2015
        solve3B_2015
        solve4A_2015
        solve4B_2015
        solve5A_2015
        solve5B_2015
        solve6A_2015
        solve6B_2015
        solve7A_2015
        solve7B_2015
        solve8A_2015
        solve8B_2015

stringTo32 :: String -> Int
stringTo32 s = go 0 s
    where
        go n [] = n
        go n (x:[]) = 2*n + if x == '1' then 1 else 0
        go n (x:xs) = go (2*n + if x == '1' then 1 else 0) xs


solve1A_2015::IO ()
solve1A_2015 = do
    inCipherText <- readFile "./src/2015/1A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let c = ShiftCipher 'M'
    let pt = decipher c cipherText
    putStrLn $ "1A: plainText = " ++ (show $ pt)
    return ()

solve1B_2015::IO ()
solve1B_2015 = do
    inCipherText <- readFile "./src/2015/1B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'O'
    let pt = decipher c cipherText
    putStrLn $ "1B: plainText = " ++ (show $ pt)
    return ()

solve2A_2015::IO ()
solve2A_2015 = do
    inCipherText <- readFile "./src/2015/2A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'P'
    let pt = decipher c cipherText
    putStrLn $ "2A: plainText = " ++ (show $ pt)
    return ()

solve2B_2015::IO ()
solve2B_2015 = do
    inCipherText <- readFile "./src/2015/2B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ cipherText
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'V'
    let pt = decipher c cipherText
    putStrLn $ "2B: plainText = " ++ (show $ pt)
    return ()

solve3A_2015::IO ()
solve3A_2015 = do
    inCipherText <- readFile "./src/2015/3A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'T'
    let pt = decipher c cipherText
    -- putStrLn $ "plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "HARYCLIETNSOUDKMBFWGPZXV" "CHGBNOFTMUJXPQLRKWVZAEYS")
    let pt2 = apply km1 cipherText
    putStrLn $ "3A: plainText = " ++ (show $ pt2)
    return ()

solve3B_2015::IO ()
solve3B_2015 = do
    inCipherText <- readFile "./src/2015/3B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'O'
    let pt = decipher c ct
    -- putStrLn $ "plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "EYSONLIRATUPQMFDKGCHBVWJ" "FBXDYOZSLCHINTKAJPVUQMRE")
    let pt = apply km1 ct
    putStrLn $ "3B: plainText = " ++ (show $ pt)
    return ()

solve4A_2015::IO ()
solve4A_2015 = do
    inCipherText <- readFile "./src/2015/4A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let c = ShiftCipher 'N'
    let pt = decipher c ct
    -- putStrLn $ "plainText = " ++ (show $ pt)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "HARYCLIETSDOKNPWFMGBUVZX" "SRFPTXUIHGLBWZCMNYEAJKQO")
    let pt2 = apply km1 ct
    putStrLn $ "4A: plainText = " ++ (show $ pt2)
    return ()

solve4B_2015::IO ()
solve4B_2015 = do
    inCipherText <- readFile "./src/2015/4B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show $ take 9 fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "Top 3grams are: " ++ (show top3grams)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "THEWIASDOKRCNMBGVUYLFPZJ" "HSCLTFGNZVEAYXRIKJPWOBQU")
    let pt = apply km1 ct
    putStrLn $ "4B: plainText = " ++ pt
    return ()

solve5A_2015::IO ()
solve5A_2015 = do
    inCipherText <- readFile "./src/2015/5A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "Top 3grams are: " ++ (show top3grams)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "THEAYRILDGCSPNBMOKUJWVFZ" "GLFCPADUNERBYWOVXTHSKJIQ")
    let pt = apply km1 ct
    putStrLn $ "5A: plainText = " ++ (show $ pt)
    return ()

solve5B_2015::IO ()
solve5B_2015 = do
    inCipherText <- readFile "./src/2015/5B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig ct
    putStrLn $ "5B: plainText = " ++ (snd pt)
    return ()

solve6A_2015::IO ()
solve6A_2015 = do
    inCipherText <- readFile "./src/2015/6A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "Top 3grams are: " ++ (show top3grams)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETAIONSHRGDCLWVYPMKUFJBXZ" "FHRSYXGCDNLIVMKPZWUJETAOQ")
    let pt = apply km1 ct
    putStrLn $ "6A: plainText = " ++ pt
    return ()

solve6B_2015::IO ()
solve6B_2015 = do
    inCipherText <- readFile "./src/2015/6B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig ct
    putStrLn $ "6B: plainText = " ++ (snd pt)
    return ()

solve7A_2015::IO ()
solve7A_2015 = do
    inCipherText <- readFile "./src/2015/7A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "Top 3grams are: " ++ (show top3grams)
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap "ETINAHORYCLINDGKPUWSBFMV" "IHUZBTAFPRXUZLSWCJMGENYK")
    let pt = apply km1 ct
    putStrLn $ "7A: plainText = " ++ pt
    return ()

topTriGrams :: Int -> String -> [Ngram]
topTriGrams n ct = fmap fst $ take n $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct


solve7B_2015::IO ()
solve7B_2015 = do
    -- This is an AMSCO cipher
    -- It was cracked by trying all the kets (key length known to be 6
    -- with a [1,2] and a [2,1] start. The resulting plain texts with "THE" as the
    -- most common trigram were filtered out and inspected. Only one of them was proper text
    inCipherText <- readFile "./src/2015/7B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    let (pt, code) = head $ reverse $ solveAmsco False 6 ct
    putStrLn $ "cipherText = " ++ ct
    putStrLn $ "This analysis shows it's a transcription cipher. The letter frequencies are good, but the"
    putStrLn $ "trigrams are not"
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "Top trigrams are: " ++ (show top3grams)
    putStrLn $ "7B: plainText = " ++ pt

solve8A_2015::IO ()
solve8A_2015 = do
    inCipherText <- readFile "./src/2015/8A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig ct
    putStrLn $ "8A: plainText = " ++ (snd pt)
    return ()

solve8B_2015::IO ()
solve8B_2015 = do
    inCipherText <- readFile "./src/2015/8B.txt"
    -- key = KH, 9H, 8S, 4C, 5C, 3S
    --       4D, KS, 7D, KC, JK, 3D
    --       4H, AS, QH, 8H, 4S, AD
    --       2S, 7C, AH, 5S, AC, 6C
    --       7H, 2D, JJ, TH, 5D, JD
    --       3C, 9C, QS, JH, TD, xx
    --       xx, xx, xx, xx, xx, xx
    --       xx, QD, xx, xx, xx, xx
    --       xx, xx, 6H, xx, 3H, xx

    -- key = 39, 35, 47,  4,  5, 42
    --       17, 52, 20, 13, 53, 16
    --       30, 40, 38, 34, 43, 14
    --       41,  7, 27, 44,  1,  6
    --       33, 15, 53, 36, 18, 24
    --        3,  9, 51, 37, 23, xx
    --       xx, xx, xx, xx, xx, xx
    --       xx, 25, xx, xx, xx, xx
    --       xx, xx, 32, xx, 29, xx

     -- 2, 8,10,11,12,19,21,22,26,28,31,45,46,48,49,50

    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ L.map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig ct
    putStrLn $ "8B: plainText = " ++ (snd pt)
    return ()
