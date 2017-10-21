{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Solve2017
    (
        stringTo32
        , main_2017
        , solve1A_2016
        , solve1B_2016
        , solve2A_2016
        , solve2B_2016
        --, solve3A_2016
        --, solve3B_2016
        --, solve4A_2016
        --, solve4B_2016
        --, solve5A_2016
        --, solve5B_2016
        --, solve6A_2016
        --, solve6B_2016
        --, solve7A_2016
        --, solve7B_2016
        --, solve8A_2016
        --, solve8B_2016
    ) where

import System.IO
import GHC.Exts (sortWith)
import Data.Char (isAlpha)
import Data.Map (toList)
import Data.List (map, sortBy, concatMap, sort)
import qualified Data.List as L
import qualified Data.Set as S (size)
import Data.Ord (comparing)
import Data.Monoid (mappend)
import Data.Tuple (swap)
import Numeric (showFFloat)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy as BS (length, take, drop, readFile)
import Data.Vector.Unboxed (generate)
import qualified Data.Vector.Unboxed as V (length)
import Data.ByteString.Lex.Fractional (readDecimal, readSigned)
import Numeric.LinearAlgebra hiding (toList)
import GHC.TypeLits
import Data.Proxy

import Analysis
import Cribs
import Cipher
import Vignere
import Bifid
import Hill
import Quadgram (qscore, addWordScore, calcIx, qgram, readDoubles)
import System.IO.Unsafe (unsafePerformIO)


main_2016 :: IO ()
main_2016 = do
        solve1A_2016
        solve1B_2016
        solve2A_2016
        solve2B_2016
        solve3A_2016
        solve3B_2016
        solve4A_2016
        solve4B_2016
        solve5A_2016
        solve5B_2016
        solve6A_2016
        solve6B_2016
        solve7A_2016
        solve7B_2016
        solve8A_2016
        solve8B_2016

hillTest :: IO ()
hillTest = do
  let pt = "HELLOMATEHOWAREYOU"
  let m = (2><2) [25::Z ./. 26, 1::Z ./. 26, 1::Z ./. 26, 0::Z ./. 26] -- Inverse of [0,1,1,1]
  let mi = myInv m
  putStrLn $ show mi
----------------------
  let ncols = Numeric.LinearAlgebra.cols m
  let nrows = Numeric.LinearAlgebra.rows m
  -- Groups of letters converted to [Int]
  --xs :: [[Int]]
  let xs = chunksOf ncols $ L.map (\c -> fromIntegral (nord c)) pt
  -- Convert the groups to vectors to a matrix
  --ws :: T Int
  let wsInt = fromLists xs
  let ws = fromZ wsInt
  -- Calculate the cipher text
  let cs = ws <> m
  putStrLn $ show ws
  putStrLn $ show m
  putStrLn $ show cs
  putStrLn $ show $ myInv m
  let ct = L.map (\x-> nchr $ fromIntegral x) $ concat $ Numeric.LinearAlgebra.toLists $ toInt cs
-----------------------------------
  let cp = HillCipher m
  let ct = cipher cp pt
  let ppt = decipher cp ct
  putStrLn pt
  putStrLn ct
  putStrLn ppt

stringTo32 :: String -> Int
stringTo32 s = go 0 s
    where
        go n [] = n
        go n (x:[]) = 2*n + if x == '1' then 1 else 0
        go n (x:xs) = go (2*n + if x == '1' then 1 else 0) xs


solve1A_2016::IO ()
solve1A_2016 = do
    inCipherText <- readFile "./src/2016/1A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let pt = solveVig cipherText
    putStrLn $ "1A pt = " ++ (show $ pt)
    return ()

solve1B_2016::IO ()
solve1B_2016 = do
    inCipherText <- readFile "./src/2016/1B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig cipherText
    putStrLn $ "1B: pt = " ++ (show $ fmap reverse pt)
    return ()

solve2A_2016::IO ()
solve2A_2016 = do
    inCipherText <- readFile "./src/2016/2A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig cipherText
    putStrLn $ "2A: pt = " ++ (show $ pt)
    return ()

solve2B_2016::IO ()
solve2B_2016 = do
    inCipherText <- readFile "./src/2016/2B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ cipherText
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)

    let km = cribMap "" ""
    let pCrib = "ETIOAHNKWSRQUMFDPYLXGVCB"
    let cCrib = "FCZDLUYJRXSNHTKAIBOWPMVQ"
    putStrLn $ show $ sortBy (comparing $ snd) $ zip pCrib cCrib
    let km1 = km `mappend` (cribMap pCrib cCrib)
    let pt2 = apply km1 cipherText
    putStrLn $ "2B: pt = " ++ (show $ pt2)

    return ()
{--
solve3A_2016::IO ()
solve3A_2016 = do
    inCipherText <- readFile "./src/2016/3A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    putStrLn $ "3A: cipherText = " ++ (show $ ct)
    let km = cribMap "" ""
    let pCrib = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let cCrib = "WESTONXYZABCDFGHIJKLMPQRUV"
    putStrLn $ show $ sortBy (comparing $ fst) $ zip pCrib cCrib
    let km1 = km `mappend` (cribMap pCrib cCrib)
    let pt2 = apply km1 ct
    putStrLn $ "3A: pt = " ++ (show $ pt2)
    return ()

solve3B_2016::IO ()
solve3B_2016 = do
    inCipherText <- readFile "./src/2016/3B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "Top 3grams are: " ++ (show top3grams)
    putStrLn $ "3B: cipherText = " ++ (show $ ct)
    let pCrib = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let cCrib = "NEURALVWXYZBCDFGHIJKMOPQST"
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap pCrib cCrib)
    let pt = apply km1 ct
    putStrLn $ "3B: pt = " ++ (show $ pt)
    return ()

solve4A_2016::IO ()
solve4A_2016 = do
    inCipherText <- readFile "./src/2016/4A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "Top 3grams are: " ++ (show top3grams)

    let pCrib = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let cCrib = "WAVEFORMXYZBCDGHIJKLNPQSTU"
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap pCrib cCrib)
    let pt = apply km1 ct
    putStrLn $ "4A: pt = " ++ (show $ pt)
    return ()

solve4B_2016::IO ()
solve4B_2016 = do
    inCipherText <- readFile "./src/2016/4B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ ct
    putStrLn $ "The cipher text is " ++ show (length ct) ++ " charcters long"
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show $ take 26 fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "Top 3grams are: " ++ (show top3grams)
    putStrLn $ "This analysis shows it's a transcription cipher. The letter frequencies are good, but the"
    putStrLn $ "trigrams are not"

    let pt = concatMap (\c -> [c!!2, c!!3, c!!1, c!!0, c!!4]) $ chunksOf 5 $ reverse ct
    putStrLn $ "4B: pt = " ++ (show $ pt)

    return ()


topTriGrams :: Int -> String -> [Ngram]
topTriGrams n pt = fmap fst $ take n $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 pt


solve5A_2016::IO ()
solve5A_2016 = do
    inCipherText <- readFile "./src/2016/5A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "============================================================================="
    putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "Frequencies are: " ++ (show fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "Top 3grams are: " ++ (show top3grams)
    let pCrib = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let cCrib = "CHARLIESTUVWXYZBDFGJKMNOPQ"
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap pCrib cCrib)
    let pt = apply km1 ct
    putStrLn $ "5A: pt = " ++ (show $ pt)
    return ()


solve5B_2016::IO ()
solve5B_2016 = do
    inCipherText <- readFile "./src/2016/5B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    --putStrLn $ "============================================================================="
    --putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    --putStrLn $ "Frequencies are: " ++ (show fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    --putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    --putStrLn $ "Top 3grams are: " ++ (show top3grams)
    -- So it's a transposition cipher.
    -- Lets look at the difference frequencies for dynamix
    let cf = charFreqs 'I' 'X' ct -- shows that 294 is possible
    let cf = charFreqs 'D' 'Y' ct -- shows that 294 is possible
    let cf = charFreqs 'T' 'H' ct -- shows that 294 is possible
    let key = [4, 0, 1, 3, 2]
    let cp = TranspositionCipher key
    let pt = decipher cp ct
    putStrLn $ "5B: pt = " ++ (show $ pt)
    return ()


solve6A_2016::IO ()
solve6A_2016 = do
    inCipherText <- readFile "./src/2016/6A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    --putStrLn $ "============================================================================="
    --putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    --putStrLn $ "Frequencies are: " ++ (show fs)
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    --putStrLn $ "Top bigrams are: " ++ (show topBigrams)
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    --putStrLn $ "Top 3grams are: " ++ (show top3grams)
    let pt = solveVig ct
    putStrLn $ "6A: plainText = " ++ (snd pt)
    return ()

solve6B_2016::IO ()
solve6B_2016 = do
    inCipherText <- readFile "./src/2016/6B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    --putStrLn $ "============================================================================="
    --putStrLn $ "cipherText = " ++ ct
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    --putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig ct
    putStrLn $ "6B: plainText = " ++ (show pt)
    return ()

--}
