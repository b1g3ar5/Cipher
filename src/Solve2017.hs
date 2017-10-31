{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Solve2017
    (
        stringTo32
        , main_2017
        , solve1A_2017
        , solve1B_2017
        , solve2A_2017
        , solve2B_2017
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
--import Numeric.LinearAlgebra hiding (toList)
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


main_2017 :: IO ()
main_2017 = do
        solve1A_2017
        --solve1B_2017
        --solve2A_2017
        --solve2B_2017


stringTo32 :: String -> Int
stringTo32 s = go 0 s
    where
        go n [] = n
        go n (x:[]) = 2*n + if x == '1' then 1 else 0
        go n (x:xs) = go (2*n + if x == '1' then 1 else 0) xs


solve1A_2017::IO ()
solve1A_2017 = do
    inCipherText <- readFile "./src/2017/1A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "cipherText = " ++ (show cipherText)
    let cipherCount = countChars cipherText
    let cipherFreq = count2freq cipherCount
    let keyICs = map (\n-> splitIC n cipherText) [1..2]
    putStrLn $ "key ICs = " ++ (show keyICs)
    let bestKeySize = (ixOfMin $ map (\d->(d-1.73)**2.0) keyICs) +1
    putStrLn $ "bestKeySize = " ++ (show bestKeySize)
    let bestSplit = splitText bestKeySize cipherText -- an Array of Strings of size bestKeySize
    putStrLn $ "bestSplit = " ++ (show bestSplit)
    let letterCorrelations = map (\s-> ixOfMin $ (map (\n ->(1.0-(Analysis.corr n s))**2.0) ("ABCDEFGHIJKLMNOPQRSTUVWXYZ"++[nchr 30]))) bestSplit
    putStrLn $ "letterCorrelations = " ++ (show letterCorrelations)
    let decodeKey = map (\i->nchr i) letterCorrelations
    putStrLn $ "decodeKey = " ++ (show decodeKey)
    let codeKey = fmap (\k-> nchr $ nAlphabet - nord k) decodeKey
    putStrLn $ "codeKey = " ++ (show codeKey)
    let plain = cipher (VigCipher decodeKey) cipherText
    putStrLn $ "1A pt = " ++ (show $ plain)
    return ()

solve1B_2017::IO ()
solve1B_2017 = do
    inCipherText <- readFile "./src/2017/1B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig cipherText
    putStrLn $ "1B: pt = " ++ (show $ fmap reverse pt)
    return ()

solve2A_2017::IO ()
solve2A_2017 = do
    inCipherText <- readFile "./src/2017/2A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars cipherText
    putStrLn $ "Frequencies are: " ++ (show fs)
    let pt = solveVig cipherText
    putStrLn $ "2A: pt = " ++ (show $ pt)
    return ()

solve2B_2017::IO ()
solve2B_2017 = do
    inCipherText <- readFile "./src/2017/2B.txt"
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
