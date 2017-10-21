{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Solve2016
    (
        stringTo32
        , main_2016
        , solve1A_2016
        , solve1B_2016
        , solve2A_2016
        , solve2B_2016
        , solve3A_2016
        , solve3B_2016
        , solve4A_2016
        , solve4B_2016
        , solve5A_2016
        , solve5B_2016
        , solve6A_2016
        , solve6B_2016
        , solve7A_2016
        , solve7B_2016
        , solve8A_2016
        , solve8B_2016
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

solve7A_2016::IO ()
solve7A_2016 = do
    inCipherText <- readFile "./src/2016/7A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    --putStrLn $ "============================================================================="
    --putStrLn $ "7A: cipherText = " ++ ct
    let ctFreqs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    --putStrLn $ "7A: Frequencies are: " ++ (show ctFreqs)
    let ctTopBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    --putStrLn $ "7A: Top bigrams are: " ++ (show ctTopBigrams)
    let ctTop3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    --putStrLn $ "7A: Top 3grams are: " ++ (show ctTop3grams)
    let pt = solveVig ct
    let ptFreqs = reverse $ sort $ map swap $ toList $ count2freq $ countChars $ snd pt
    --putStrLn $ "7A: Frequencies are: " ++ (show ptFreqs)
    let ptTopBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 $ snd pt
    --putStrLn $ "7A: Top bigrams are: " ++ (show ptTopBigrams)
    let ptTop3grams = take 6 $ reverse $ sortWith snd $ toList $ loseZeros $ countNgrams 3 $ snd pt
    --putStrLn $ "7A: Top 3grams are: " ++ (show ptTop3grams)
    putStrLn $ "7A: plainText = " ++ (show $ (fst pt, reverse $ snd pt))
    return ()

solve7B_2016::IO ()
solve7B_2016 = do
    -- This is an AMSCO cipher
    -- It was cracked by trying all the keys (key length known to be 6
    -- with a [1,2] and a [2,1] start. The resulting plain texts with "THE" as the
    -- most common trigram were filtered out and inspected. Only one of them was proper text
{-
import GHC.Exts (sortWith)
import Data.Char (isAlpha)
import Data.Map (toList)
import Data.List as L
import qualified Data.Set as S (size)
import Data.Tuple (swap)
import Numeric (showFFloat)
import Quadgram
-}

    inCipherText <- readFile "./src/2016/7B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    let (pt, code) = head $ solveAmsco False 6 ct
    --putStrLn $ "============================================================================="
    --putStrLn $ "7B: cipherText = " ++ ct
    --putStrLn $ "7B: length of ct is " ++ (show $ length ct)
    let ctFreqs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    --putStrLn $ "7B: ct frequencies are: " ++ (show $ map (\t -> (snd t, showFFloat (Just 3) (fst t) "")) ctFreqs)
    let ctTopBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    --putStrLn $ "7B: ct top bigrams are: " ++ (show ctTopBigrams)
    let ctTop3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    --putStrLn $ "7B: ct top 3grams are: " ++ (show ctTop3grams)
    let bgs = map (\n-> countBigrams n ct) [1..20]
    let bgvar = map var bgs
    --putStrLn $ show "7B: bgvar = " ++ (show bgvar)
    --putStrLn "There is a peak for bg!!1, bigrams with space 2 - which indicates a period of 4, ie. 4 letters coded at a time"
    let sbg = filter (\t -> snd t > 0.005) $ reverse $ sortWith snd $ toList (count2freq $ loseZeros $ bgs!!1)
    --putStrLn $ "7B: filtered corted bg!!1 is " ++ (show $ sbg)
    --
    --putStrLn $ "7B: The actual counts of bg!!1 are " ++ (show $ reverse $ sortWith snd $ toList $ bitoMap $loseZeros $ bgs!!1)
    let r1 = applyCrib (Crib (Just 0) Nothing "MART") ct
    let r2 = applyCrib (Crib (Just 0) Nothing "MARTINWE") ct
    let rs = reverse $ sortWith (S.size) $ consolidate $ r1 ++ r2
    --putStrLn $ "Rules are: " ++ (show r1)
    --putStrLn $ "Rules are: " ++ (show r2)
    --putStrLn $ "Rules are: " ++ (show rs)
    --let gs = grids rs
    --putStrLn $ "Grids are: " ++ (show $ take 3 gs)
    let startGrid = bifidAlphabet
    let newGrid = "LIGOABCDEFHKMNPQRSTUVWXYZ"
    let bc = BifidPeriodCipher 4 newGrid
    let pt = decipher bc ct
    putStrLn $ "7B: pt = " ++ pt
    --let bstest = pack "ABCDEFGHIJKL"
    --putStrLn $ " bstest is: " ++ (show bstest)
    --let ixs = generate ((BS.length bstest)-4) id
    --putStrLn $ " ixs is: " ++ (show ixs)
    --let ci = calcIx $ BS.take 4 (BS.drop 0 $ pack ct)
    --putStrLn $ " ci: " ++ (show ci)
    --putStrLn $ " length of qgram is: " ++ (show $ V.length qgram)
    --let aws = addWordScore bstest 0 0
    --putStrLn $ " aws: " ++ (show aws)
    --let sctest = qscore bstest
    --putStrLn $ " Score is: " ++ (show sctest)
    --let sc = qscore $ pack pt
    --putStrLn $ " Score is: " ++ (show sc)
    --let sc = scoreGrid startGrid ct
    --putStrLn $ "Grid " ++ startGrid ++ " scores: " ++ (show sc)

    return ()

solve8A_2016::IO ()
solve8A_2016 = do
    inCipherText <- readFile "./src/2016/8A.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "8A: cipherText = " ++ ct
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars ct
    putStrLn $ "8A: Frequencies are: " ++ (show fs)
    let ctTopBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    putStrLn $ "8A: Top bigrams are: " ++ (show ctTopBigrams)
    let ctTop3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 ct
    putStrLn $ "8A: Top 3grams are: " ++ (show ctTop3grams)

    -- These are cribs for Harry/charlie at the start and end of the pt
    --let pCribStart1 = (2><2) [7, 0, 17, 17] :: Matrix (Z ./. 26) --HARR, from pt
    let pCribStart2 = (2><2) [2, 7, 0, 17] :: Matrix (Z ./. 26) --CHAR, from pt
    --let pCribEnd1   = (2><2) [0, 17, 17, 24] :: Matrix (Z ./. 26) --ARRY, from pt
    --let pCribEnd2 = (2><2) [17, 11, 8, 4] :: Matrix (Z ./. 26) --RLIE, from pt
    let cCribStart = (2><2) [18,21,6,5] :: Matrix (Z ./. 26) --SVGF, from pt
    --let cCribEnd = (2><2) [15, 15,10,4] :: Matrix (Z ./. 26) --PPKE, from pt

    -- This is a crib for the most likely bigrams to be TH and HE
    let pCribTHHE = (2><2) [19, 7, 7, 4] :: Matrix (Z ./. 26) --THHE, from pt
    let cCribTHHE = (2><2) [7, 0, 3, 17] :: Matrix (Z ./. 26) --HADR, from pt

    -- Of these cribs the following 2 have an identical implied key matrix
    let m2 = mySolve cCribStart pCribStart2
    let m6 = mySolve cCribTHHE pCribTHHE
    putStrLn $ "m2 is: " ++ (show m2)
    putStrLn $ "m6 is: " ++ (show m6)

    -- This is the implied key
    let key = (2><2) [25,1,22,23] :: Matrix (Z ./. 26)
    let cp = HillCipher key
    -- Whoops, I got the cribs the wrong way round - so cipher not decipher
    let pt = cipher cp ct
    putStrLn pt


count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

{-
102
002
002
012
002
1112
0002
1002
0102
1012
1002
1102
0112
0112
0002
00101112
10100112
11000112
11000002
00000012
01112
00012
00012
01012
11002
01012
00012
00112
01102
11002
12
02
02
02
02
0112
1102
1012
0002
0012
002
002
012
112
102
00102
00102
01002
01002
00002
111112
111012
010012
000002
010002
0111002
0000002
101110
-}

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (L.group (L.sort list))

solve8B_2016::IO ()
solve8B_2016 = do
    inCipherText <- readFile "./src/2016/8B.txt"
    let ct  = clean (\c -> c /= ' ') $ concat $ lines inCipherText
    --putStrLn $ "8B: cipherText = " ++ ct
    --putStrLn $ "8B: The length of the cipherText = " ++ (show $ length ct)
    let fs = reverse $ sort $ map swap $ (filter (\i-> ((snd i) /= 0))) $ toList $ countChars ct
    --putStrLn $ "8B: Frequencies are: " ++ (show fs)
    --putStrLn "The factors are 5, 23, 283. So that means 23*283=6509 characters"
    --putStrLn "if we assume that it's binary (using [01]) with 2 haviSo, we need to split into chunks of 5, then transpose each one and we might get lettersng some other meaning?!"
    --putStrLn "This seems sensible with 2 having a much lower frequency..."
    let wds = wordsOn (=='2') ct
    let ls = map length wds
    --putStrLn $ "The maximum letter length is: " ++ (show $ maximum ls)
    --putStrLn $ "The minimum letter length is: " ++ (show $ minimum ls)
    --putStrLn $ "The mean length is: " ++ (show $ (fromIntegral $ sum ls)/(fromIntegral $ length ls))
    --putStrLn $ "The frequencies are: " ++ (show $ map (\x -> count x ls) [1..14])
    --putStrLn "Looking at the lengths, they og in groups of 5 - this fits with 5 binary"
    --putStrLn "chars for a letter."
    --putStrLn "So, we need to split into chunks of 5, then transpose each one and we might get letters"

    let wd5 = chunksOf 5 wds
    let twd5 = map L.transpose wd5
    --putStrLn $ "The first 20 chars are: " ++ (show $ take 20 twd5)
    --putStrLn $ "Lets look at the length 3 and see if we can find 'the'"
    --putStrLn $ "The first 100 chars are:\n" ++ (show $ take 100 $ L.sort $ L.filter (\w -> (length w) == 3) twd5)
    --putStrLn $ "The first 20 words are:\n" ++ (show $ take 20 $ reverse $ sortWith fst $ frequency twd5)
    --putStrLn $ "The first 20 chars are:\n" ++ (show $ take 20 $ reverse $ sortWith fst $ frequency $ concat twd5)

    --putStrLn $ "All this shows that this process is working, so lets just convert the binary to int and use vig"

    let cs = map (\w -> nchr $ (read $ [w!!0])*2^4+(read $ [w!!1])*2^3+(read $ [w!!2])*2^2+(read $ [w!!3])*2+(read $ [w!!4])) $ concat twd5
    putStrLn $ "The new cs is: " ++ (show cs)
    let fs = reverse $ sort $ map swap $ toList $ count2freq $ countChars cs
    --putStrLn $ "8B: Frequencies are: " ++ (show fs)
    let ctTopBigrams = take 10 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 cs
    --putStrLn $ "8B: Top bigrams are: " ++ (show ctTopBigrams)
    let ctTop3grams = take 10 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countNgrams 3 cs
    --putStrLn $ "8B: Top 3grams are: " ++ (show ctTop3grams)
    let pCrib = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_[]^"
    let cCrib = "YDWAGC^TQIEUM]SK[O_BRJZFVNxxxx"
    let km = cribMap "" ""
    let km1 = km `mappend` (cribMap pCrib cCrib)
    let pt = apply km1 cs
    putStrLn $ "8B: pt = " ++ (show $ pt)
    return ()
