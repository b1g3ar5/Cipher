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
        , solve3A_2017
        , solve3B_2017
        , solve4A_2017
        , solve4B_2017
        , solve5A_2017
        , solve5B_2017
        , solve6A_2017
        --, solve6B_2017
        --, solve7A_2016
        --, solve7B_2016
        --, solve8A_2016
        --, solve8B_2016
    ) where

import System.IO
import GHC.Exts (sortWith)
import Data.Char (isAlpha, chr)
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

import Utils
import Analysis
import Cribs
import Cipher
import Vignere
import Autokey
import Bifid
import Hill
import Quadgram (qscore, addWordScore, calcIx, qgram, readDoubles)
import System.IO.Unsafe (unsafePerformIO)


main_2017 :: IO ()
main_2017 = do
        --solve1A_2017 -- shift
        --solve1B_2017 -- shift
        -- solve2A_2017 -- square "CAIRO"
        --solve2B_2017 -- transpose
        --solve3A_2017 -- random crib? Mentions Polybius for 3B
        --solve3B_2017 -- Polybius with Roman numerals
        --solve4A_2017 -- random crib. Mentions Vignere for 4B
        --solve4B_2017 -- Vignere, "ARCANAIMPERII"
        --solve5A_2017 -- Says polyalphabetic nineteenth century
        --solve5B_2017
        --solve6A_2017
        solve6B_2017
        --solve7A_2017
        --solve7B_2017
        --solve8A_2017
        --solve8B_2017


stringTo32 :: String -> Int
stringTo32 = go 0
    where
        go n [] = n
        go n [x] = 2*n + if x == '1' then 1 else 0
        go n (x:xs) = go (2*n + if x == '1' then 1 else 0) xs


solve1A_2017::IO ()
solve1A_2017 = do
    inCipherText <- readFile "./src/2017/1A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let plain = solveShift cipherText
    putStrLn $ "1A pt = " ++ show plain
    return ()


process x = sortBy (flip compare) $ map swap $ toList $ count2freq $ countChars x

solve1B_2017::IO ()
solve1B_2017 = do
    inCipherText <- readFile "./src/2017/1B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let pt = solveShift cipherText
    putStrLn $ "1B: pt = " ++ show pt
    return ()

solve2A_2017::IO ()
solve2A_2017 = do
    inCipherText <- readFile "./src/2017/2A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = process cipherText
    let pCrib = "HARRYIAMVERYGRATEFULFORTHEFILEFOLLOWINGYOURTIPOFFSCDKBXZJ"
    let cCrib = "UCGGPVCZLOGPTGCJOSKYSDGJUOSVYOSDYYDMVBTPDKGJVEDSSHIRXANQW"

    let ret = unzip $ sort $ zip (L.nub pCrib) (L.nub cCrib)
    putStrLn $ "2A: crib = " ++ show ret

    let km1 = zeroMap `mappend` cribMap pCrib cCrib
    let pt2 = apply km1 cipherText
    putStrLn $ "2A: pt = " ++ pt2
    return ()

solve2B_2017::IO ()
solve2B_2017 = do
    inCipherText <- readFile "./src/2017/2B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = sortBy (flip compare) $ map swap $ toList $ count2freq $ countChars cipherText

    let cnk = chunksOf 274 cipherText
    let pt = concat $ L.transpose cnk
    putStrLn $ "2B: pt = " ++ show pt
    return ()

    return ()


solve3A_2017::IO ()
solve3A_2017 = do
    inCipherText <- readFile "./src/2017/3A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    --putStrLn $ "cipherText = " ++ cipherText
    let fs = process cipherText

    let pCrib = "MARYAMALLTHEBESTITHOUGHTWHATYOUSAIDINHOSPITALGIVENTHINKUNFINDBLACKJODIE"
    let cCrib = "ICROCICBBFZEJEYFGFZWMSZFAZCFOWMYCGXGPZWYDGFCBSGTEPFZGPUMPLGPXJBCQUNWXGE"
    let ret = unzip $ sort $ zip (L.nub pCrib) (L.nub cCrib)
    putStrLn $ "3A: crib = " ++ show ret
    let km1 = zeroMap `mappend` cribMap pCrib cCrib
    let pt = apply km1 cipherText
    putStrLn $ "3A: pt = " ++ pt
    return ()


readI :: String -> Int
readI = read

solve3B_2017::IO ()
solve3B_2017 = do
    inCipherText <- readFile "./src/2017/3B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    -- Change the Roman numerals to numbers
    let cCrib = "XLCDM"
    let pCrib = "12345"
    let km1 = zeroMap `mappend` cribMap pCrib cCrib
    let ct = apply km1 cipherText
    -- Read the numbers in pairs
    let ct1 = chunksOf 2 ct
    -- Replace with a random letter
    let cCrib = [ x:[y] | x <- "12345", y <- "12345"]
    let pCrib = "ABCDEFGHIKLMNOPQRSTUVWXYZ"
    let km2 = zeroMap `mappend` cribMap pCrib cCrib
    let ct2 = apply km2 ct1

    putStrLn $ "3B: ct2 = " ++ show ct2
    return ()


solve4A_2017::IO ()
solve4A_2017 = do
    inCipherText <- readFile "./src/2017/4A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let fs = process cipherText

    let pCrib = "HARRYALLTHEBESTJODIEMARYAMSUGGESTEDIGETINTOUCHASWEHAVECONFIRMEDBOOKGROUPQ"
    let cCrib = "UGXXVGIIKURARTKSCFNRBGXVGBTHQQRTKRFNQRKNYKCHZUGTPRUGORZCYENXBRFACCLQXCHMD"
    -- turns out to be:
    -- gaza frequens Libycum: duxit Karthago triumphum
    -- GAZFREQUNSLIBYCMDXTKHOPV
    let ret = unzip $ sort $ zip (L.nub pCrib) (L.nub cCrib)
    putStrLn $ "4A: crib = " ++ show ret
    let km1 = zeroMap `mappend` cribMap pCrib cCrib
    let pt2 = apply km1 cipherText
    putStrLn $ "4A: pt = " ++ pt2

    return ()

solve4B_2017::IO ()
solve4B_2017 = do
    -- The clue in 4A says 16th century french cipher
    inCipherText <- readFile "./src/2017/4B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let pt = solveVig cipherText
    putStrLn $ "4B: pt = " ++ show pt
    return ()

countRepeats :: String -> [Int]
countRepeats ct = go ct '$' []
  where
    go :: String -> Char -> [Int] -> [Int]
    go [] lastC ns = reverse ns
    go (x:xs) lastC [] = go xs x [1]
    go (x:xs) lastC m@(n:ns) = if x == lastC then
                                    go xs lastC $ n+(1::Int):ns
                                  else
                                    go xs x $ (1::Int):m


solve5A_2017::IO ()
solve5A_2017 = do
    inCipherText <- readFile "./src/2017/5A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let nn = 875
    let mm = 1577
    let ct  = take nn cipherText ++ drop (mm + nn) cipherText
    let ctUE  = take mm $ drop nn cipherText
    putStrLn $ "The length of the cipherNoUE is: " ++ show (length ct)
    putStrLn $ "The length of the justUE is: " ++ show (length ctUE)

    -- First the UE bit. It's 1577 long which is 19*83
    -- If we replace the EU with * and space and print out in a rectangle
    let pCrib = "* "
    let cCrib = "EU"
    let km1 = zeroMap `mappend` cribMap pCrib cCrib
    let pt = apply km1 ctUE
    let chunks = chunksOf 83 pt
    putStrLn $ unlines chunks
    -- It says ARCANA IMPERII


    --putStrLn $ "ct = " ++ ct

    let fs = process ct
    --putStrLn $ "\nFrequencies of ct are: " ++ show fs


    --let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    --putStrLn $ "\nTop bigrams are: " ++ show topBigrams
    --let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countTrigrams 3 ct
    --putStrLn $ "\nTop 3grams are: " ++ show top3grams

    let pCrib = "ETNIAHMARYAMOSVDUJGSELCUKPBZFW"
    let cCrib = "YRKFDBJDPWDJLQTOSGAQYICSHMEXZU"
    let km1 = zeroMap `mappend` cribMap pCrib cCrib
    let pt2 = apply km1 ct
    --putStrLn $ "\n5A: ct = " ++ ct
    putStrLn $ "\n5A: pt = " ++ pt2

    return ()


solve5B_2017::IO ()
solve5B_2017 = do
    -- The clue in 4A says 16th century french cipher
    inCipherText <- readFile "./src/2017/5B.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText

    let fs = process cipherText
    putStrLn $ "\nFrequencies of cipherText are: " ++ show fs
    let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 cipherText
    putStrLn $ "\nTop bigrams are: " ++ show topBigrams
    let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countTrigrams 3 cipherText
    putStrLn $ "\nTop 3grams are: " ++ show top3grams

-- ON HIS POURNAL DATED THE ODES OF UCTOBER IN THE YEAR OF THE CONSULSHIPS OF
-- IAECELIUS ZULLIUS IAPITO VOMPONIANUS VLOTIUS LIRMUS AND MAIUS IORNELIUS MALLICANUS
-- GGRICOLA WROTE ZHE MYSTERY OF THE BATTLE AT IAMULODONUMIS AT LAST SOLVED
-- IALGACUS MAY BE A HARBARIAN NOW BUT HE WAS A XOMAN CITIZEN THEN WHO BETRAYED
-- US ALL FOR LOVE OF A BARBARIAN O THAS TAKEN ALL MY SKILLS AS A LEADER OF
-- MEN TO KEEP HIM ALIVE ZHE REGIONNAIRES SPEND THEIR EVENINGS DESIGNING NEW
-- AND CRUEL WAYS TO EXECUTE HIM IN REVENGE FOR THE SHAME HE BROUGHT UPON US
-- BUT HIS LIFE IS PRECIOUS OT IS THE ONLY CARD LEFT TO PLAY IN OUR SEARCH FOR
-- SALVATION AND THE RETURN OF THE STOLEN GQUILAEOF WE CAN ALSO RECOVER THE
-- IODEX THEN PERHAPS ITS LOSS CAN BE CONCEALED AND OUR LIVES WILL BE SPARED
-- XELEASING THE XOMAN TRAITOR IALGACUS MUST HAVE STUCK IN THE PROUD GGRICOLAS
-- THROAT BUT HE MADE A PACT WITH THE REMAINING IALEDONII AND TRAVELLED NORTH
-- TO EXCHANGE THE PRISONER FOR THE GQUILAE AND THE IODEX HUT THE CUNNING
-- IALEDONIAN TRIBESMAN SET ANOTHER TRAP AND PRESENTED GGRICOLA WITH A FORGERY
-- CUNNINGLY ASSEMBLED WITH PAGES FROM THE BOOKS STOLEN WHEN THE TRIBE RANSACKED
-- SONSMRAUPIUS LOR TOO LONG THE SONS OF XOME HAD UNDERESTIMATED
-- THE PEOPLE IN HRITANNIA AND WHILE THE GQUILA OF THE REGION HAD BEEN RESTORED
-- BY THE EXCHANGE THEIR HONOUR WAS NOT GGRICOLA FACED A RETURN TO XOME
-- HUMILIATION AND ALMOST CERTAIN DEATH ZHE SIXTH CHAPTER OF MY TALE OF WOE
-- IS GUARDED BY LIGHTNING BULL AND OAK


    let pt = solveBeaufort cipherText
    putStrLn $ "\n5B: pt = " ++ show (fst pt, snd pt)
    return ()



solve6A_2017::IO ()
solve6A_2017 = do
    inCipherText <- readFile "./src/2017/6A.txt"
    let cipherText  = clean isAlpha $ concat $ lines inCipherText
    let nn = 875
    let mm = 1577

    let pt = solveVig cipherText
    putStrLn $ "6A: pt = " ++ show pt

-- c, 7 -> i = +6
-- g, 1 -> m = +6
-- m, 111 -> s = +6
-- d, 13 -> j = +6
-- i, 20 -> o = +6
-- m, 53 -> s = +6
-- n, 17, 186 -> t = +6
-- u, 179 = a -> = +6

-- 1, 7, 13, 17, 20, 53, 111, 179
-- 0, 6, 12, 16, 19, 52, 110, 178

{-
GARYAMCFOUNDDODIEATILYMPIAANDSHEHASCHAPTERSIXALREADYMHEHADBEENLEADINGTHEGCXUMOPERATIVESONAWILDGOOSECHASEAROUNDMELCUKMAKINGALOTOFNOISEABOUTLOOKINGFORTHEMISSINGCHAPTERATTHENEMPLEOFURTEMISNHATGAVEMETIMETORETRIEVE
IT FROM DODIES FRIEND AT THE VRITISH GUSEUM WHICH HAS A COLLECTION OF ARTEFACTS FROM THE NEMPLE DODIE SAYS SHE KNEW WE WOULD
FIGURE OUT WHERE TO GO NEXT NHE CLUE WAS IN THE LOCATIONS NHE FIRST ONE WAS AT THE AREAT JYRAMID THE SECOND AT THE FORT OF KAITBAY
WHICH WAS BUILT FROM THE RUINS OF THE FIGHT HOUSE AT ULEXANDRIA AND THE THIRD ON LHODES AMONG THE RUINS OF THE WOLOSSUS NHE ENIGHTS
CASTLE  AT VODRUM WAS BUILT FROM THE REMAINS OF THE GAUSOLEUM AT BALICARNASSUS HENCE THE GRAVE TASK OF GUARDING THE BOOK MO ALL FIVE
CHAPTERS WERE FOUND AT THE SITE OF ONE OF THE MEVEN QONDERS OF THE ANCIENT WORLD NHE ONLY REMAINING LOCATIONS ARE THE MTATUE OF
TEUS AT ILYMPIA AND THE BANGING AARDENS OF VABYLON AND NOONE HAS ANY IDEA WHERE THE GARDENS MIGHT HAVE BEEN SO THE ONLY PLACE WE
COULD GO NEXT IS ILYMPIA NHE CLUE AT THE END OF CHAPTER FIVE POINTS STRAIGHT THERE SINCE LIGHTING BULL AND OAK ARE ALL SYMBOLS OF
TEUS QE SHOULD BE SAFE HERE BECAUSE DODIES NETWORK HAS SENT HER ATTACKERS ON TO FONDON BY LAYING A LONG TRAIL OF FORGED DOCUMENTS
REVEALING THE LOCATION OF CHAPTER FIVE NHAT BOUGHT US ENOUGH TIME TO LOCATE AND DECIPHER CHAPTER SIX QE HAD BEEN THINKING ABOUT
THE EVOLUTION OF THE CMPERIAL WIPHERS NACITUS USED BOTH THE PIGENERE AND VEAUFORT CIPHERS WHICH ARE POLYALPHABETIC VERSIONS OF
THE WAESAR SHIFT AND AT FIRST WE ASSUMED THAT CHAPTER SIX WOULD BE ENCRYPTED THE SAME WAY QE WERE ALMOST RIGHT GEANWHILE
DODIES UNEXPECTED TALENT FOR FORGERY HAS GIVEN ME AN IDEA C THINK WE MIGHT BE ABLE TO TURN OUR ENEMIES ON ONE ANOTHER BY
EXPLOITING HER GIFT BUT C KNOW HER CURRENT FOCUS IS FIGURING OUT WHERE ON EARTH THE SEVENTH WONDER MIGHT BE


-- GARYAM C FOUND DODIE AT ILYMPIA AND SHE HAS CHAPTER SIX ALREADY MHE HAD BEEN LEADING THE GCXUM OPERATIVES ON A WILD GOOSE CHASE
-- AROUND MELCUK MAKING A LOT OF NOISE ABOUT LOOKING FOR THE MISSING CHAPTER AT THE NEMPLE OF URTEMIS NHAT GAVE ME TIME TO RETRIEVE
-- IT FROM DODIES FRIEND AT THE VRITISH GUSEUM WHICH HAS A COLLECTION OF ARTEFACTS FROM THE NEMPLE DODIE SAYS SHE KNEW WE WOULD
-- FIGURE OUT WHERE TO GO NEXT NHE CLUE WAS IN THE LOCATIONS NHE FIRST ONE WAS AT THE AREAT JYRAMID THE SECOND AT THE FORT OF KAITBAY
-- WHICH WAS BUILT FROM THE RUINS OF THE FIGHT HOUSE AT ULEXANDRIA AND THE THIRD ON LHODES AMONG THE RUINS OF THE WOLOSSUS NHE ENIGHTS
-- CASTLE  AT VODRUM WAS BUILT FROM THE REMAINS OF THE GAUSOLEUM AT BALICARNASSUS HENCE THE GRAVE TASK OF GUARDING THE BOOK MO ALL FIVE
-- CHAPTERS WERE FOUND AT THE SITE OF ONE OF THE MEVEN QONDERS OF THE ANCIENT WORLD NHE ONLY REMAINING LOCATIONS ARE THE MTATUE OF
-- TEUS AT ILYMPIA AND THE BANGING AARDENS OF VABYLON AND NOONE HAS ANY IDEA WHERE THE GARDENS MIGHT HAVE BEEN SO THE ONLY PLACE WE
-- COULD GO NEXT IS ILYMPIA NHE CLUE AT THE END OF CHAPTER FIVE POINTS STRAIGHT THERE SINCE LIGHTING BULL AND OAK ARE ALL SYMBOLS OF
-- TEUS QE SHOULD BE SAFE HERE BECAUSE DODIES NETWORK HAS SENT HER ATTACKERS ON TO FONDON BY LAYING A LONG TRAIL OF FORGED DOCUMENTS
-- REVEALING THE LOCATION OF CHAPTER FIVE NHAT BOUGHT US ENOUGH TIME TO LOCATE AND DECIPHER CHAPTER SIX QE HAD BEEN THINKING ABOUT
-- THE EVOLUTION OF THE CMPERIAL WIPHERS NACITUS USED BOTH THE PIGENERE AND VEAUFORT CIPHERS WHICH ARE POLYALPHABETIC VERSIONS OF
-- THE WAESAR SHIFT AND AT FIRST WE ASSUMED THAT CHAPTER SIX WOULD BE ENCRYPTED THE SAME WAY QE WERE ALMOST RIGHT GEANWHILE
-- DODIES UNEXPECTED TALENT FOR FORGERY HAS GIVEN ME AN IDEA C THINK WE MIGHT BE ABLE TO TURN OUR ENEMIES ON ONE ANOTHER BY
-- EXPLOITING HER GIFT BUT C KNOW HER CURRENT FOCUS IS FIGURING OUT WHERE ON EARTH THE SEVENTH WONDER MIGHT BE
-}
    return ()


solve6B_2017::IO ()
solve6B_2017 = do
    -- The clue in 4A says 16th century french cipher
    inCipherText <- readFile "./src/2017/6B.txt"
    let ct  = clean isAlpha $ concat $ lines inCipherText

    --let fs = process ct
    --putStrLn $ "\nFrequencies of cipherText are:\n" ++ (concatMap (\t -> show t ++ "\n") fs)
    --let topBigrams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countBigrams 1 ct
    --putStrLn $ "\nTop bigrams are: " ++ show topBigrams
    --let top3grams = take 6 $ reverse $ sortWith snd $ toList $ count2freq $ loseZeros $ countTrigrams 3 ct
    --putStrLn $ "\nTop 3grams are: " ++ show top3grams

    -- Try AutokeyCipher
    let pts = fmap (\i -> decipher (AutokeyCipher $ replicate i 'A')  ct) [1..10]
    let fss = fmap quadgramScore pts
    putStrLn $ "\nquadgram scores are:\n" ++ show fss


    --let pts = fmap (\c -> decipher (AutokeyCipher $ [c] ++ replicate 6 'A')  ct) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    --let fss = fmap quadgramScore $ take 1 pts
    --mapM_ (\fs -> putStrLn $ "\nquadgram scores are:\n" ++ (concatMap (\t -> show t ++ "\n") fs)) $ zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" fss


    return ()
