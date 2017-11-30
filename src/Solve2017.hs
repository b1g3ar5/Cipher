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
import Data.Char (isAlpha, chr, toUpper)
import Data.Map (toList)
import Data.List (map, sortBy, concatMap, sort, transpose, intersperse)
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
        --solve2A_2017 -- square "CAIRO"
        --solve2B_2017 -- transpose
        --solve3A_2017 -- random crib? Mentions Polybius for 3B
        --solve3B_2017 -- Polybius with Roman numerals
        --solve4A_2017 -- random crib. Mentions Vignere for 4B
        --solve4B_2017 -- Vignere, "ARCANAIMPERII"
        --solve5A_2017 -- square DECOYZABFGHIJKLMPQRSTUWX". Says polyalphabetic nineteenth century for 5B.
        --solve5B_2017 -- Beaufort "TCRTGTLHEPCLL"
        --solve6A_2017 -- Vignere "FKAY", with some first letters of words moved 6 letters forward
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
    let ret = unzip $ sort $ zip (L.nub pCrib) (L.nub cCrib)
    putStrLn $ "\n5A: crib = " ++ show ret
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

-- ON HIS JOURNAL DATED THE IDES OF OCTOBER IN THE YEAR OF THE CONSULSHIPS OF
-- CAECELIUS TULLIUS CAPITO POMPONIANUS PLOTIUS FIRMUS AND GAIUS CORNELIUS GALLICANUS
-- AGRICOLA WROTE THE MYSTERY OF THE BATTLE AT CAMULODONUM IS AT LAST SOLVED
-- CALGACUS MAY BE A BARBARIAN NOW BUT HE WAS A ROMAN CITIZEN THEN WHO BETRAYED
-- US ALL FOR LOVE OF A BARBARIAN IT HAS TAKEN ALL MY SKILLS AS A LEADER OF
-- MEN TO KEEP HIM ALIVE THE LEGIONNAIRES SPEND THEIR EVENINGS DESIGNING NEW
-- AND CRUEL WAYS TO EXECUTE HIM IN REVENGE FOR THE SHAME HE BROUGHT UPON US
-- BUT HIS LIFE IS PRECIOUS OT IS THE ONLY CARD LEFT TO PLAY IN OUR SEARCH FOR
-- SALVATION AND THE RETURN OF THE STOLEN AQUILAE IF WE CAN ALSO RECOVER THE
-- CODEX THEN PERHAPS ITS LOSS CAN BE CONCEALED AND OUR LIVES WILL BE SPARED
-- RELEASING THE ROMAN TRAITOR CALGACUS MUST HAVE STUCK IN THE PROUD AGRICOLAS
-- THROAT BUT HE MADE A PACT WITH THE REMAINING CALEDONII AND TRAVELLED NORTH
-- TO EXCHANGE THE PRISONER FOR THE AQUILAE AND THE CODEX BUT THE CUNNING
-- CALEDONIAN TRIBESMAN SET ANOTHER TRAP AND PRESENTED AGRICOLA WITH A FORGERY
-- CUNNINGLY ASSEMBLED WITH PAGES FROM THE BOOKS STOLEN WHEN THE TRIBE RANSACKED
-- MONS GRAUPIUS FOR TOO LONG THE SONS OF ROME HAD UNDERESTIMATED
-- THE PEOPLE IN BRITANNIA AND WHILE THE AQUILA OF THE REGION HAD BEEN RESTORED
-- BY THE EXCHANGE THEIR HONOUR WAS NOT AGRICOLA FACED A RETURN TO ROME
-- HUMILIATION AND ALMOST CERTAIN DEATH THE SIXTH CHAPTER OF MY TALE OF WOE
-- IS GUARDED BY LIGHTNING BULL AND OAK


    let pt = solveBeaufort cipherText
    putStrLn $ "\n5B: pt = " ++ show pt
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


-- MARYAM I FOUND JODIE AT OLYMPIA AND SHE HAS CHAPTER SIX ALREADY SHE HAD BEEN LEADING THE GCXUM OPERATIVES ON A WILD GOOSE CHASE
-- AROUND SELCUK MAKING A LOT OF NOISE ABOUT LOOKING FOR THE MISSING CHAPTER AT THE TEMPLE OF ARTEMIS THAT GAVE ME TIME TO RETRIEVE
-- IT FROM JODIES FRIEND AT THE BRITISH MUSEUM WHICH HAS A COLLECTION OF ARTEFACTS FROM THE TEMPLE JODIE SAYS SHE KNEW WE WOULD
-- FIGURE OUT WHERE TO GO NEXT NHE CLUE WAS IN THE LOCATIONS NHE FIRST ONE WAS AT THE GREAT PYRAMID THE SECOND AT THE FORT OF RAITBAY
-- WHICH WAS BUILT FROM THE RUINS OF THE FIGHT HOUSE AT ALEXANDRIA AND THE THIRD ON RHODES AMONG THE RUINS OF THE COLOSSUS THE KNIGHTS
-- CASTLE  AT BODRUM WAS BUILT FROM THE REMAINS OF THE MAUSOLEUM AT HALICARNASSUS HENCE THE GRAVE TASK OF GUARDING THE BOOK MO ALL FIVE
-- CHAPTERS WERE FOUND AT THE SITE OF ONE OF THE SEVEN WONDERS OF THE ANCIENT WORLD THE ONLY REMAINING LOCATIONS ARE THE STATUE OF
-- ZEUS AT OLYMPIA AND THE HANGING GARDENS OF BABYLON AND NOONE HAS ANY IDEA WHERE THE GARDENS MIGHT HAVE BEEN SO THE ONLY PLACE WE
-- COULD GO NEXT IS OLYMPIA THE CLUE AT THE END OF CHAPTER FIVE POINTS STRAIGHT THERE SINCE LIGHTING BULL AND OAK ARE ALL SYMBOLS OF
-- ZEUS WE SHOULD BE SAFE HERE BECAUSE JODIES NETWORK HAS SENT HER ATTACKERS ON TO LONDON BY LAYING A LONG TRAIL OF FORGED DOCUMENTS
-- REVEALING THE LOCATION OF CHAPTER FIVE THAT BOUGHT US ENOUGH TIME TO LOCATE AND DECIPHER CHAPTER SIX QE HAD BEEN THINKING ABOUT
-- THE EVOLUTION OF THE IMPERIAL CIPHERS TACITUS USED BOTH THE VIGENERE AND BEAUFORT CIPHERS WHICH ARE POLYALPHABETIC VERSIONS OF
-- THE CAESAR SHIFT AND AT FIRST WE ASSUMED THAT CHAPTER SIX WOULD BE ENCRYPTED THE SAME WAY WE WERE ALMOST RIGHT MEANWHILE
-- JODIES UNEXPECTED TALENT FOR FORGERY HAS GIVEN ME AN IDEA I THINK WE MIGHT BE ABLE TO TURN OUR ENEMIES ON ONE ANOTHER BY
-- EXPLOITING HER GIFT BUT I KNOW HER CURRENT FOCUS IS FIGURING OUT WHERE ON EARTH THE SEVENTH WONDER MIGHT BE
-}
    return ()


solve6B_2017::IO ()
solve6B_2017 = do
    -- 6A says almost Vignere or Beaufort...

    -- OFFICIAL HINT: For this challenge you have double the time, so assume double the difficulty.

    -- OFFICIAL CLUE: The following has just come in from Maryam. I have taken the liberty to decode it for you.
    -- Good luck. “The ioc was pretty conclusive about the period of the cipher, but frequency analysis didn’t give
    -- us a Caesar shift for each column. It was Jodie who pointed out that if the Romans were using lookup tables
    -- from the Codex then they didn’t have to rely entirely on Caesar shifts in each column so we had to do the hard
    -- work of figuring out each of the substitutions used.”

    -- OFFICIAL HINT: You may be thinking what a fine mess this is, but half of this you have done before.

    -- OFFICIAL CLUE: Hopefully by now you have the period, you have the ciphers, and you just need a key word.
    -- Unfortunately, this is related to a farmer’s death.

    -- So, 2 ciphers
    -- ioc give period of 15
    -- farmers death is AGRICOLAE MORTEM in Latin which is 15 letters - could be the key word?
    -- "What a fine mess" is Laurel and Hardy - not sure what relevence this is

    inCipherText <- readFile "./src/2017/6B.txt"
    let cipherText  = fmap toUpper $ clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "\n6B length of ct: " ++ show (length cipherText)

    let cipherCount = countChars cipherText
    let cipherFreq = count2freq cipherCount
    let keyICs = map (`splitIC` cipherText) [1..20]
    let bestKeySize = ixOfMin (map (\d->(d-65)**2.0) keyICs) +1
    putStrLn $ "\n6B keyICs: " ++ show keyICs
    putStrLn $ "\n6B bestKeySize: " ++ show bestKeySize

    let cts = splitText bestKeySize cipherText
    putStrLn $ "lengths are: " ++ show (fmap length cts)
    let splitFreqs = fmap process cts
    mapM_ (\fs -> putStrLn $ "\n6B splitFss: " ++ show (take 6 fs) ++ "\n") splitFreqs

    let cribs = fmap ct2initialCrib cts
    let pts = zipWith apply cribs cts

    putStrLn $ "6B: ct = " ++ show (concat $ transpose pts)


    let key = "AGRICOLAEMORTEM"
    --let ptVig = decipher (BeaufortCipher key) cipherText
    --let ptBef = decipher (VigCipher key) cipherText
    --putStrLn $ "\n6B ptVig: " ++ show (ptVig)
    --putStrLn $ "\n6B ptBef: " ++ show (ptBef)

    return ()


ct2initialCrib :: String -> KeyMap Char
ct2initialCrib ct = zeroMap `mappend` cribMap pCrib cCrib
  where
    fs = process ct
    pCrib = "ET"
    cCrib = fmap snd $ take 2 fs
