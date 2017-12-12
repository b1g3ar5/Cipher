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
        , solve6B_2017
        , solve7A_2017
        , solve7B_2017
        --, solve8A_2017
        --, solve8B_2017
    ) where

import System.IO
import GHC.Exts (sortWith)
import Data.Char (isAlpha, chr, toUpper)
import Data.Map (toList)
import Data.List (map, sortBy, concatMap, sort, transpose, intersperse)
import Data.List.Split (splitOn)
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
import Stats
import Cribs
import Cipher
import Vignere
import Transposition
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
        --solve3B_2017 -- Polybius (ie. coordinates in a 5'5 square) with Roman numerals
        --solve4A_2017 -- random crib. Mentions Vignere for 4B
        --solve4B_2017 -- Vignere "ARCANAIMPERII"
        --solve5A_2017 -- square DECOYZABFGHIJKLMPQRSTUWX". Says polyalphabetic nineteenth century for 5B.
        --solve5B_2017 -- Beaufort "TCRTGTLHEPCLL"
        --solve6A_2017 -- Vignere "FKAY", with some first letters of words moved 6 letters forward
        --solve6B_2017 -- Vignere with Affine "AGRICOLAMORTEM" (instead of shift)
        --solve7A_2017 -- Vignere "HANGINGGARDENS"
        solve7B_2017
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

    -- So, let's try Vignere and Affine alternate...

    inCipherText <- readFile "./src/2017/6B.txt"
    let cipherText  = fmap toUpper $ clean isAlpha $ concat $ lines inCipherText
    putStrLn $ "\n6B length of ct: " ++ show (length cipherText)

    printStats cipherText
    let cipherCount = countChars cipherText
    let cipherFreq = count2freq cipherCount
    let keyICs = map (`splitIC` cipherText) [1..20]
    let bestKeySize = ixOfMin (map (\d->(d-65)**2.0) keyICs) +1
    putStrLn $ "\n6B keyICs: " ++ show keyICs
    putStrLn $ "\n6B bestKeySize: " ++ show bestKeySize

    let cts = splitText bestKeySize cipherText
    putStrLn $ "lengths are: " ++ show (fmap length cts)
    let splitFreqs = fmap process cts

    let pts = fmap solveAffine cts
    let key = fmap (\t -> nchr $ snd $ fst t) pts
    putStrLn $ "\n6B key is: " ++ show key
    putStrLn $ "\n6B ptAff: " ++ show (concat $ transpose $ fmap snd pts)

    return ()


ct2initialCrib :: String -> KeyMap Char
ct2initialCrib ct = zeroMap `mappend` cribMap pCrib cCrib
  where
    fs = process ct
    pCrib = "ET"
    cCrib = fmap snd $ take 2 fs


solve7A_2017::IO ()
solve7A_2017 = do
  inCipherText <- readFile "./src/2017/7A.txt"
  let cipherText  = clean isAlpha $ concat $ lines inCipherText

  let pt = snd $ solveVig cipherText
  putStrLn $ "7A: pt = " ++ show pt
-- WELL THAT WAS A SURPRISE AND QUITE A RELIEF AS I SAID BEFORE NO ONE REALLY KNOWS WHERE THE
-- HANGING GARDENS OF BABYLON WERE LOCATED AND I DONT THINK WE WOULD HAVE HAD A CHANCE OF FINDING
-- THE NEXT CHAPTER IF THAT WAS WHERE IT WAS BURIED BUT IT SEEMS THAT TACITUS HAD A BETTER
-- IDEA THE BABYLONIAN GODDESS OF LOVE AND WAR WAS ISHTAR AND THE ISHTAR GATE FROM BABYLON WAS
-- ONE OF THE ORIGINAL SEVEN WONDERS OF THE WORLD IT WAS LATER REPLACED BY THE LIGHTHOUSE OF
-- ALEXANDRIA SO I AM NOT SURE WHY TACITUS USED BOTH BUT MAYBE BECAUSE HE DIDNT KNOW WHERE
-- THE HANGING GARDENS WERE EITHER OR MAYBE HE JUST WANTED TO CONFUSE THE UNINITIATED AND TO
-- ADD AN EXTRA LAYER OF SECRECY VIA CONFUSION A BIT LIKE HE DOES BY PILING UP CIPHERS IN THESE
-- LATER CHAPTERS ANYWAY WE ARE IN LUCK THE ISHTAR GATE NOW LIVES IN THE PERGAMON MUSEUM IN
-- BERLIN AND I HAPPEN TO HAVE A PASS TO THE FULL COLLECTION I AM NOT SURE HOW THEY WILL FEEL
-- ABOUT US DISMANTLING IT TO TRY TO FIND CHAPTER SEVEN BUT IF WE EXPLAIN WHAT IS IN IT I
-- SUSPECT THE CURATORS CURIOSITY WILL OVERCOME HIS NATURAL PROTECTIVENESS PERHAPS THERE WE
-- WILL FINALLY UNLOCK THE SECRET OF THE IXTH LEGION THAT LEAVES THE QUESTION OF HOW WE DEAL
-- WITH MIDAS AND MARYAM HAS A CLEVER IDEA WE SHOULD LET THE COLLECTOR DEAL WITH THEM THE
-- RUSSIAN MAFIA CAN BE PRETTY RUTHLESS IF THEY FEEL BETRAYED AND SHE HAS SUGGESTED A WAY
-- WE MIGHT MAKE THE THIEVES FALL OUT IT IS A CUNNING PLAN AND I THINK I CAN HELP

  -- Look at the Stats
  let icPt = ic pt
  putStrLn $ "7A: icPt = " ++ show icPt
  let micPt = mic pt
  putStrLn $ "7A: micPt = " ++ show micPt
  let mkaPt = mka pt
  putStrLn $ "7A: mkaPt = " ++ show mkaPt
  let dicPt = dic pt
  putStrLn $ "7A: dicPt = " ++ show dicPt
  let ediPt = edi pt
  putStrLn $ "7A: ediPt = " ++ show ediPt
  let (lrPt, rodPt) = lr pt
  putStrLn $ "7A: lrPt = " ++ show lrPt
  putStrLn $ "7A: rodPt = " ++ show rodPt
  let ldiPt = ldi pt
  putStrLn $ "7A: ldiPt = " ++ show ldiPt
  let sddPt = sdd pt
  putStrLn $ "7A: sddPt = " ++ show sddPt
  putStrLn $ "\n"


  return ()


printStats :: String -> IO ()
printStats ct = do
  let fs = countChars ct
  putStrLn $ "number of chars is: " ++ show (length $ loseZeros fs)
  let icCt = ic ct
  putStrLn $ "ic = " ++ show icCt
  let micCt = mic ct
  putStrLn $ "mic = " ++ show micCt
  let mkaCt = mka ct
  putStrLn $ "mka = " ++ show mkaCt
  let dicCt = dic ct
  putStrLn $ "dic = " ++ show dicCt
  let ediCt = edi ct
  putStrLn $ "edi = " ++ show ediCt
  let (lrCt, rodCt) = lr ct
  putStrLn $ "lr = " ++ show lrCt
  putStrLn $ "rod = " ++ show rodCt
  let ldiCt = ldi ct
  putStrLn $ "ldi = " ++ show ldiCt
  let sddCt = sdd ct
  putStrLn $ "sdd = " ++ show sddCt
  return ()

solve7B_2017::IO ()
solve7B_2017 = do
  inCipherText <- readFile "./src/2017/7B.txt"
  inCipherText2 <- readFile "./src/2017/7B_2.txt"
  let cipherText  = fmap toUpper $ clean (\c -> isAlpha c || c =='_') $ concat $ lines inCipherText

  let ct = decipher (scytaleCipher 6) cipherText

  let pt = solveVig ct
  putStrLn $ "7B: pt = " ++ show pt

  --putStrLn $ "7B: length of ct2 is: " ++ show (length ct2)
  --putStrLn $ unlines $ chunksOf 344 $ fmap (\c -> if c=='I' then 'O' else '-') ct2
  -- This says "Scytale securitatem praebet amplius"
  -- Which means "Scytale provides more security"

-- WITH HINDSIGHT AN OPPORTUNITY FOR PEACE WAS THROWN AWAY CALGACUS MAY HAVE BEEN A
-- TRAITOR BUT HE WAS BORN A ROMAN AND HAD FORGED THE LOCAL TRIBES INTO AN ORGANISED
-- AND DISCIPLINED FORCE IT MAY HAVE BEEN POSSIBLE TO TREAT WITH HIM BRINGING
-- CALEDONIA WITHIN THE EMPIRE IN EXCHANGE FOR THE GOVERNORSHIP OF CALEDONIA BUT SALUSTIUS WAS
-- A GREEDY AND AMBITIOUS MAN FOR WHOM VICTORY WAS EVERYTHING HE HAD NOW ISH TO SHARE CALEDONIA OR
-- GLORY WITH CALGACUS AND THUS CONCEIVED A PLAN TO REPAY THE TREACHERY OF THE CALEDONII IN FULL HE MADE
-- A GREAT FANFARE OF PUSHING HIS FORCES INTO NORTHERN CALEDONIA TO THE REMOTE FORT OF INCHTUTHIL
-- SALUSTIUS IN SECRET HAD CONCEIVED AS LYAND AUDACIOUS PLAN CATO WAS INSTRUCTED TO PROCEED WITH AN
-- EXPEDITIONARY FORCE FROM EBORACUM TO THE FRONTIER FORT AT INCHTUTHIL WHERE HE WOULD RELIEVE THE II
-- LEGION ADIUTRIXPIAFIDEL   -IS WHO WERE REQUIRED IN DACIA HE QUICKLY ESTABLISHED THE LEGIONIN A STATE
-- OF BATTLE READINESS AND WAS JOINED THERE BY SALUSTIUS WHO TOOK COMMAND OF THE IX LEGION HUMILIATING
-- THE FAITHFUL CATO IN FRONT OF HIS OWN MEN SALUSTIUS WAS NO STRATEGIST BUT IN TACTICS AND CUNNING HE
-- KNEW NO PARALLEL FROM THE LEGIONS EXTENSIVE SCOUTING NETWORK HE HAD LEARNED THAT CALGACUS HAD
-- ESTABLISHED HIS HEADQUARTERS IN THE MOUNTAINS WEST OF STRACATHRO THE MARCHING CAMP LEAVING CATO IN
-- CHARGE OF A SMALL HOLDING FORCE OF TWO COHORTS AT INCHTUTHIL SALUSTIUS OSTENTATIOUSLY LED THE REST
-- OF THE IX LEGION TO STRACATHRO HE SET UP CAMP AND LAID WASTE TO THE VILLAGES AROUND HIM IN AN ACT OF
-- GROSS PROVOCATION AND CRUELTY HE EMULATED HIS HERO CRASSUS KNOWING THAT THE MANY CALEDONII WHO SUFFERED
--  A GRUESOME END ON HIS CALEDONIAN WAY WERE NOT INVOLVED WITH CALGACUS AND HIS REVOLT THEY WERE JUST
-- FODDER IN HIS ATTEMPT TO PROVOKE A CONFRONTATION AS HE KNEW THEY MUST THE CALEDONIAN ARMIES MUSTERED
-- FOR A FINAL SHOW DOWN WITH THE LEGION AND MARCHED ON STRACATHRO SALUSTIUS SET HIS FORCES WITH PICKET
-- FENCES AND TRAPS BUT AGAINST THE SCALE OF THE CALEDONIAN FORCE THERE WAS LITTLE HOPE OF SUCCESS NONE
-- THE LESS WITH THEIR PRIDE AT STAKE AND ACCEPTING THEIR FATE THE LEGIONNAIRES OF THE IXTH WATCHED AND
-- WAITED TO JOIN BATTLE UNDER COVER OF DARKNESS AND BEFORE ANY SKIRMISH COULD BE CONDUCTED SALUSTIUS
-- SECRETLY SET OUT WITH A SMALL FORCE OF HAND PICKED SOLDIERS WHO HAD TRAVELLED WITH HIM FROM ROME
-- WITH A DEVIOUS PLAN TO STEAL THE CODEX FROM RIGHT UNDER CALGACUS NOSE SALUSTIUS WAS A VERY UNPLEASANT
-- MAN BUT HE WAS WIDELY READ AND KNEW OF THE HEBREW TALE OF GIDEON HE SET HIS GUARD AT KEY POINTS AROUND
-- THE PERIMETER OF THE CALEDONIAN CAMP AND IN THE DEPTHS OF NIGHT WHEN ALL MEN ARE AT THEIR LOWEST
-- EBB THEY ROSE AS ONE BREAKING COVER AND MAKING NOISE LIKE A MUCH LARGER ARMY IN THE CONFUSION
-- SALUSTIUSS LIEUTENANT STOLE INTO THE CAMP AND MADE AWAY WITH THE CODEX RETURNING IT TO SALUSTIUS
-- WHO REMAINED THROUGHOUT AT A SAFE DISTANCE PERHAPS SALUSTIUS CARED NOTHING FOR THE FATE OF THE IXTH
-- LEGION PERHAPS ITS DESTRUCTION WAS PART OF DOMITIANS PLANNED REVENGE OR PERHAPS IT WAS JUST A
-- DIVERSIONARY TACTIC TO HAVE THEM CAMPED AT STRACATHRO BUT THE OUTCOME WAS THE SAME ROUSED BY THE
-- SURPRISE INVASION OF THEIR CAMP THE CALEDONIAN WARRIORS LAUNCHED A MASSIVE ASSAULT ON STRACATHRO
-- WHATEVER HIS MOTIVATION IN AN ACT OF THE GREATEST TREACHERY SALUSTIU SABANDONED THE IXTH LEGION TO
-- OBLIVION AND SET OUT SOUTH FOR THE FORTIFIED PORT AT CARRIDEN WHERE HE INTENDED TO ESCAPE BY SEA
-- FROM BRITANNIA WITH THE CODEX HE SENT A DESPATCH TO CATO ORDERING HIM TO RETREAT WITH ALL ABLE
-- MEN TO CARRIDEN RAZE INCHTUTHIL TO THE GROUND AND LEAVE NONE OF THE WEAK OR WOUNDED ALIVE
-- IN ORDER TO PROVIDE NOTHING OF VALUE TO THE ENEMY SALUSTIUS AND HIS GUARD MUST HAVE THOUGHT
-- THEY WOULD BE SAFE BUT ONCE MORE A PROUD SON OF ROME HAD GROSSLY UNDERESTIMATED CALGACUS
-- REALISING QUICKLY THAT THE CODEX WAS LOST HE SET OUT TO RECOVER IT MARCHING HIS MEN DOUBLE
-- TIME IN PURSUIT OF THE FLEEING SALUSTIUS AND SO AT CARRIDEN SALUSTIUS REGROUPED WITH HIS TRUSTY
-- GUARD AND THE REMAINING COHORTS FROM INCHTUTHIL LED BY THE STEADFAST MARCUS FIDELIUS CATO IT
-- WAS NOT LONG BEFORE THE PURSUING CALEDONIAN FORCE ARRIVED BEFORE THE GATES OF CARRIDEN AND LAID
-- SIEGE WITH THEIR BACKS TO WATER THE ROMAN FORCE WAS TRAPPED THEIR SAFETY LAY IN A REFUGE FAR OVER
-- THE SEA AND IT IS FITTING THAT THE ENDING OF MY STORY WILL LIE BURIED SAFELY IN THEIR REFUGE
-- AT NOVIO MAGUS BATAVORUM AMCG



  return ()
