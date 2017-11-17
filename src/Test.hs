--{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test
    (
    test
) where

import System.IO
import Control.Monad
import Data.Array as A hiding((!))
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Map as M hiding((!), fromList)
import Data.List as L
import Text.Printf
import Data.Tuple (swap)
import Data.String.Utils

import Analysis
import Cribs
import Cipher
import Vignere
import Autokey
import Utils



test :: IO ()
test = do
  let inCipherText = "SLWXI GWOPG ROHXX MPRRV ASPFF GNEWR VJLTK JIXVE EWVLQ NXAWH SYVVY MDEPU YELJE YVEEL ZCKEG EMVMP XXAWI YGVRL HJMXR XXWKX CKILW UVGKP RAFZG JXOSK XCDSN FLWQW GTKZM PFJYK ZSTVX TPZEX VRLVG RCCHM JMQRJ GHEEI TTILW UVGKE KQAWU YSPFL SJRZX SKXCB IBFSJ KIQWW SPKEK PALLT LWLAS RUJEG ULMQE IWTQX JVYLL ZINVE DVMFD VHMZW TCIEW AKIRR TXJKG QEXTA FWCYY ZWFIY CITCG JHZRT FUMCC"
  let inPlainText = "AHUGE NEWLE AKOFF INANC IALDO CUMEN TSHAS REVEA LEDHO WTHEP OWERF ULAND ULTRA WEALT HYINC LUDIN GTHEQ UEENS PRIVA TEEST ATESE CRETL YINVE STVAS TAMOU NTSOF CASHI NOFFS HORET AXHAV ENSDO NALDT RUMPS COMME RCESE CRETA RYISS HOWNT OHAVE ASTAK EINAF IRMDE ALING WITHR USSIA NSSAN CTION EDBYT HEUST HELEA KDUBB EDTHE PARAD ISEPA PERSC ONTAI NSAHU GENEW LEAKO FFINA NCIAL"
  let cipherText  = clean isAlpha $ concat $ lines inCipherText
  let plainText  = clean isAlpha $ concat $ lines inPlainText
  let (codeKey, pts) = solveVig cipherText
  --print $ show codeKey
  --print $ show pts

  let key4 = "FORT"
  let key5 = "FORTY"
  let pt = "AHUGENEWLEAKOFFINANCIALDOCUMENTSHASREVEALEDHOWTHEPOWERFULANDULTRAWEALTHYINCLUDINGTHEQUEENSPRIVATEESTATESECRETLYINVESTVASTAMOUNTSOFCASHINOFFSHORETAXHAVENSDONALDTRUMPSCOMMERCESECRETARYISSHOWNTOHAVEASTAKEINAFIRMDEALINGWITHRUSSIANSSANCTIONEDBYTHEUSTHELEAKDUBBEDTHEPARADISEPAPERSCONTAINSAHUGENEWLEAKOFFINANCIAL"
  let ct4 = cipher (AutokeyCipher key4) pt
  let ct5 = cipher (AutokeyCipher key5) pt


  --let pts4 = fmap (\i -> decipher (AutokeyCipher $ replicate i 'A')  ct4) [1..10]
  --let fss4 = fmap quadgramScore pts4
  --putStrLn $ "\nquadgram scores are:\n" ++ show (sortOn fst $ zip fss4 fss4)
  --putStrLn $ "\nSo AAAA is best length\n"

  --let xx = keyLength ct4
  --putStrLn $ "Solution is: " ++ show xx
  --let yys = fmap (\i -> topChars ct4 4 i 4) [1..4]
  --putStrLn $ "Solution is: " ++ show yys
  -- ["WFES","BOSH","RWKA","AEIF"]

  let pts4 = fmap (\c -> decipher (AutokeyCipher ['A', 'A', 'A', c])  ct4) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let fss4 = fmap quadgramScore pts4
  putStrLn $ "\nquadgram scores are:\n" ++ show (sortOn fst $ zip fss4 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  putStrLn "\nSo T, X are the best letters\n"


  --let pts5 = fmap (\i -> decipher (AutokeyCipher $ replicate i 'A')  ct5) [1..10]
  --let fss5 = fmap quadgramScore pts5
  --putStrLn $ "\nquadgram scores are:\n" ++ show fss5
  --putStrLn "\nSo AAAAA is best length\n"

  --let pts5 = fmap (\c -> decipher (AutokeyCipher $ c : "AAAA")  ct5) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  --let fss5 = fmap quadgramScore pts5
  --putStrLn $ "\nquadgram scores are:\n" ++ show (sortOn fst $ zip fss5 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  --putStrLn "\nSo B, L, F are the best letters\n"

  --let pts5 = fmap (\c -> decipher (AutokeyCipher ['A', c, 'A', 'A', 'A'])  ct5) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  --let fss5 = fmap quadgramScore pts5
  --putStrLn $ "\nquadgram scores are:\n" ++ show (sortOn fst $ zip fss5 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  --putStrLn "\nSo O, K, B are the best letters\n"

  --let pts5 = fmap (\c -> decipher (AutokeyCipher ['A', 'A', c, 'A', 'A'])  ct5) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  --let fss5 = fmap quadgramScore pts5
  --putStrLn $ "\nquadgram scores are:\n" ++ show (sortOn fst $ zip fss5 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  --putStrLn "\nSo R, Q are the best letters\n"

  --let pts5 = fmap (\c -> decipher (AutokeyCipher ['A', 'A', 'A', c, 'A'])  ct5) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  --let fss5 = fmap quadgramScore pts5
  --putStrLn $ "\nquadgram scores are:\n" ++ show (sortOn fst $ zip fss5 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  --putStrLn "\nSo T, X are the best letters\n"

  --let pts5 = fmap (\c -> decipher (AutokeyCipher ['A', 'A', 'A', 'A', c])  ct5) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  --let fss5 = fmap quadgramScore pts5
  --putStrLn $ "\nquadgram scores are:\n" ++ show (sortOn fst $ zip fss5 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  --putStrLn "\nSo J, N, Y are the best letters\n"

  --let pwds = [[a,b,c,d,e]| a <- "BLF", b<-"OKB", c<-"RQ", d<-"TX", e<-"JNY"]
  --let pts = fmap (\pwd -> decipher (AutokeyCipher pwd)  ct5) pwds
  --let fss = fmap quadgramScore pts
  --putStrLn $ "\nquadgram scores are:\n" ++ show (sortOn fst $ zip fss pwds)
  --putStrLn "\nSo FORTY wins\n"



  return ()


go :: String -> (Char, String)
go ct = (c, decipher (ShiftCipher c) ct)
  where
    unshifts = L.map (\c ->  decipher (ShiftCipher c) ct) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    fds = L.map freqDist unshifts
    c = nchr $ ixOfMin fds
