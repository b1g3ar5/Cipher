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
import Utils



test :: IO ()
test = do
  let inCipherText = "SLWXI GWOPG ROHXX MPRRV ASPFF GNEWR VJLTK JIXVE EWVLQ NXAWH SYVVY MDEPU YELJE YVEEL ZCKEG EMVMP XXAWI YGVRL HJMXR XXWKX CKILW UVGKP RAFZG JXOSK XCDSN FLWQW GTKZM PFJYK ZSTVX TPZEX VRLVG RCCHM JMQRJ GHEEI TTILW UVGKE KQAWU YSPFL SJRZX SKXCB IBFSJ KIQWW SPKEK PALLT LWLAS RUJEG ULMQE IWTQX JVYLL ZINVE DVMFD VHMZW TCIEW AKIRR TXJKG QEXTA FWCYY ZWFIY CITCG JHZRT FUMCC"
  let inPlainText = "AHUGE NEWLE AKOFF INANC IALDO CUMEN TSHAS REVEA LEDHO WTHEP OWERF ULAND ULTRA WEALT HYINC LUDIN GTHEQ UEENS PRIVA TEEST ATESE CRETL YINVE STVAS TAMOU NTSOF CASHI NOFFS HORET AXHAV ENSDO NALDT RUMPS COMME RCESE CRETA RYISS HOWNT OHAVE ASTAK EINAF IRMDE ALING WITHR USSIA NSSAN CTION EDBYT HEUST HELEA KDUBB EDTHE PARAD ISEPA PERSC ONTAI NSAHU GENEW LEAKO FFINA NCIAL"
  let cipherText  = clean isAlpha $ concat $ lines inCipherText
  let plainText  = clean isAlpha $ concat $ lines inPlainText
  let (codeKey, pts) = solveVig cipherText
  print $ show codeKey
  print $ show pts
  return ()


go :: String -> (Char, String)
go ct = (c, decipher (ShiftCipher c) ct)
  where
    unshifts = L.map (\c ->  decipher (ShiftCipher c) ct) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    fds = L.map freqDist unshifts
    c = nchr $ ixOfMin fds
