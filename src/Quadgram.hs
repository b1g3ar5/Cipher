module Quadgram
  (
  qscore
  , addWordScore
  , calcIx
  , qgram
  , readDoubles
  ) where

import System.IO.Unsafe (unsafePerformIO)
import Utils (nord)
import Data.Vector.Unboxed
import Data.ByteString.Lazy (ByteString, index, pack)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lex.Fractional (readDecimal, readSigned)
import Data.ByteString.Internal (w2c)

import GHC.Word (Word8)


--w2c :: Word8 -> Char
--w2c = unsafeChr . fromIntegral
--{-# INLINE w2c #-}

-- assumes that text consists only of uppercase letters(no punctuation or spaces)
qscore :: ByteString -> Double
{-# INLINE qscore #-}
qscore ct = foldl' (addWordScore ct) 0 $ generate (fromIntegral $ (BS.length ct)-4) id

-- Calc the index given a 4 letter string
calcIx:: ByteString -> Int
{-# INLINE calcIx #-}
calcIx ct = (nord $ w2c $ ct `index` 0) * 17576 + (nord $ w2c $ ct `index` 1) * 676 + (nord $ w2c $ ct `index` 2) * 26 + (nord $ w2c $ ct `index` 3)

-- The function called for each index value
addWordScore :: ByteString -> Double -> Int -> Double
{-# INLINE addWordScore #-}
addWordScore ct score ix = score + qgram!(calcIx $ BS.take 4 (BS.drop (fromIntegral ix) ct))


qgram :: Vector Double
qgram = readDoubles empty bs
  where
    bs = unsafePerformIO $ BS.readFile $ "src/Data/QuadgramsSmall.txt"

readDoubles :: Vector Double ->  ByteString -> Vector Double
readDoubles vs ss = maybe vs id $ fmap (go vs) $ parseDouble ss
  where
    parseDouble :: ByteString -> Maybe (Double, ByteString)
    parseDouble bs = fmap (\t->(fst t, BS.fromStrict $ snd t)) $ readSigned readDecimal $ BS.toStrict bs

    go :: Vector Double -> (Double, ByteString) -> Vector Double
    go acc t = readDoubles (snoc acc $ fst t) $ BS.tail $ snd t
