{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE RoleAnnotations     #-}


module Cipher
    (
        CadenusCipher(..)
        , Cipher(..)
        , cipherRailRoad
        , decipherRailRoad
        , AffineCipher(..)
        , HillCipher(..)
        , KeyCipher(..)
        , RailRoadCipher(..)
        , PlayfairCipher(..)
        , TranspositionCipher(..)
        , AmscoCipher(..)
        , lineLengths
        , padLines
        , getWords
        , reverseAlternate
        , rail
        , derail
        , zipSort
        , key
        , hillCrib
        , mapi
        , spin
        , unSpin
        , keySort
        , unKeySort
        , writeToSchema
        , decipherAmsco
        , cipherAmsco
        , amscoSchema
        , solveAmsco
        , decipherOld
        , mightBeEnglish
        , myInv
        , mySolve
    ) where

-- import NumericPrelude hiding (unzip)
-- import MathObj.Matrix as MM hiding (zipWith)

import GHC.TypeLits
import GHC.Exts (sortWith)
import Data.Int
import Data.Maybe (fromJust)
import Data.Char
import Data.Ord
import Data.Monoid hiding((<>))
import Data.Map as M hiding (size)
import Data.List as L hiding (unzip)
import Data.Set as S
import Data.String.Utils as H hiding (join)
import Control.Monad (join)
import Data.Vector as V hiding (sequence, (++), concatMap, drop, take, length, zipWith, zip, head, concat, unzip, elem, replicate, tail, reverse)

import Analysis hiding (chunksOf)
import Cribs
import Utils
import Data.Proxy
import Numeric.LinearAlgebra


{-

This is a start in producing a simple cryptanalysis library in Haskell.

Just random thoughts

We'll have cipher text as UPPER CASE and plain text as lower case

we have ord - gives the int for a letter, and chr gives the letter of an int

-}

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

--chunksOf :: Int -> [e] -> [[e]]
--chunksOf i ls = fmap (take i) (build (splitter ls)) where
--  splitter :: [e] -> ([e] -> a -> a) -> a -> a
--  splitter [] _ n = n
--  splitter l c n  = l `c` splitter (drop i l) c n

{-# INLINE forcePair #-}
forcePair :: (a,b) -> (a,b)
forcePair ~(a,b) = (a,b)

--unzip :: [(a,b)] -> ([a],[b])
--unzip = forcePair . L.foldr (\ (x,y) ~(xs,ys) -> (x:xs,y:ys)) ([],[])

mapi :: (a -> Int -> b) -> [a] -> [b]
mapi f l = L.zipWith f l [0..]

class Cipher a where
    cipher::a->String->String
    decipher::a->String->String


-- This splits the pt into 2,1,2... letter groups and then splits
-- these into groups of a given length (length of code) then these
-- columns are rearranged according to the code and concatenated together.
-- The initial columns may be longer because we run out of letters.
-- This version does 2,1,2... when the Bool is TRUE 1,2,1... otherwise

data AmscoCipher = AmscoCipher Bool [Int] deriving (Show)

-- | The Cadenus cipher has a keyword. The plain text is written in columns under
-- the key word in batches of 25*length (ie. until the columns are 25 letters long)
-- The columns are then arranged alphabetically and then the columns are started on
-- according to the letter for the code of that column (ie. if the letter is e then
-- the column is rotated to start at the 5th lettr in the column
data CadenusCipher = CadenusCipher [Int] deriving (Show)

instance Cipher CadenusCipher where
    cipher (CadenusCipher ky) pt = cipherCadenus ky pt
    decipher (CadenusCipher ky) ct = decipherCadenus ky ct

-- The list must be finite otherwise I don't think the ++ works on the end on an infinite list
spin :: Int -> [a] -> [a]
spin n xs = (drop n xs) ++ (take n xs)

-- The list must be finite otherwise I don't think the ++ works on the end on an infinite list
unSpin :: Int -> [a] -> [a]
unSpin n xs = spin (length xs - n ) xs

-- Sort the list and key into ascending order and just return the list
keySort :: Ord s => [s] -> [a] -> [(s, a)]
keySort ky xs = sortBy (comparing fst) $ zip ky xs

-- Not very efficient...
unKeySort :: [Int] -> [a] -> [(Int, a)]
unKeySort ky xs = zipWith (\k t -> (k, snd t)) ky $ keySort (L.map snd $ keySort ky [1..]) xs

-- The length of the cipher text need to be a multiple of the length of the key *25
-- so I pad to that with x's.
cipherCadenus :: [Int] -> String -> String
cipherCadenus ky pt = concatMap cipherBlock blocksOfPt
    where
        n = length pt
        m = length ky
        paddedPt = pt ++ (take (((if ((rem n m) > 0) then 1 else 0) + n `div` (m*25))*m*25 - n) $ repeat 'x')
        blocksOfPt = chunksOf (m*25) paddedPt
        cipherBlock :: String -> String
        cipherBlock bpt = L.concat $ L.transpose $ L.map (\t -> unSpin (fst t) $ snd t) $ keySort ky $ cols bpt
            where
                cols bpt = L.transpose $ chunksOf m bpt

decipherCadenus ky ct = concatMap decipherBlock blocksOfCt
    where
        n = length ct
        m = length ky
        blocksOfCt = chunksOf (m*25) ct
        decipherBlock bct = L.concat $ L.transpose $ L.map (\t -> spin (fst t) $ snd t) $ unKeySort ky $ cols bct
            where
                cols bct = L.transpose $ chunksOf m bct



instance Cipher AmscoCipher where
    -- To decipher we need to write the ct back into columns
    -- Sort them out with the key
    -- Put them back into the 2,1,2... format
    -- Transpose and then concatenate.
    decipher = decipherAmsco
    cipher = cipherAmsco

decipherAmsco :: AmscoCipher -> String -> String
decipherAmsco cipher@(AmscoCipher is21 code) ct = concat $ concat $ L.transpose $ fmap snd $ sortWith fst $ zip code codeInSchema
    where
        len = length ct
        schema = amscoSchema len cipher
        codeInSchema = writeToSchema ct $ L.map (\ix -> (L.transpose $ schema)!!ix) code

decipherOld (AmscoCipher _ code) ct = concat $ concat $ L.transpose ctCols
    where ncol = length code
          len = length ct
          colLength = len `div` ncol
          colRem = len `rem` ncol
          cols = L.map (\ix-> colLength + (if (ix<colRem) then 1 else 0) ) code
          drops = L.scanl (\acc x -> acc + x) 0 cols
          cts = L.zipWith (\c d -> take c $ drop d ct) cols drops
          sortedCts = L.map (\ix -> cts!!ix) code
          ctCols = mapi (\xs i -> L.concatMap (\c -> let p = (if (even i) then 2 else 1)
                                                     in [take p c, drop p c]
                                              ) $ chunksOf 3 xs
                        ) sortedCts


cipherAmsco :: AmscoCipher -> String -> String
cipherAmsco cipher@(AmscoCipher is12 code) pt = concat $ concat $ L.map (\ix -> (L.transpose $ codeInSchema)!!ix) code
    where
        len = length pt
        schema = amscoSchema len cipher
        codeInSchema :: [[String]]
        codeInSchema = writeToSchema pt schema


-- amscoSchema take the length of the text and an AmcsoCipher
amscoSchema :: Int -> AmscoCipher -> [[Int]]
amscoSchema  len (AmscoCipher is21 code) = (take nRows $ concat $ repeat $ [take nCols $ concat $ repeat doub,take nCols $ concat $ repeat $ reverse doub] ) ++ [extraEnd]
    where
        -- The initial order
        doub = if is21 then [2,1] else [1,2]
        nCols = length code
        -- The number of characters on a line ********** ONLY WOKRS FOR nCols EVEN  ***************************
        nChars = L.sum $ take nCols $ concat $ repeat doub
        -- The number of rows and the characters left over
        (nRows, rowRem) = len `quotRem` nChars
        -- The number of 3's in the leftovers, and another leftover
        (nColsRem, charsRem) = rowRem `quotRem` 3
        -- If the nRows is even the extra row will start doub else it will start reverse doub
        extraDoub = if (even nRows) then doub else (reverse doub)
        -- If charsRem is one then we append a 1.
        -- If charsRem is 2 then if extraDoub is doub then we append [1,1] else we append [2]
        extraEnd = if (charsRem==1) then
                        (1:(take (nCols-nColsRem*2-1) $ repeat 0))
                   else
                        (if (even nRows) then
                            (1:1:(take (nCols-nColsRem*2-2) $ repeat 0))
                         else
                            (2:(take (nCols-nColsRem*2-1) $ repeat 0))
                        )
        extraRow = (take (nColsRem*2) $ concat $ repeat extraDoub) ++ extraEnd


writeToSchema :: String -> [[Int]] -> [[String]]
writeToSchema ct schema = fst $ L.foldl goOutside ([], ct) $ take (length schema) [0..]
    where
        -- takes the remaining ct and the accumulated list of strings
        -- adds a string of length n onto the string list and drops it from the remaining ct
        goInside :: ([String],String)->Int->([String],String)
        goInside (acc, ct) n = (acc ++ [take n ct], drop n ct)
        -- Makes a new column by calling goInside and adds it to the accumulated columns
        goOutside :: ([[String]],String)->Int->([[String]],String)
        goOutside (cols, ctIn) ix = (cols ++ [newCol], ctOut)
            where
                (newCol,ctOut) = L.foldl goInside ([], ctIn) $ schema!!ix

-- Solves Asmco ciphers when you know the key size
solveAmsco::Bool->Int->String->[(String, [Int])]
solveAmsco is21 n ct = L.filter (\(pt, code) -> mightBeEnglish pt) pts
    where
        pts  :: [(String, [Int])]
        pts  = (fmap (\code -> (decipher (AmscoCipher is21  code) ct, code)) $ permutations $ take n [0..])



-- This function just woks out the trigrams and checks that "THE" and "AND" are in the top 4
mightBeEnglish :: String -> Bool
mightBeEnglish pt = (elem "THE" top4) && (elem "AND" top4)
    where
        top4 = topTriGrams 10 pt

topTriGrams n ct = fmap fst $ take n $ reverse $ sortWith snd $ M.toList $ count2freq $ loseZeros $ countNgrams 3 ct

--NumericPrelude.filter (\(i,x)-> x=="THE") $ zip [0..] $ fmap (\code -> head $ topTriGrams 1 $ decipher (AmscoCipher True code) ct) $ permutations [0,1,2,3,4,5]

--codes = fmap (\t-> (permutations [0,1,2,3,4,5])!!(fst t)) $ NumericPrelude.filter (\(i,x)-> x=="THE") $ zip [0..] $ fmap (\code -> head $ topTriGrams 1 $ decipher (AmscoCipher True code) ct) $ permutations [0,1,2,3,4,5]

-- deriving instance Storable (Mod i m)
-- deriving instance Element (Mod i m)

-- A matix is T in the numeric prelude
data KnownNat n => HillCipher n = HillCipher (Matrix (Z ./. n)) -- deriving (Show)

instance (KnownNat n) => Cipher (HillCipher n) where
    cipher = cipherHill
    decipher (HillCipher m) = cipherHill (HillCipher $ myInv m)


fromInt64ToInt :: Int64 -> Int
fromInt64ToInt = fromIntegral


fromIntToInt64 :: Int -> Int64
fromIntToInt64 = fromIntegral


-- Temporary until I work out how to do inverse in NumericPrelude
cipherHill :: (Integral i, KnownNat n) => HillCipher n -> String -> String
cipherHill (HillCipher m) ct = L.map (\x-> nchr $ fromIntegral x) $ concat $ Numeric.LinearAlgebra.toLists $ toInt cs
    where
      ncols = Numeric.LinearAlgebra.cols m
      nrows = Numeric.LinearAlgebra.rows m
      -- Groups of letters converted to [Int]
      --xs :: [[Int]]
      xs = chunksOf ncols $ L.map (\c -> fromIntToInt64 (nord c)) ct
      -- Convert the groups to vectors to a matrix
      --ws :: T Int
      wsInt = fromLists xs
      ws = fromZ wsInt
      -- Calculate the cipher text
      cs = ws <> m


-- Not it is assumed that its a 2*2 matrix!!!
myInv :: forall n. KnownNat n => Matrix (Z ./. n) -> Matrix (Z ./. n)
myInv k = head [ pi | a <- rng, b <- rng, c <- rng, d <- rng, let pi = fromZ $ (2><2) [a, b, c, d] , (k<>pi) == eye ]
  where
    nn :: Integer
    nn = natVal (Proxy:: Proxy n)

    mm :: Z
    mm = fromIntegral nn

    rng :: [Z]
    rng = [0..(mm-1)]

    eye = (2><2) [1::Z ./. n, 0::Z ./. n, 0::Z ./. n, 1::Z ./. n]


-- Solves a<>m == b for m, where they are all 2*2 matrices
mySolve :: forall n. KnownNat n => Matrix (Z ./. n) -> Matrix (Z ./. n) -> [Matrix (Z ./. n)]
mySolve a b = [ m | w <- rng, x <- rng, y <- rng, z <- rng, let m = fromZ $ (2><2) [w, x, y, z] , (a<>m) == b ]
  where
    nn :: Integer
    nn = natVal (Proxy:: Proxy n)

    mm :: Z
    mm = fromIntegral nn

    rng :: [Z]
    rng = [0..(mm-1)]

    eye = (2><2) [1::Z ./. n, 0::Z ./. n, 0::Z ./. n, 1::Z ./. n]







getN :: Proxy nAlphabet -> Int
getN (Proxy) = nAlphabet

-- This works out a 2x2 matrix from a 4 letter crib
hillCrib:: String -> String -> Matrix (Z ./. 26)
hillCrib ct pt = key
    where
        --nct :: [Z ./. 26]
        nct = L.map nord ct
        --npt :: [Int ./. 26]
        npt = L.map nord pt
        --ca :: Z ./. 26
        --cb :: Z ./. 26
        --cc :: Z ./. 26
        --cd :: Z ./. 26
        ca = head nct
        cb = head $ tail nct
        cc = head $ tail $ tail $ nct
        cd = head $ tail $ tail $ tail $ nct
        --pa :: Z ./. 26
        --pb :: Z ./. 26
        --pc :: Z ./. 26
        --pd :: Z ./. 26
        pa = head npt
        pb = head $ tail npt
        pc = head $ tail $ tail $ npt
        pd = head $ tail $ tail $ tail $ npt
        -- Work out the inverse cipher text matrix
        -- mpt :: Matrix (Z ./. n)
        mpt = fromInt ((2><2) $ fmap int2i npt) :: Matrix (Z ./. 26)
        --mct :: Matrix (Z ./. 26)
        mct = fromInt ((2><2) $ fmap int2i nct) :: Matrix (Z ./. 26)

        key = luSolve' (luPacked' mpt) mct


data AffineCipher n = AffineCipher (Z ./. n) (Z ./. n) deriving (Show)

-- a must be coprime with nAlphabet ie a = 1,3,5,7,9,11,15,17,19,21,23,25
instance KnownNat n => Cipher (AffineCipher n) where
    cipher (AffineCipher a b) =  L.map (affineShift a b)
    decipher (AffineCipher a b) = L.map (affineDeshift a b)

affineShift::KnownNat n => Z ./. n -> Z ./. n-> Char -> Char
affineShift a b x = nchr $ mod2int $ a*nx + b
  where
      nx = int2mod $ nord x

--normalize :: (Reifies s a, KnownNat s, Integral a) => a -> Mod a s
--normalize :: (Reifies s a, KnownNat s, Integral a) => a -> Mod Int n
--normalize a = b where b = (mod a (reflect b))::Mod Int n

-- Shift a char x to a*x+b mod 26
--affineDeshift :: Integral a => a -> a -> Char -> Char
--affineDeshift :: KnownNat n => Mod Int n -> Mod Int n -> Char -> Char
affineDeshift :: KnownNat n => Z ./. n -> Z ./. n -> Char -> Char
affineDeshift i j x = nchr $ mod2int $ invi*xj
  where
    mx = int2mod $ nord x
    invi = 1 / i
    xj = mx - j




data KeyCipher = KeyCipher KeyMap deriving (Show)

-- (De)ciphers to * if the letter isn't included
-- This means that zeoMap can just be empty
instance Cipher KeyCipher where
    cipher (KeyCipher (KeyMap a)) = L.map (\c-> findWithDefault '*' c a)
    decipher (KeyCipher (KeyMap a)) = L.map (\c-> findWithDefault '*' c ra)
        where
            ra = M.fromList $ L.map (\x-> (snd x, fst x)) $ M.toList a


instance Monoid KeyCipher  where
    mempty = KeyCipher zeroMap
    mappend (KeyCipher k) (KeyCipher l) = KeyCipher $ k `mappend` l


instance Cipher RailRoadCipher where
    cipher (RailRoadCipher n) = rail n --cipherRailRoad n
    decipher (RailRoadCipher n) = derail n --decipherRailRoad n


data RailRoadCipher = RailRoadCipher Int deriving (Show)


-- n is how many characters down you go - so the wave length is n+n-2
cipherRailRoad::Int->String->String
cipherRailRoad n pt = H.replace "#" "" $ concat $ splitText n words
    where
        l = length pt
        waveLength = 2*n-2
        (nWaves, rem) = quotRem l waveLength
        paddedPt = pt ++ (if rem>0 then take (waveLength - rem) $ repeat '#' else "")
        waves = chunksOf waveLength paddedPt
        words = concatMap (\w -> (take n w) ++ ['#'] ++ (reverse $ drop n w) ++ ['#']) waves


-- n is how many characters down you go - so the wave length is n+n-2
-- So we have to split the ct with words of length
decipherRailRoad::Int->String->String
decipherRailRoad n ct = H.replace "#" "" $ concat $ reverseAlternate pt
    where
        l = length ct
        waveLength = 2*n-2
        (nWaves, rem) = quotRem l waveLength
        nWavesPlus = nWaves + (if rem>0 then 1 else 0)
        ns = lineLengths n rem nWaves
        words = getWords ct ns
        transposedPt = padLines words $ 2*nWavesPlus
        pt = splitText (2*nWavesPlus) $ concat transposedPt

reverseAlternate::[String]->[String]
reverseAlternate [] = []
reverseAlternate (x:[]) = [x]
reverseAlternate (x:y:xs) = [x,reverse y] ++ (reverseAlternate xs)


padLines::[String]->Int->[String]
padLines words n = [first] ++ middles ++ [last]
    where
        first = padTo n $ intersperseBlank $ head words
        last = padTo n $ intersperseBlank $ head $ reverse words
        middles = L.map (padTo n) $ reverse $ tail $ reverse $ tail words

lineLengths n 0 nWaves = [nWaves] ++ (take (n-2) $ repeat $ 2*nWaves) ++ [nWaves]
lineLengths n rem nWaves = if rem<=n then
                                    [nWaves+1] ++ (take (rem-1) $ repeat $ 2*nWaves+1)
                                               ++ (take (n-rem-1) $ repeat $ 2*nWaves)
                                               ++ [nWaves]
                                 else
                                    [nWaves+1] ++ (take (2*n-rem-2) $ repeat $ 2*nWaves+1)
                                               ++ (take (rem-n) $ repeat $ 2*nWaves+2)
                                               ++ [nWaves+1]

getWords ct [] = []
getWords ct (x:xs) = if length ct<= x then [take x ct] else (take x ct):(getWords (drop x ct) xs)

intersperseBlank xs = concatMap (\c->[c,'#']) xs

padTo n xs = xs ++ (take (n-(length xs)) $ repeat '#')


rail :: Int -> [a] -> [a]
rail n = zipSort (cycle $ [1..n] ++ [n-1,n-2..2])

derail :: Ord a => Int -> [a] -> [a]
derail n s = zipSort (rail n $ take (length s) [0..]) s

zipSort :: Ord a => [a] -> [b] -> [b]
zipSort ks = L.map snd . sortWith fst . zip ks

-- Bifid

-- This is an example of a bifid square - the alphabet is in order
-- of first line, second line etc. of the 5x5 square


-----------------------------------------------------------------------------
--
-- Playfair Cipher
--
------------------------------------------------------------------------------

type Key = M.Map (Int, Int) Char

process :: String -> String
process = H.replace "J" "I" . L.map toUpper . L.filter isLetter

key :: String -> Key
key = M.fromList . concat .
      zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0..]) [0..] .
      chunksOf 5 . (`L.union` "ABCDEFGHIKLMNOPQRSTUVWXYZ") . nub . process

bigram :: Key -> Int -> Char -> Char -> String
bigram k dir c1 c2
    | y1 == y2  = get (x1 + dir, y1) : [get (x2 + dir, y2)]
    | x1 == x2  = get (x1, y1 + dir) : [get (x2, y2 + dir)]
    | otherwise = get (x2, y1)       : [get (x1, y2)]
    where (x1, y1) = head . M.keys $ M.filter (== c1) k
          (x2, y2) = head . M.keys $ M.filter (== c2) k
          get (x,y) = k M.! (mod x 5, mod y 5)

playfairCipher :: Key -> String -> String
playfairCipher _ []       = []
playfairCipher k [x]      = playfairCipher k (x : "X")
playfairCipher k (x:y:xs) | x == y    = playfairCipher k [x] ++ playfairCipher k (y:xs)
                   | otherwise = bigram k 1 x y ++ playfairCipher k xs

playfairDecipher :: Key -> String -> String
playfairDecipher k = concatMap (\[x,y] -> bigram k (-1) x y) . chunksOf 2

-- This does Bifid where the period is the whole of the plain text
data PlayfairCipher = PlayfairCipher Key

instance Cipher PlayfairCipher where
    cipher (PlayfairCipher bs) = playfairCipher bs
    decipher (PlayfairCipher bs) = playfairDecipher bs


-- This is a transpoisiotn cipher where the key is given by [Int]
data TranspositionCipher = TranspositionCipher [Int]

instance Cipher TranspositionCipher where
    cipher (TranspositionCipher key) pt = L.concat $ L.map fst $ sortWith snd $ zip (L.transpose ws) key
        where
            m = length key
            ws = chunksOf m pt
    decipher (TranspositionCipher key) ct = L.concat $ L.transpose  $ L.map fst $ sortWith snd $ zip ws unKey
        where
          unKey = L.map fst $ sortWith snd $ zip [0..] key
          m = length key
          n = length ct
          (q, r) = quotRem n m
          cols = zipWith (+) (take n $ repeat q) ((take r $ repeat 1) ++ (take (m-r) $ repeat 0))
          ws = chunkUsing cols ct
          -- putStrLn $ concat $ L.transpose ws
