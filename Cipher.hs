{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cipher
    (
	    ShiftCipher(..)
	    , Cipher(..)
	    , caesarCipher
	    , cipherRailRoad
	    , decipherRailRoad
	    , VigCipher(..)
	    , solveVig
	    , AffineCipher(..)
	    , HillCipher(..)
	    , KeyCipher(..)
	    , RailRoadCipher(..)
	    , BifidCipher(..)
	    , BifidPeriodCipher(..)
	    , PlayfairCipher(..)
        , TranspositionCipher(..)
        , AmscoCipher(..)
	    , bifidCipher
	    , bifidDecipher
	    , bifidSquare
	    , coordValue
	    , lineLengths
	    , padLines
	    , getWords
	    , reverseAlternate
	    , Cipher.indices
	    , toIndex
	    , fromIndex
	    , prepare
	    , Crib(..)
	    , Axis(..)
	    , Coord(..)
	    , Rule(..)
	    , applyCrib
	    , grids
	    , consolidate
	    , initialChoices
	    , prunes
	    , prune
	    , allChooses
	    , validGrid
	    , rail
	    , derail
	    , zipSort
	    , key
        , hillCrib
        , mapi
    ) where

import NumericPrelude
import MathObj.Matrix as MM hiding (zipWith)
--import Algebra.Vector hiding (Eq(..))

import GHC.Exts (sortWith)
import Data.Char
import Data.Monoid
import Data.Map as M
import Data.List as L
import Data.List.Split as LS (chunksOf)
import Data.List.HT as H hiding (unzip)
--import Data.Tuple (swap)
--import Data.String.Utils as U
import Data.Set as S

import Analysis hiding (chunksOf)
import Cribs
import Mod

{-

This is a start in producing a simple cryptanalysis library in Haskell.

Just random thoughts

We'll have cipher text as UPPER CASE and plain text as lower case

we have ord - gives the int for a letter, and chr gives the letter of an int

-}

mapi :: (a -> Int -> b) -> [a] -> [b]
mapi f l = L.zipWith f l [0..]

class Cipher a where
    cipher::a->String->String
    decipher::a->String->String


-- This splits the pt into 2,1,2... letter groups and then splits 
-- these into groups of a given length (length of code) then these 
-- columns are rearranged according to the code and concatenated together.
-- The initial columns may be longer because we run out of letters.
-- This version only does 2,1,2... not 1,2,1...
data AmscoCipher = AmscoCipher [Int] deriving (Show)

-- | The Cadenus cipher has a keyword. The plain text is written in columns under
-- the key word until the columns are 25 letters long
data CadenusCipher = CadenusCipher String deriving (Show)

instance Cipher AmscoCipher where
    -- To decipher we need to write the ct back into columns
    -- Sort them out with the key
    -- Put them back into the 2,1,2... format
    -- Transpose and then concatenate.
    decipher (AmscoCipher code) ct = concat $ concat $ L.transpose ctCols
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

    cipher (AmscoCipher code) pt = L.concat $ L.concatMap (\ix -> pts!!ix) code
        where n = length code
              pts = L.transpose $ chunksOf n $ L.concatMap (\c-> [take 2 c, drop 2 c]) $ chunksOf 3 pt


data ShiftCipher = ShiftCipher Char deriving (Show)

instance Cipher ShiftCipher where
    cipher (ShiftCipher a) = L.map (cShift a) 
    decipher (ShiftCipher a) = L.map (cShift $ nchr $ nAlphabet- nord a)

-- 'D' is equivalent to 3
caesarCipher = ShiftCipher 'D'

data VigCipher = VigCipher String deriving (Show)

instance Cipher VigCipher where
    cipher (VigCipher key) = zipWith cShift (cycle key)
    decipher (VigCipher key) = zipWith (\k c -> cShift (minus k) c) (cycle key)
        where
            minus k = nchr $ nAlphabet - nord k

-- Solves Vignere returning the key and the plain text
solveVig::String->(String, String)
solveVig cipherText = (key, plain)
    where
    cipherCount = txt2count $ cipherText
    cipherFreq = count2freq cipherCount 
    keyICs = L.map (\n-> splitIC n cipherText) [1..20]
    bestKeySize = (ixOfMin $ L.map (\d->(d-1.73)**2.0) keyICs) +1
    bestSplit = splitText bestKeySize cipherText -- an Array of Strings of size bestKeySize
    letterCorrelations = L.map (\s-> ixOfMin $ (L.map (\n ->(1.0-(Analysis.corr n s))**2.0) ("ABCDEFGHIJKLMNOPQRSTUVWXYZ"++[chr 95]))) bestSplit
    key = L.map (\i->chr $ i + 65 ) letterCorrelations
    plain = cipher (VigCipher key) cipherText


-- A matix is T in the numeric prelude
data HillCipher = HillCipher (T Int) deriving (Show)

instance Cipher HillCipher where
    cipher = cipherHill
    decipher = decipherHill


-- Temporary until I work out how to do inverse in NumericPrelude
decipherHill :: HillCipher -> String -> String
decipherHill (HillCipher m) ct = L.map (\x-> nchr $ mod x 26) $ concat $ rows cs
    where
        (ncols, nrows) = dimension m
        -- Groups of letters converted to [Int]
        --xs :: [[Int]]
        xs = chunksOf ncols $ L.map nord ct
        -- Convert the groups to vectors to a matrix
        --ws :: T Int
        ws = MM.fromColumns (length xs) nrows $ L.transpose xs
        -- Calculate the cipher text
        cs = ws * m


-- This works out a 2x2 matrix from a 4 letter crib
hillCrib:: String -> String -> Maybe (T Int)
hillCrib ct pt = fmap (\a-> MM.fromRows 2 2 a) mabcd
    where
        nct = L.map nord ct
        npt = L.map nord pt
        ca = head nct
        cb = head $ tail nct
        cc = head $ tail $ tail $ nct
        cd = head $ tail $ tail $ tail $ nct
        pa = head npt
        pb = head $ tail npt
        pc = head $ tail $ tail $ npt
        pd = head $ tail $ tail $ tail $ npt
        -- Work out the inverse cipher text matrix    
        miv :: Maybe (T Int)    
        miv = inverse (MM.fromList 2 2 [ca, cb, cc, cd])
        -- The pt times the ct inverse gives the matrix entries
        mab :: Maybe (T Int)    
        mab = fmap (\iv -> iv * (MM.fromList 2 1 [pa, pc])) miv
        mcd :: Maybe (T Int)    
        mcd = fmap (\iv -> iv * (MM.fromList 2 1 [pb, pd])) miv
        -- They need to be modded
        mmab :: Maybe [Int]
        mmab = fmap (\xs -> L.map (\x -> mod x 26) $ Mod.toList xs) mab
        mmcd :: Maybe [Int]
        mmcd = fmap (\xs -> L.map (\x -> mod x 26) $ Mod.toList xs) mcd
        -- combine the 2 maybes to one maybe
        mabcd:: Maybe [[Int]]
        mabcd = maybe Nothing (\a->maybe Nothing (\c-> Just [a,c]) mmcd) mmab
        
cipherHill :: HillCipher -> String -> String
cipherHill = decipher


data AffineCipher = AffineCipher Int Int deriving (Show)

-- a must be coprime with nAlphabet ie a = 1,3,5,7,9,11,15,17,19,21,23,25
instance Cipher AffineCipher where
    cipher (AffineCipher a b) =  L.map (affineShift a b)
    decipher (AffineCipher a b) = L.map (affineDeshift a b)

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

type Grid = String

bifidAlphabet = "ABCDEFGHIKLMNOPQRSTUVWXYZ"

bifidSquare :: Grid
bifidSquare = "THEORYABCDFGIKLMNPQSUVWXZ"
 
indices :: [(Int, Int)]
indices = zip (L.concatMap (replicate 5) [1..5]) (cycle [1..5])
 
toIndex :: Grid -> Char -> (Int, Int)
toIndex bs c = M.fromList (zip bs Cipher.indices) M.! c
 
fromIndex :: Grid -> [Int] -> Char
fromIndex bs [x, y] = M.fromList (zip Cipher.indices bs) M.! (x, y)
fromIndex bs _      = undefined
 
-- Get rid of white space
prepare :: String -> String -> String
prepare bs = L.filter (`elem` bs) . H.replace "J" "I" . L.map toUpper
 
-- This does Bifid where the period is the whole of the plain text
data BifidCipher = BifidCipher Grid

instance Cipher BifidCipher where
    cipher (BifidCipher bs) = bifidCipher bs
    decipher (BifidCipher bs) = bifidDecipher bs

bifidCipher :: Grid -> String -> String
bifidCipher bs = L.map (fromIndex bs) . sliceVertical 2 . uncurry (++) .
         unzip . L.map (toIndex bs) . (prepare bs)
 
bifidDecipher :: Grid -> String -> String
bifidDecipher bs xs = L.map (fromIndex bs) . sliceHorizontal (length xs) .
            concatMap ((\(x, y) -> [x, y]) . (toIndex bs)) $ prepare bs xs
       
-- Bifid with a period is just the application of bifid to the plain text
-- split into pieces of length = the period       
data BifidPeriodCipher = BifidPeriodCipher Int Grid       
        
instance Cipher BifidPeriodCipher where
    cipher (BifidPeriodCipher n bs) = bifidPeriodCipher n bs
    decipher (BifidPeriodCipher n bs) = bifidPeriodDecipher n bs

bifidPeriodCipher::Int->Grid->String->String
bifidPeriodCipher _ _ [] = []
bifidPeriodCipher n bs pt = (cipher (BifidCipher bs) (take n pt)) ++ (bifidPeriodCipher n bs (drop n pt))
        
bifidPeriodDecipher::Int->Grid->String->String
bifidPeriodDecipher n bs ct = (decipher (BifidCipher bs) (take n ct)) ++ (bifidPeriodDecipher n bs (drop n ct))
        
-- With Bifid if we have a crib we can work out various rules like
-- Tx=Ty=Hx etc, where the x mewans the x coordinate of T in the bifid square.
-- If we can work out these rules in code then we could come up with a list of
-- potential squares (which might be shorter than 25!).
-- We could then decipher with these and test the resulting plain text for 
-- correlations (to 1.73) and so choose a best square......

-- An axis is either X or Y
data Axis = X | Y deriving(Show, Eq, Ord)
-- A coordinate is a letter with an axis
type Coord = (Char, Axis)
-- A rule is a set of Coords that must all be equal
type Rule = Set Coord
-- A crib is a String and a position which it starts at
data Crib = Crib {cx::Maybe Int, cy::Maybe Int, ctxt::String} deriving(Show)
data Position = Position {px::Int, py::Int} deriving(Show, Eq)

-- The choices available for a letter
type CharChoices = (Char, [Position])

-- The choices available for all the letters
initialChoices::[CharChoices]
initialChoices = L.map (\c-> (c,[Position x y| x<-[1..5], y<-[1..5]]) ) bifidAlphabet

-- prune the choice available for a letter with a rule and a value for that rule
prune::CharChoices->(Rule, Int)->CharChoices
prune (c,ps) (cos, v) = (c, S.foldl (\acc co -> L.filter (cIsV co) acc) ps cos)
    where
        cIsV (coc,X) p = if c == coc then px p == v else True
        cIsV (coc,Y) p = if c == coc then py p == v else True
   
-- pruns a set of choices available for soem letter with a set of rule and their values
prunes::[CharChoices]->[(Rule, Int)]->[CharChoices]
prunes cs rvs = L.map (\c -> L.foldl (\acc rv-> prune acc rv) c rvs) cs
    
-- The crib (plain text) turns in to just the x coords of the letters
cribToCoords::String->[Coord]
cribToCoords pt = L.map (\c-> (c, X)) pt

-- This function takes a crib and a piece of cipher text and returns a set of rules
-- It just works for the bifid at present and assumes no period
-- It makes a equivalence lists of all the char x/ys that have to be the same
applyCrib::Crib->String->[Rule]
applyCrib crib ct = consolidate $ (zipWith (\x y -> S.fromList [x, y]) cribXs ctXs) ++ (zipWith (\x y -> S.fromList [x, y]) cribYs ctYs)
    where       
        n=length $ ctxt crib
        cribXs = L.map (\c-> (c, X)) $ ctxt crib
        cribYs = L.map (\c-> (c, Y)) $ ctxt crib
        ctXs = case cx crib of 
                Nothing -> []
                Just x  -> ctCoords (drop x ct) -- the crib starts at x, so make the list from there
        ctYs = case cy crib of 
                Nothing -> []
                Just y  -> ctCoords (drop y ct) -- the crib starts at y, so make the list from there
        -- From the string take n/2 chars, then make a list (c1,X), (c1,Y), (c2,X) ...
        ctCoords bit = L.concatMap (\c-> [(c, X), (c,Y)]) $ take (n `quot` 2) bit
        

-- Takes a list of rules and consolidates the sets with an intersection
consolidate ::[Rule] -> [Rule]
consolidate = L.foldl comb []
  where comb [] s' = [s']
        comb (s:ss) s'
          | S.null (s `S.intersection` s') = s : comb ss s'
          | otherwise = comb ss (s `S.union` s')
        
coordValue::Grid->Coord->Int
coordValue sq (c, a) = case a of 
                        X -> fst $ toIndex sq c
                        Y -> snd $ toIndex sq c
                        
consistant::Grid->Rule->Bool
consistant g r = n==1
    where
        n = S.size $ S.map (\x -> coordValue g x) r        
        
grids::[Rule]->[[CharChoices]]
grids rs = L.map (\cs -> prunes initialChoices cs) $ L.map (zip rs) $ allChooses (length rs) [1..5]
        
-- Checks that no letters have the same place
validGrid::[CharChoices]->Bool
validGrid cs = length ps == (length $ nub ps)
    where
        ss = L.filter (\(c, ps) ->length ps == 1) cs
        ps = L.map snd ss
        
allChooses::Int->[a]->[[a]]
allChooses 0 es = [[]]
allChooses n es =  [x:as | x<-es, as<-allChooses (n-1) es]

        
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
      LS.chunksOf 5 . (`L.union` "ABCDEFGHIKLMNOPQRSTUVWXYZ") . nub . process
      --LS.chunksOf 5 . (`L.union` L.delete 'J' ['A'..'Z']) . nub . process
 
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
playfairDecipher k = concatMap (\[x,y] -> bigram k (-1) x y) . LS.chunksOf 2
 
-- This does Bifid where the period is the whole of the plain text
data PlayfairCipher = PlayfairCipher Key

instance Cipher PlayfairCipher where
    cipher (PlayfairCipher bs) = playfairCipher bs
    decipher (PlayfairCipher bs) = playfairDecipher bs

--data PlayfairCrib = Crib {start::Int, txt::String} deriving(Show)

--applyPlayfairCrib::PlayfairCrib->String->[Rule]
--applyPlayfairCrib crib ct = consolidate $ (zipWith (\x y -> S.fromList [x, y]) cribXs ctXs) ++ (zipWith (\x y -> S.fromList [x, y]) cribYs ctYs)
--    where       
--        n=length $ txt crib
--        ctbgs = LS.chunksOf 2 $ take n $ drop (start crib) ct
--        cribbgs = LS.chunksOf 2 $ pftxt crib
                
        
-- This is a transpoisiotn cipher where the 
data TranspositionCipher = TranspositionCipher [Int]

instance Cipher TranspositionCipher where
    cipher (TranspositionCipher xs) pt = L.concatMap (\w-> L.map (\x-> w!!x) xs) $ L.transpose ws
        where
            m = length xs
            ws = chunksOf m pt
    decipher (TranspositionCipher xs) ct = L.concatMap (\w-> L.map (\x-> w!!x) xs) $ L.transpose ws
        where
            m = length xs
            n = length ct
            ws = chunksOf (n `div` m) ct



