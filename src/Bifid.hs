module Bifid(
  BifidCipher(..)
  , BifidPeriodCipher(..)
  , bifidCipher
  , bifidDecipher
  , bifidSquare
  , indices
  , toIndex
  , fromIndex
  , prepare
  , Crib(..)
  , Axis(..)
  , Coord(..)
  , Rule(..)
  , applyCrib
  , coordValue
  , grids
  , consolidate
  , initialChoices
  , prunes
  , prune
  , allChooses
  , validGrid
  , scoreGrid
  , bifidAlphabet
) where

import Data.Char (toUpper)
import Data.List (concatMap, filter, map, foldl, nub)
import Data.Map (fromList, (!), )
import qualified Data.Set as S hiding ((!), filter)
import Data.String.Utils (replace)
import Data.List.HT (sliceVertical, sliceHorizontal)
import Data.ByteString.Lazy.Char8 (pack)


import Cipher
import Analysis (nord, nchr)
import Quadgram (qscore)

keyGrid :: String -> Grid
keyGrid key = key ++ filter (\c -> c `notElem` key) bifidAlphabet

scoreGrid :: Grid -> String -> Double
scoreGrid g ct = qscore $ pack pt
  where
    cr = BifidPeriodCipher 4 g
    pt = decipher cr ct


qtest = qscore $ pack bifidAlphabet

type Grid = String

bifidAlphabet = "ABCDEFGHIKLMNOPQRSTUVWXYZ"

bifidSquare :: Grid
bifidSquare = "THEORYABCDFGIKLMNPQSUVWXZ"

indices :: [(Int, Int)]
indices = zip (concatMap (replicate 5) [1..5]) (cycle [1..5])

toIndex :: Grid -> Char -> (Int, Int)
toIndex bs c = fromList (zip bs indices) ! c

fromIndex :: Grid -> [Int] -> Char
fromIndex bs [x, y] = fromList (zip indices bs) ! (x, y)
fromIndex bs _      = undefined

-- Get rid of white space
prepare :: String -> String -> String
prepare bs = filter (`elem` bs) . replace "J" "I" . fmap toUpper

-- This does Bifid where the period is the whole of the plain text
data BifidCipher = BifidCipher Grid

instance Cipher BifidCipher where
    cipher (BifidCipher bs) = bifidCipher bs
    decipher (BifidCipher bs) = bifidDecipher bs

bifidCipher :: Grid -> String -> String
bifidCipher bs = map (fromIndex bs) . sliceVertical 2 . uncurry (++) .
         unzip . map (toIndex bs) . (prepare bs)

bifidDecipher :: Grid -> String -> String
bifidDecipher bs xs = map (fromIndex bs) . sliceHorizontal (length xs) .
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
bifidPeriodDecipher _ _ [] = []
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
type Rule = S.Set Coord
-- A crib is a String and a position which it starts at
data Crib = Crib {cx::Maybe Int, cy::Maybe Int, ctxt::String} deriving(Show)
data Position = Position {px::Int, py::Int} deriving(Show, Eq)

-- The choices available for a letter
type CharChoices = (Char, [Position])

-- The choices available for all the letters
initialChoices::[CharChoices]
initialChoices = map (\c-> (c,[Position x y| x<-[1..5], y<-[1..5]]) ) bifidAlphabet

-- prune the choice available for a letter with a rule and a value for that rule
prune::CharChoices->(Rule, Int)->CharChoices
prune (c,ps) (cos, v) = (c, S.foldl (\acc co -> filter (cIsV co) acc) ps cos)
    where
        cIsV (coc,X) p = if c == coc then px p == v else True
        cIsV (coc,Y) p = if c == coc then py p == v else True

-- pruns a set of choices available for soem letter with a set of rule and their values
prunes::[CharChoices]->[(Rule, Int)]->[CharChoices]
prunes cs rvs = fmap (\c -> foldl (\acc rv-> prune acc rv) c rvs) cs

-- The crib (plain text) turns in to just the x coords of the letters
cribToCoords::String->[Coord]
cribToCoords pt = fmap (\c-> (c, X)) pt

-- This function takes a crib and a piece of cipher text and returns a set of rules
-- It just works for the bifid at present and assumes no period
-- It makes a equivalence lists of all the char x/ys that have to be the same
-- This doesn't work when it's a BifidPeriod cipher.
applyCrib::Crib->String->[Rule]
applyCrib crib ct = consolidate $ (zipWith (\x y -> S.fromList [x, y]) cribXs ctXs) ++ (zipWith (\x y -> S.fromList [x, y]) cribYs ctYs)
    where
        n=length $ ctxt crib
        cribXs = fmap (\c-> (c, X)) $ ctxt crib
        cribYs = fmap (\c-> (c, Y)) $ ctxt crib
        ctXs = case cx crib of
                Nothing -> []
                Just x  -> ctCoords (drop x ct) -- the crib starts at x, so make the list from there
        ctYs = case cy crib of
                Nothing -> []
                Just y  -> ctCoords (drop y ct) -- the crib starts at y, so make the list from there
        -- From the string take n/2 chars, then make a list (c1,X), (c1,Y), (c2,X) ...
        ctCoords bit = concatMap (\c-> [(c, X), (c,Y)]) $ take (n `quot` 2) bit


-- Takes a list of rules and consolidates the sets with an intersection
consolidate ::[Rule] -> [Rule]
consolidate = foldl comb []
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
grids rs = fmap (\cs -> prunes initialChoices cs) $ fmap (zip rs) $ allChooses (length rs) [1..5]

-- Checks that no letters have the same place
validGrid::[CharChoices]->Bool
validGrid cs = length ps == (length $ nub ps)
    where
        ss = filter (\(c, ps) ->length ps == 1) cs
        ps = fmap snd ss

allChooses::Int->[a]->[[a]]
allChooses 0 es = [[]]
allChooses n es =  [x:as | x<-es, as<-allChooses (n-1) es]
