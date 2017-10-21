module Tree
    (
        Tree(..)
        , L(..)
        , Label(..)
        , Dict(..)
        , W(..)
        , getDict
        , subDict
        , isWord
        , toWord
        , addWord
        , tsum
        , dcount
        , toWords
        , foldMap
        --, go
        , smallDict
        , toWordAndFreq

    ) where

import System.IO
import Control.Monad
import Control.Applicative
import Data.Array as A
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Map as M
import Data.List as L
import Text.Printf
import Data.Tuple (swap)
import Data.Foldable
import Debug.Trace


-- SEEMS LIKE A TREE FOR WORDS FROM A DICTIONARY - SO THAT THEY CAN BE LOOKED UP
-- LOOKS UNFINISHED

data Tree a = Tree {label::a, kids::[Tree a]}

showTree :: (Show a) => Tree a -> [String]
showTree (Tree l []) = [show l]
showTree (Tree l ks) = L.map (show l ++) $ L.concatMap Tree.showTree ks

toWords :: Tree Label -> [String]
toWords (Tree l []) = [Tree.showChar l]
toWords (Tree l ks) = L.map (Tree.showChar l ++) $ L.concatMap toWords ks

instance (Show a) => Show (Tree a) where
    show t@(Tree l ks) = L.concat $ L.intersperse ", " $ Tree.showTree t

go :: Tree Label -> [String]
go t = L.map (\c-> [toChr $ ch $ label $ t] ++ [c] ) $ L.map (toChr.ch.label) $ kids t

instance Functor Tree where
  fmap f (Tree x y) = Tree (f x) (L.map (fmap f) y)


-- This version works on the intersection of the Tree structures
instance Applicative Tree where
    pure x = Tree x (repeat $ pure x)
    (Tree f  fs) <*> (Tree x xs) = Tree (f x) (zipWith (<*>) fs xs)

instance Monad Tree where
    return a = Tree a []
    -- There's 2 bits here, the kids from applying f to the label and then the
    -- kids from maping f onto the kids in the tree
    t >>= f = Tree fa $ fks ++ (L.map (>>= f) $ kids t)
        where
            Tree fa fks = f $ label t



instance Foldable Tree where
    foldMap f (Tree l ks) = L.foldl mappend mempty $ (f l) : (L.map (foldMap f) ks)

instance Monoid Int where
    mempty = 0
    m `mappend` n = m+n

tsum::Dict -> Int
tsum dd = L.sum $ L.map (foldMap count) dd

class Functor w => Comonad w where
   (=>>)    :: w a -> (w a -> b) -> w b
   coreturn :: w a -> a
   cojoin     :: w a -> w (w a)
   x =>> f = fmap f (cojoin x)

instance Comonad Tree where
    coreturn (Tree a ks) = a
    cojoin  t = Tree t (L.map cojoin $ kids t)
    --extract (Tree l ks) = l
    --duplicate w@(Tree l as) = Tree w (map duplicate as)


-- Letters, '*' is a special wildcard letter
-- This type is so that I can write new Eq and Ord classes
-- where * == any character, ie. a wildcard
data L = L {toChr::Char}

instance Show L where
    show l = [toChr l]

isWildCard (L c) = c == '*'

instance Eq L where
    (==) (L a) (L b) = if (a=='*') || (b=='*') then True else (a==b)

instance Ord L where
    compare (L a) (L b) = if (a=='*') || (b=='*') then EQ else (compare a b)

-- Words!!
type W = [L]

toWord::[Char]->W
toWord [] = []
toWord (c:cs) = (L $ toUpper c):(toWord cs)

-- We'll make a tree of these
-- A letter, and isWord flag and a words score (which will equal the sum of the kids
-- plus 1 if isWord=True (which means this node is the end of a word)
data MLabel = MLabel {mch::L, mwd::Bool, mn::Maybe Int} deriving (Show)
data Label = Label {ch::L, wd::Bool, freq::Double, count::Int}

instance Show Label where
    show (Label ch wd f n) = (show ch) ++ ", " ++ (show n) ++ ", " ++ (show f)

showChar :: Label -> String
showChar (Label ch wd f n) = (show ch)


minc::Maybe Int->Maybe Int
minc = liftM2 (+) (Just 1)

type Dict = [Tree Label]

dcount::Dict->Int
dcount d = L.sum $ L.map (count . label) d

-- Add a word to a Dict
-- Need to do single letters forst so that we can set the wd boolean to TRUE for the end of a word
-- Added a frequency, so that likelihood of sentences can be calculated
-- Frequency will be zero except for the end of a word (ie, when wd=True
addWord::Dict->W->Double->Dict
addWord d [] f =  d
addWord [] w@(l:[]) f =  [Tree (Label l True f 1) []]
addWord [] w@(l:ls) f =  [Tree (Label l False 0.0 $ 1) $ addWord [] ls f]
addWord d@(t:ts) w@(l:[]) f =  case compare (ch $ lt) l of
                                 -- if equal increment and set to TRUE because it's a word
                                 -- EQ -> [Tree (Label l True $ 1 ) (kids t)] ++ ts
                                 EQ -> [Tree (Label l True f $ 1 + count lt) (kids t)] ++ ts
                                 -- c is bigger so add to the others
                                 LT -> [t] ++ addWord ts w f
                                 -- c is smaller so make a new node in front of the others
                                 GT -> [Tree (Label l True f $ 1) []] ++ d
                            where
                                lt = label t
addWord d@(t:ts) w@(l:ls) f =  case compare (ch $ lt) l of
                                 -- if equal increment, but don't set to true, it's not the end of  word
                                 -- EQ -> [Tree (Label l (wd lt) $ count lt) (addWord (kids t) ls)] ++ ts
                                 EQ -> [Tree (Label l (wd lt) (freq lt) $ 1 + count lt) (addWord (kids t) ls f)] ++ ts
                                 -- c is bigger so add to the others
                                 LT -> [t] ++ addWord ts w f
                                 -- c is smaller so make a new node in front of the others
                                 GT -> [Tree (Label l False 0.0 $ 1) $ addWord [] ls f] ++ d
                            where
                                lt = label t


isWord::Dict->W->Bool
isWord _ [] = False
isWord [] _ = False
isWord dd@(t:ts) chs@(c:[]) = case compare (ch $ lt) c of
                                     EQ -> if isWildCard c then (L.or $ L.map (wd . label) dd ) else wd $ lt
                                     LT -> isWord ts chs
                                     GT -> False
                            where
                                lt = label t
isWord dd@(t:ts) chs@(c:cs) = case compare (ch $ lt) c of
                                     EQ -> if isWildCard c then (L.or $ L.map (\t-> isWord (kids t) cs) dd) else isWord (kids t) cs -- doesn't work for '*' because it will == all the ts
                                     LT -> isWord ts chs
                                     GT -> False
                            where
                                lt = label t


-- This function works out a Dict of the words that fit the [L] word
-- which may have wildcards ('*') in. eg'*AT' will return a dictionary with
-- "CAT", "BAT", "MAT" in.
subDict::W->Dict->Dict
subDict _ [] = []
subDict [] _ = []
subDict chs@(c:[]) (t:ts) = case compare (ch $ lt) c of
                                     EQ -> (if (wd lt) then [Tree (Label (ch $ lt) (wd lt) (freq lt) $ 1) []] else []) ++ subDict chs ts
                                     LT -> subDict chs ts
                                     GT -> []
                            where
                                lt = label t
subDict chs@(c:cs) (t:ts) = case compare (ch $ lt) c of
                                     EQ -> case sd of -- case (subDict cs $ kids t) of
                                            []        -> subDict chs ts
                                            -- otherwise -> [Tree (Label (ch $ lt) (wd lt) $ 0) $ sd] ++ subDict chs ts
                                            otherwise -> [Tree (Label (ch $ lt) (wd lt)  (freq lt) $ dcount sd) $ sd] ++ subDict chs ts
                                           where
                                            sd = subDict cs $ kids t
                                     LT -> subDict chs ts
                                     GT -> []
                            where
                                lt = label t
                                -- n = L.map



getDict::IO (Dict)
getDict = do
    ls<-readFile("./Data/Gutenberg2006.txt")
    let wds = L.map toWordAndFreq $ lines ls
    let n = length wds
    let t = []::Dict
    let s = L.foldl (\d wf -> addWord d (fst wf) (snd wf)) ([]::Dict) wds
    return s

-- Note the freqnecy is -log(prob)
-- So we can add them and the most likely will be the minimum
toWordAndFreq ss = (toWord w, f)
    where
        ws = words ss
        w = ws!!1
        f =  -(log $ (read $ ws!!2)*0.000000001) -- the number in the file is occurrences in 1bn words

smallDict :: Dict
smallDict = L.foldl (\d w -> addWord d w 0.0) ([]::Dict) $ L.map toWord words100


words100 :: [String]
words100 = [
    "the",
    "be",
    "of",
    "and",
    "a",
    "in",
    "to",
    "have",
    "it",
    "to",
    "for",
    "i",
    "that",
    "you",
    "he",
    "on",
    "with",
    "do",
    "at",
    "by",
    "not",
    "this",
    "but",
    "from",
    "they",
    "his",
    "that",
    "she",
    "or",
    "which",
    "as",
    "we",
    "an",
    "say",
    "will",
    "would",
    "can",
    "if",
    "their",
    "gov",
    "what",
    "there",
    "all",
    "get",
    "her",
    "make",
    "who",
    "as",
    "out",
    "up",
    "see",
    "know",
    "time",
    "take",
    "them",
    "some",
    "could",
    "so",
    "him",
    "year",
    "into",
    "its",
    "then",
    "think",
    "my",
    "come",
    "than",
    "more",
    "about",
    "now",
    "last",
    "your",
    "me",
    "no",
    "other",
    "give",
    "just",
    "should",
    "these",
    "people",
    "also",
    "well",
    "any",
    "only",
    "new",
    "very",
    "when",
    "may",
    "way",
    "look",
    "like",
    "use",
    "her",
    "such",
    "how",
    "because",
    "when",
    "as",
    "good",
    "find"]
