module Cribs
    (
        KeyMap(..)
        , zeroMap
        , idMap
        , caesarMap
        , keywordMap
        , cribMap
        , apply
        --, apply2
    ) where

import System.IO
import Control.Monad
import Data.Monoid
import Data.Array as A
import Data.Char
import Data.Maybe
import Data.Map as M
import Data.List as L
import Text.Printf
import Data.Tuple (swap)
--import Matrix
import Analysis
import CTexts


-- Usage is as follows:

-- cribMap ct pt makes a KeyMap out of the given crib (cipher test and plain text)

-- apply keyMap ct applies the key map to the cipher text to work out plain text, * is used where it
-- doesn't know the letter


newtype KeyMap a = KeyMap (Map a Char) deriving (Show)

zeroMap::Ord a => KeyMap a
zeroMap = KeyMap $ fromList []

idMap :: KeyMap Char
idMap = KeyMap $ fromList $ L.map (\x->(nchr x,toLower $ nchr x)) [0..25]

-- Note the mappend is left biased when there is a conflict
instance Ord a => Monoid (KeyMap a) where
    mempty = zeroMap
    mappend (KeyMap k) (KeyMap l) = KeyMap $ M.union k l

caesarMap::KeyMap Char
caesarMap = KeyMap $ fromList $ L.map (\x->(nchr x,toLower $ nchr $ mod (x+3) nAlphabet)) [0..25]


-- Doesn't check for repeating letters in the key!!!
keywordMap::String->KeyMap Char
keywordMap key = KeyMap $ fromList $ zipWith (,) alphabet $ key++rest
    where
        rest = alphabet L.\\ key

cribMap::Ord a => String -> [a] -> KeyMap a
cribMap pt ct = KeyMap $ fromList $ zipWith (\p c -> (c,toLower p)) pt ct

apply::Ord a => KeyMap a -> [a] -> String
apply (KeyMap m) ct = L.map decode ct
    where
        decode c = case M.lookup c m of
                        Nothing -> '*'
                        Just p -> p

{-
apply2::Ord a => KeyMap a -> [a] -> String
apply2 (KeyMap m) ct = L.map decode ct
    where
        decode c = case M.lookup c m of
                        Nothing -> c
                        Just p -> p
-}
