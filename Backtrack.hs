module Backtrack
    (
        PList(..)
        , ptake
        , test1
    ) where

import Control.Monad.Logic
import Control.Monad.Identity
 
import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Data.Char
import Control.Monad
import Control.Monad.Logic
import Data.Monoid
import Data.List hiding ()
 
import Tree

test1 = map (map toChr) $  head $ reverse $ map fst $ head $ unO $ parse pwords "th*tyouheonwithdoatby"


data PList a = P { unO :: [[a]] } deriving (Show,Eq)

instance Functor PList where
    fmap f (P xs) = P (fmap (fmap f) xs)

instance Monoid (PList a) where
    mempty = P []
    (P l1) `mappend` (P l2) = P (l1 ++ l2)


headm :: Monoid m => [m] -> m
headm (a:as) = a
headm [] = mempty

tailm :: Monoid m => [m] -> [m]
tailm (a:as) = as
tailm [] = []

zipm :: Monoid m => [[m]] -> [m]
zipm ms | all null ms = []
zipm ms = let
    heads = map headm ms
    tails = map tailm ms
    h = mconcat heads
    t = zipm (filter (not . null) tails)
    in h : t

ptake:: Int -> PList m -> PList m
ptake 0 _ = P []
ptake n (P a) = P (take n a)

instance Monad PList where
    return x = P [[x]]
    x >>= f = let P xs = (fmap (unO . f) x) in P (join xs) where
        join []     = []
        join (m:ms) = let
            part1 = zipm m
            part2 = join ms
            in headm part1 : zipm [tailm part1,part2]


instance MonadPlus PList where
    mzero = P []
    mplus (P xs) (P ys) = P (zipm [xs,ys])


newtype Parser a = Parser (String -> PList (a,String))

parse (Parser f) x = f x

instance Monad Parser where
    return a = Parser (\cs -> P [[(a,cs)]])
    p >>= f = Parser (\cs -> do
                 (a,cs') <- parse p cs
                 parse (f a) cs')

instance MonadPlus Parser where
    mzero = Parser (\cs -> mzero)
    p `mplus` q = Parser (\cs -> parse p cs `mplus` parse q cs)

instance Functor Parser where
    fmap = parserMap

parserMap :: (a -> b) -> Parser a -> Parser b
parserMap f (Parser fa) = Parser $ \s -> do
                                            (va, sa) <- fa s 
                                            return (f va, sa)

instance Applicative.Applicative Parser where
    pure = return
    (<*>) = ap -- TODO: Can this be optimized?

instance Applicative.Alternative Parser where
    empty = mzero
    (<|>) = mplus


ss = "thatyouheonwithdoatby\n"

-- Parses n copies of a parser
pcount :: Int -> Parser a -> Parser [a]
pcount n p           | n <= 0    = return []
                    | otherwise = sequence (replicate n p)

-- Parses a character
item :: Parser Char
item = Parser (\cs -> case cs of
                    "" -> mzero
                    (c:cs) -> P [[(c,cs)]]
               )

litem :: Logic Char
litem = LogicT (\f -> mzero) -- f::a->r->r

-- Parses a character that satisfies a consition
sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c then return c else mzero

-- Parses a particular character
char :: Char -> Parser Char
char c = sat (c ==)

-- Parses a string of length n
string  :: Int -> Parser String
string n = pcount n item

-- Parses a word of length n - see Tree for waht a word is
nword :: Int -> Parser W
nword n = do
    ss <- string n
    let dd = subDict (toWord ss) smallDict
    if ((dcount dd) > 0) then return (toWord ss) else mzero

-- Tries a list of parsers until one works
choice :: [Parser a] -> Parser a
choice ps = foldr (Applicative.<|>) mzero ps

-- Gets a word by trying words of increasing size
word :: Parser W
word = choice ps
        where
            ps = map nword [1..5]

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = scan
                    where
                      scan  = do{ end; return [] }
                            Applicative.<|>
                              do{ x <- p; xs <- scan; return (x:xs) }

try :: Parser a -> Parser a
try (Parser fa) = Parser $ \s -> fa s

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = try (do{ c <- try p; mzero }
                           Applicative.<|> return ()
                      )

eof :: Parser ()
eof = notFollowedBy item 

pwords :: Parser [W]
pwords = manyTill word $ eof       


