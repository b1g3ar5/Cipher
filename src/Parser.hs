
module Parser
    (
        PList(..)
        , ptake
        , test1
    ) where

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Data.Char
import Control.Monad
import Data.Monoid
import Data.List

import Tree


test1 = map (map toChr) $  head $ reverse $ map fst $ head $ unO $ parse (Parser.words) "th*tyouheonwithdoatby"


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

-- Not sure what this is for?!
--penalty :: PList ()
--penalty = P [[],[()]]


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
        (c:cs) -> P [[(c,cs)]])

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

words :: Parser [W]
words = manyTill word $ eof       


{-

ex1 = do
    x <- P $ map (\x -> [x]) [1..]
    y <- P $ map (\y -> [y]) [1..]
    z <- P $ map (\z -> [z]) [1..]
    guard $ x*x+y*y==z*z
    return $ (x,y,z)



lowers = "1234567890-=/"
uppers = "!@#$%^&*()_+?"
lower x = lookup x (zip uppers lowers)
upper x = lookup x (zip lowers uppers)

upperChar x = case upper x of
    Nothing -> mzero
    Just y -> char y >> return x

lowerChar x = case lower x of
    Nothing -> mzero
    Just y -> char y >> return x

avoid :: Parser ()
avoid = Parser $ \cs -> do
            penalty
            return ((),cs)

keyChar x = char x `mplus` (avoid >> upperChar x) `mplus` (avoid >> lowerChar x)

digit = do
            x <- foldl mplus mzero (map keyChar "0123456789")
            return (fromIntegral (ord x-ord '0'))

number1 :: Integer -> Parser Integer
number1 m = return m `mplus` do
                n <- digit
                number1 (10*m+n)

number :: Parser Integer
number = do
            n <- digit
            number1 n

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a 
chainl p op a = (p `chainl1` op) `mplus` return a 
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a 
p `chainl1` op = do {a <- p; rest a} 
    where 
        rest a = (do
                    f <- op
                    b <- p
                    rest (f a b)) `mplus` return a 

shouldHave c = keyChar c `mplus` (avoid >> return c)

expr = term `chainl1` addop 
term = monomial `chainl1` mulop 
monomial = factor `chainl1` powop
factor = number `mplus` do {shouldHave '('; n <- expr; shouldHave ')'; return n} 
powop = keyChar '^' >> return (^)
addop = do {keyChar '+'; return (+)} `mplus` do {keyChar '-'; return (-)} 
mulop = do {keyChar '*'; return (*)} `mplus` do {keyChar '/'; return (div)} 

end :: Parser ()
end = Parser $ \cs ->
        if null cs then P [[((),"")]] else mzero


completeExpr :: Parser Integer
completeExpr = do
                n <- expr
                end
                return n

ex2::PList (Integer, String)
ex2 = parse completeExpr "2^(1+3)"

-}
