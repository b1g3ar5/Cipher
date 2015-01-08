import Control.Monad.Logic
import Control.Monad.Identity
import Control.Monad.Reader
 
import Data.List
import Data.Maybe
import Data.Ord
import Data.Ix
import qualified Data.Map as Map
import System.Environment
 
import Dict

main :: IO ()
main = do 
        dd <- getDict
        let b = observeMany 10 . (allWords dd 0 test2) $ [[]]
        putStrLn $ show b


type Words a = Logic a


-- allWords adds a new word to the list and then call itself until there is no text left
allWords :: Dict -> Int -> String -> [[String]] -> Words [[String]]
allWords dd n pt ss | n >= (length pt) =   return $ tail ss
                    | otherwise       = do  next <- msum . map return $ nextWord dd pt n
                                            allWords dd (n + length next) pt $ ss ++ [[next]]

-- nextWord uses the nword to find words in the dictionary at the
-- current location and returns a list of them all 
nextWord :: Dict -> String -> Int -> [String]
nextWord dd pt n = concat $ concatMap (\d -> map toWords d) ds
    where
        ds = map (\i -> nword dd $ take i $ drop n pt) $ reverse [1..8]


-- Parses a word of length n - see Tree for waht a word is
nword :: Dict -> String -> Dict
nword dd ss = subDict ss dd


test1 = "thAtyouheonwithdoatby"
test2 = "heAswithyourateprobably"


{-

-- This function works out next moves from board size, current moves and current cell
-- Parameters are:
-- n = board size
-- b = map of moves alreay made
-- (Int, Int) = current cell
-- The succs function ...
--   - works out 4 possible moves
--   - prunes moves off hte edge of the board
--   - returns the rest
successors :: Int -> Map.Map (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
successors n b = sortWith (length . succs) . succs
 where sortWith :: (Ord b) => (a -> b) -> [a] -> [a]
       sortWith f = map fst . sortBy (comparing snd) . map (\x -> (x, f x))
       succs :: (Int, Int) -> [(Int, Int)]
       succs (i,j) = [ (i', j') | (dx,dy) <- [(1,2),(2,1)]
                                , i' <- [i+dx,i-dx] , j' <- [j+dy, j-dy]
                                , isNothing (Map.lookup (i',j') b)
                                , inRange ((1,1),(n,n)) (i',j') ]
 
-- This function adds a new move to a tour and recursively calls itself until the route goes to all cells
--
-- The parameters are:
-- n = board size
-- k = current route length
-- s = current cell
-- b = current route
-- tour :: (MonadPlus m) => Int -> Int -> (Int, Int) -> Map.Map (Int, Int) Int -> m (Map.Map (Int, Int) Int)
tour :: Int -> Int -> (Int, Int) -> Map.Map (Int, Int) Int -> Logic (Map.Map (Int, Int) Int)
tour n k s b | k > n*n   = return b
             | otherwise = do next <- msum . map return $ successors n b s
                              tour n (k+1) next $ Map.insert next k b

showBoard :: Int -> Map.Map (Int, Int) Int -> String
showBoard n b = unlines . map (\i -> unwords . map (\j ->
                  pad . fromJust $ Map.lookup (i,j) b) $ [1..n]) $ [1..n]
 where k = ceiling . logBase 10 . fromIntegral $ n*n + 1
       pad i = let s = show i in replicate (k-length s) ' ' ++ s
 

main :: IO ()
main = do (n:_) <- map read `fmap` getArgs
           let b = observe . (tour n 2 (1,1)) $ Map.singleton (1,1) 1
           putStrLn $ showBoard n b


-}



