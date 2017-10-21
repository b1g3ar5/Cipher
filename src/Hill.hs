module Hill (

    ) where
    
import System.IO
import Control.Monad
import Data.Array as A
import Data.Char
import Data.Maybe
import Data.List as L --hiding (transpose)
import Text.Printf


-- CONTAINS SOME MATRIX STUFF - PROBABLY POOR, BUT IT TAUGHT ME SOME MANIPULATION
-- DONE SO THAT I COULD HAVE MATRICES MULTIPLICAIOT WITH MOD.

-- s is the base x is the number, this is x `mod` s
-- the answesr is between 0 ans s-1
myMod::(Num t, Ord t, Integral t) => t->t->t
myMod s x   | x<0 = (x `mod` s) + s
            | x>s = x `mod` s
            
-- Works out an inverse of x in mod s
modinv s x = head $ filter (\z-> ((myMod s (z*x))==1)) [0..(s-1)]

data Vec t = Vec {unVec::[t]} deriving (Show)

--instance Num Vector t where
--	(+) (Vector as) (Vector bs) = fmap (\a-> fmap ((+) a) bs) as
--	(-) (Vector as) (Vector bs) = fmap (\a-> fmap ((-) a) bs) as

vecHead (Vec xs) = head xs


instance Functor Vec where
	fmap f (Vec ts) = Vec (map f ts)


-- The function truncates to the shortest length
dot::Num t => Vec t -> Vec t -> t
dot (Vec x1) (Vec x2) = sum $ zipWith (*) x1 x2


data Mat t = Mat { rows::[Vec t] } deriving (Show)
   
instance Functor Mat where
	fmap f (Mat irs) = Mat  ors
		where
			ors = fmap (\r->Vec $ fmap f r) $ map unVec irs

   
matMod::(Num t, Ord t, Integral t)=>t->Mat t -> Mat t
matMod s = fmap (myMod s)
          
pushVec::Vec t->Mat t->Mat t
pushVec (Vec xs) (Mat irs) = Mat rs
    where
        rs = irs ++ [Vec xs]
		
pushColVec::Vec t->Mat t->Mat t
pushColVec (Vec xs) (Mat irs) = Mat rs
    where
        rs = zipWith push xs irs
		
push::t->Vec t->Vec t
push x (Vec xs) = Vec (xs++[x])
              
matSize :: Mat t -> Int
matSize = length . rows     

coords :: Mat t -> [[(Int, Int)]]
coords = zipWith (map . (,)) [0..] . map (zipWith const [0..]) . (map unVec) . rows

delmatrix :: Int -> Int -> Mat t -> Mat t
delmatrix i j m = dellist i $ Hill.transpose $ dellist j $ Hill.transpose m
  where
    dellist i (Mat xs) = Mat $ take i xs ++ drop (i + 1) xs        

transpose::Mat t -> Mat t
transpose (Mat rs) = Mat $ map (\c-> Vec c) cs
    where
        cs = L.transpose $ map unVec rs
                
matMult::Num t=>Mat t ->Mat t ->Mat t
matMult m1@(Mat r1s) m2@(Mat r2s) = Mat $ [Vec $ map (dot c2) r1s | c2<-rows $ Hill.transpose m2]

determinant :: Floating t => Mat t -> t
determinant m@(Mat rs)
    | matSize m == 1 = vecHead (head rs)
    | otherwise    = sum $ zipWith addition [0..] $ rows m
  where
    addition i (Vec (x:_)) =  x * cofactor i 0 m

cofactor :: Floating t => Int -> Int -> Mat t -> t
cofactor i j m = ((-1.0) ** fromIntegral (i + j)) * determinant (delmatrix i j m)

cofactorM :: Floating t => Mat t -> Mat t
cofactorM m = Mat $ map (Vec . map (\(i,j) -> cofactor j i m)) $ coords m

inverse :: Floating t => Mat t -> Mat t
inverse m = fmap (* recip det) $ cofactorM m
  where
    det = determinant m

v1 = Vec [1,2,3]
v2 = Vec [4,5,6]

m = Mat [v2,v1,v2]
n = Mat [v1,v1,v2]

p= matMult m n
