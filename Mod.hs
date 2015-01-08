{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mod
    (
        modinv
        , add 
        , inverse
        , toList

) where

import System.IO
import Control.Monad
import Data.Array as A hiding((!))
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Map as M hiding((!), toList, fromList)
import Data.List as L
import Text.Printf
import Data.Tuple (swap)
import Data.String.Utils
import Number.ResidueClass
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV

import NumericPrelude
import MathObj.Matrix as MM hiding (zipWith)
import Algebra.Vector hiding (Eq(..))
import Algebra.Ring
import Algebra.IntegralDomain
import Algebra.PrincipalIdealDomain

-- This module does the modular arithmetic - usually mod 26
-- for cipher algorithms.

-- It also does the matrix inverse when the numbers are mod 26



-- Works out an inverse of x in mod s
modinv::Int->Int->[Int]
modinv s x = L.filter (\z-> ((mod (z*x) s)==1)) [0..(s-1)]

--modinv :: (Algebra.Ring.C a, Algebra.IntegralDomain.C a , Algebra.PrincipalIdealDomain.C a ) => a -> a -> a
--modinv = Number.ResidueClass.recip

(!)::T a -> (Dimension, Dimension) -> a
m ! (x,y) = MM.index m x y

data U a = U {
      rowOffset :: Dimension
    , colOffset :: Dimension
    , t :: T a
}


toList :: T Int -> [Int]
toList = concat . columns

-- This just works for 2x2 matrices!!!
inverse:: T Int -> Maybe (T Int)
inverse m = fmap (\y-> fromList 2 2 y) ys
    where
        xs = toList m -- This give [a, b, c, d]
        det = m!(0,0) * m!(1,1) - m!(0,1) * m!(1,0)
        invDets = modinv 26 det
        invDet = if length invDets>0 then Just (head invDets) else Nothing
        ys = fmap (\d-> L.map (\x-> mod (x*d) 26) [m!(1,1), -m!(0,1), -m!(1,0), m!(0,0)]) invDet


{-
luDecomp :: () => U a -> Maybe (U a,U a,U a,a)
luDecomp a = recLUDecomp a i i 1 1 n
 where
  i = Algebra.Ring.one $ numRows a
  n = min (numRows a) (numColumns a)

recLUDecomp ::  ()
            =>  U a -- ^ U
            ->  U a -- ^ L
            ->  U a -- ^ P
            ->  a        -- ^ d
            ->  Int      -- ^ Current row
            ->  Int      -- ^ Total rows
            -> Maybe (U a,U a,U a,a)
recLUDecomp u l p d k n =
    if k > n then Just (u,l,p,d)
    else if ukk == 0 then Nothing
                     else recLUDecomp u'' l'' p' d' (k+1) n
 where
  -- Pivot strategy: maximum value in absolute value below the current row.
  i  = maximumBy (\x y -> compare (abs $ u ! (x,k)) (abs $ u ! (y,k))) [ k .. n ]
  -- Switching to place pivot in current row.
  u' = switchRows k i u
  l' = let lw = numColumns l
           en = encode lw
           lro = rowOffset l
           lco = colOffset l
       in  if i == k
              then l
                else U 
--              else M (numRows l) (numColumns l) lro lco lw $
--                     V.modify (\mv -> forM_ [1 .. k-1] $ 
--                                 \j -> MV.swap mv (en (i+lro,j+lco))
--                                                  (en (k+lro,j+lco))
--                                ) $ mvect l
  p' = switchRows k i p
  -- Permutation determinant
  d' = if i == k then d else negate d
  -- Cancel elements below the pivot.
  (u'',l'') = go u' l' (k+1)
  ukk = u' ! (k,k)
  go u_ l_ j =
    if j > numRows u_
    then (u_,l_)
    else let x = (u_ ! (j,k)) / ukk
         in  go (combineRows j (-x) k u_) (setElem x (j,k) l_) (j+1)



switchRows :: Int -- ^ Row 1.
           -> Int -- ^ Row 2.
           -> T a -- ^ Original matrix.
           -> T a -- ^ Matrix with rows 1 and 2 switched.
switchRows r1 r2 (M n m ro co w vs) = M n m ro co w $ V.modify (\mv -> do
  numLoop 1 m $ \j ->
    MV.swap mv (encode w (r1+ro,j+co)) (encode w (r2+ro,j+co))) vs

-- | Switch two coumns of a matrix.
--   Example:
--
-- >                ( 1 2 3 )   ( 2 1 3 )
-- >                ( 4 5 6 )   ( 5 4 6 )
-- > switchCols 1 2 ( 7 8 9 ) = ( 8 7 9 )
switchCols :: Int -- ^ Col 1.
           -> Int -- ^ Col 2.
           -> T a -- ^ Original matrix.
           -> T a -- ^ Matrix with cols 1 and 2 switched.
switchCols c1 c2 (M n m ro co w vs) = M n m ro co w $ V.modify (\mv -> do
  numLoop 1 n $ \j ->
    MV.swap mv (encode m (j+ro,c1+co)) (encode m (j+ro,c2+co))) vs

encode :: Int -> (Int,Int) -> Int
{-# INLINE encode #-}
encode m (i,j) = (i-1)*m + j - 1

decode :: Int -> Int -> (Int,Int)
{-# INLINE decode #-}
decode m k = (q+1,r+1)
 where
  (q,r) = quotRem k m

mvect::T a -> Vector a
mvect = V.fromList . L.concat . MM.rows

--setElem x p (U ro co (Cons $ Array (n, m) v)) = U ro co $ V.modify (msetElem x w ro co p) v
-- setElem x p (M n m ro co w v) = M n m ro co w $ V.modify (msetElem x w ro co p) v



-}
