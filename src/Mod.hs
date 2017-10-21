{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Mod
    (
        --affineShift
        --, affineDeshift
        --, linSolve
        --, mord
) where


import GHC.TypeLits
import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Map as M hiding((!), toList, fromList, size)
import Data.List as L hiding (concat, transpose)
import Text.Printf
import Data.Tuple (swap)
import Data.String.Utils
-- import Data.Vector as V hiding (fromList)
import qualified Data.Vector.Mutable     as MV
import Utils
-- import Modular
import Data.Reflection hiding (Z)
import Data.Proxy
import Foreign.Storable
import Numeric.LinearAlgebra



{-
data Mat a = Mat { columns:: Vector (Vector a)} deriving (Show)


dims :: Mat a -> (Int, Int)
dims m = (ncols, V.length $ V.head cols)
  where
    cols = columns m
    ncols = V.length cols

rows :: Mat a -> Vector (Vector a)
rows m = transpose $ columns m


transpose :: Vector ( Vector a) -> Vector (Vector a)
transpose m =
  if V.null m then
    V.empty
    else if V.null $ V.head m then
          transpose $ V.tail m
          else
            V.cons (V.cons x  (V.map V.head xss)) $ transpose $ V.cons xs $ V.map V.tail xss
  where
    x = V.head $ V.head m
    xs = V.tail $ V.head m
    xss = V.tail m


(*!):: Mat a -> (Int, Int) -> a
(Mat cs) *! (i, j) = (cs!i)!j


(!*) :: (Integral i, KnownNat n) => Vector (Mod i n) -> Vector (Mod i n) -> Mod i n
v !* w = V.sum $ V.zipWith (\x y-> x*y) v  w


(!**) :: (Integral i, KnownNat n) => Mat (Mod i n) -> Mat (Mod i n) -> Maybe (Mat (Mod i n))
n !** m = if (j==k) then Just $ Mat $ V.map (\cm -> V.map (\rn -> rn !* cm) $ rows n) $ columns m else Nothing
  where
    (i, j) = dims m
    (k, l) = dims n

matFromList :: Int -> Int -> [a] -> Mat a
matFromList i j xs = Mat $ fromList $ L.map fromList cols
  where
    cols = chunksOf i xs

matToList :: Mat a -> [a]
matToList m = L.concatMap toList $ columns m

-- This just works for 2x2 matrices!!!
inverse:: (Integral i, KnownNat n) => Mat (Mod i n) -> Mat (Mod i n)
inverse m = matFromList (2::Int) (2::Int) ys
    where
        xs = matToList m -- This give [a, b, c, d]
        det = m*!(0,0) * m*!(1,1) - m*!(0,1) * m*!(1,0)
        invDet = inv det
        ys = L.map (\x-> x * invDet) [m*!(1,1), -m*!(0,1), -m*!(1,0), m*!(0,0)]
-}
