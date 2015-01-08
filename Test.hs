{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test
    (

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
--import Number.ResidueClass
--import qualified Data.Vector             as V
--import qualified Data.Vector.Mutable     as MV

--import NumericPrelude
--import MathObj.Matrix as MM hiding (zipWith)
--import Algebra.Vector hiding (Eq(..))
--import Algebra.Ring
--import Algebra.IntegralDomain
--import Algebra.PrincipalIdealDomain

