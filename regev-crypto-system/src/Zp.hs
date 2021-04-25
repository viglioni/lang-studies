module Zp
  ( sampleFromZpn,
    internalProdZpn,
    addZpn,
    subsetOfm,
    sumZpn,
    sumZp,
    (+!),
    (-!),
    (*!),
  )
where

import Data.List (transpose)
import qualified Data.Maybe
import System.Random

sampleFromZpn :: (RandomGen g) => Int -> Int -> g -> [Int]
sampleFromZpn p dimension gen = take dimension $ randomRs (0, p) gen

justTrue :: Bool -> Int -> Maybe Int
justTrue bool n = if bool then Just n else Nothing

subsetOfm :: (RandomGen g) => Int -> g -> [Int]
subsetOfm m gen =
  let booleans = randoms gen :: [Bool]
      elements = zipWith justTrue booleans [0 .. (m -1)]
   in Data.Maybe.catMaybes elements

z_ :: Int -> Int -> Int
z_ = flip mod

(+!) :: Int -> Int -> Int -> Int
(+!) p x y = zp $ zp x + zp y
  where
    zp = z_ p

(-!) :: Int -> Int -> Int -> Int
(-!) p x y = zp $ zp x - zp y
  where
    zp = z_ p

(*!) :: Int -> Int -> Int -> Int
(*!) p x y = zp $ zp x * zp y
  where
    zp = z_ p

sumZp :: Int -> [Int] -> Int
sumZp p = foldr (p +!) 0

internalProdZpn :: Int -> [Int] -> [Int] -> Int
internalProdZpn p v1 v2 =
  let sum = sumZp p
      (*) = (*!) p
      list = zipWith (*) v1 v2
   in sum list

addZpn :: Int -> [Int] -> [Int] -> [Int]
addZpn p v1 v2 =
  let (+) = (+!) p
   in zipWith (+) v1 v2

sumZpn :: Int -> [[Int]] -> [Int]
sumZpn p matrix =
  let sum = sumZp p
   in map sum $ transpose matrix
