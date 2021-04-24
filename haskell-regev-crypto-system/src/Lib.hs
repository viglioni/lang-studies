module Lib
  (calcM,
  )
where

import System.Random

_calcM :: Int -> Int -> StdGen -> Int
_calcM p n gen = round $  incEpsilon * logp * incN
  where
    epsilon = fst $ random gen :: Float
    incEpsilon = 1 + epsilon
    logp = log . fromIntegral $ p
    incN = fromIntegral (n+1)

calcM :: Int -> Int -> IO Int
calcM p n = do
  gen <- newStdGen
  let m = _calcM p n gen
  return m
