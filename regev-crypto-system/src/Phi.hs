module Phi (phiPDF, phiAlphaPDF, phiAlphaZpPDF) where

import Data.Random.Normal
import System.Random

square :: (Num n) => n -> n
square x = x * x

sqrtInt :: (Floating a) => Int -> a
sqrtInt = sqrt . fromIntegral

logInt :: (Floating a) => Int -> a
logInt = log . fromIntegral

inverse :: (Floating a) => a -> a
inverse x = 1 / x

mul :: (Floating a) => a -> a -> a
mul a b = a * b

alphaN :: (Floating a) => Int -> a
alphaN n = inverse . mul (sqrtInt n) . square . logInt $ n

phiPDF :: (RandomGen g, Floating b, Random b) => b -> g -> [b]
phiPDF beta = normals' (0, stdDev)
  where
    stdDev = beta / sqrt (2 * pi)

phiAlphaPDF :: (RandomGen g) => Int -> g -> [Double]
phiAlphaPDF = phiPDF . alphaN

discretize :: Int -> Double -> Int
discretize p sample = flip mod p $ round $ sample * fromIntegral p

phiAlphaZpPDF :: (RandomGen g) => Int -> Int -> Int -> g -> [Int]
phiAlphaZpPDF p dimension quantity gen = zpSamples
  where
    samples = take quantity $ phiAlphaPDF dimension gen
    zpSamples = map (discretize p) samples

