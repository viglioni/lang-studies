module Encrypt
  ( EncryptedBit,
    getSumA,
    getSumB,
    encryptBit,
    getSubSet,
    encryptedSize
  )
where

import Keys
import System.Random
import Zp

data EncryptedBit = EncryptedBit
  { sumA :: [Int],
    sumB :: Int
  }
  deriving (Show)

getSumA :: EncryptedBit -> [Int]
getSumA EncryptedBit {sumA = s} = s

getSumB :: EncryptedBit -> Int
getSumB EncryptedBit {sumB = s} = s

encryptedSize :: EncryptedBit -> Int
encryptedSize encryptedBit =
  let aSize = length $ getSumA encryptedBit
  in aSize + 1

getSubSet :: [Int] -> [a] -> [a]
getSubSet indexes list = map (list !!) indexes

_encryptBit :: (RandomGen g) => Int -> Int ->  PublicKey -> Int -> g -> EncryptedBit
_encryptBit p m publicKey bit gen =
  let setS = subsetOfm m gen
      sumA = sumZpn p $ getSubSet setS $ getAfromKey publicKey
      sumB = sumZp p $ getSubSet setS $ getBfromKey publicKey
      additionTerm = if bit == 0 then 0 else floor $ fromIntegral p / 2
      sumBWithTerm = (+!) p additionTerm sumB
   in EncryptedBit {sumA = sumA, sumB = sumBWithTerm}


encryptBit :: Int -> Int -> PublicKey  -> Int -> IO EncryptedBit
encryptBit p m publicKey bit = do
  gen <- newStdGen
  let enc = _encryptBit p m publicKey bit gen
  return enc
