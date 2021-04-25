module Decrypt (decryptBit) where

import Encrypt (EncryptedBit, getSumA, getSumB)
import Keys (PrivateKey)
import Zp (internalProdZpn, (-!))

isCloserToZero :: Int -> Int -> Bool
isCloserToZero p num =
  let p4 = floor $ fromIntegral p / 4 in (num <= p4) || (num > (p - p4))

decryptBit :: Int -> PrivateKey -> EncryptedBit -> Int
decryptBit p privateKey encryptedBit =
  let a = getSumA encryptedBit
      b = getSumB encryptedBit
      (-) = (-!) p
      result = b - internalProdZpn p a privateKey
   in if isCloserToZero p result then 0 else 1

