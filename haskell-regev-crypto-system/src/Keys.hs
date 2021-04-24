module Keys
  ( genPrivateKey,
    genPublicKey,
    PrivateKey,
    PublicKey,
    getAfromKey,
    getBfromKey,
    privateKeySize,
    publicKeySize
  )
where

import Data.List.Split (chunksOf)
import Data.Maybe
import Phi
import System.Random
import Zp

type PrivateKey = [Int]

data PublicKey = PublicKey
  { aVectors :: [[Int]],
    bVectors :: [Int]
  }
  deriving (Show)

getAfromKey :: PublicKey -> [[Int]]
getAfromKey PublicKey {aVectors = a} = a

getBfromKey :: PublicKey -> [Int]
getBfromKey PublicKey {bVectors = b} = b

privateKeySize :: PrivateKey -> Int
privateKeySize = length

publicKeySize :: PublicKey -> Int
publicKeySize publicKey =
  let bSize = length $ getBfromKey publicKey
      aSize = (*) bSize $ length . getAfromKey $ publicKey
   in aSize + bSize

genPrivateKey :: Int -> Int -> IO PrivateKey
genPrivateKey p dimension = do
  gen <- newStdGen
  let key = sampleFromZpn p dimension gen
  return key

_genPublicKey ::
  (RandomGen g) =>
  Int ->
  Int ->
  Int ->
  PrivateKey ->
  g ->
  g ->
  PublicKey
_genPublicKey p dimension m privateKey gen1 gen2 =
  let aVectors = chunksOf dimension $ sampleFromZpn p (dimension * m) gen1
      errors = phiAlphaZpPDF p dimension m gen2
      aDotS = map (internalProdZpn p privateKey) aVectors
      bVectors = addZpn p aDotS errors
   in PublicKey {aVectors = aVectors, bVectors = bVectors}

genPublicKey :: Int -> Int -> Int -> PrivateKey -> IO PublicKey
genPublicKey p dimension m privateKey = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  let key = _genPublicKey p dimension m privateKey gen1 gen2
  return key
